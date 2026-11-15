{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Backend recipes, sessions, outcomes, and evidence without a provider.
module Axioma.Backend
  ( backendTopic,
  )
where

import Axioma.Common (assert, tempRoot)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Acp (AcpConfig (..), defaultAcpConfig)
import Free.Agent.Backend
import Free.Agent.Cli (Cli (..), StderrPolicy (..), parseSessionId)
import Free.Agent.Gateway (GatewayConfig (..), defaultGatewayConfig)
import Free.Agent.Host (BareConfig (..), defaultBareConfig, hermesCli)
import System.Directory (doesFileExist, removePathForcibly)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Process (createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

backendTopic :: IO ()
backendTopic = do
  putStrLn "Backend: independent Session legs and typed evidence"
  recipeWitnesses
  processWitnesses
  commandWitness
  httpWitnesses
  acpWitness

recipeWitnesses :: IO ()
recipeWitnesses = do
  let hermes = hermesCli (Just "model") (Just "provider") (Just "high") True "seat.sid"
      hermesArgv = cliArgv hermes "prompt" (Just "old-sid")
  assert "B20a the Hermes recipe keeps its backend-specific flags and resume" $
    hermesArgv
      == [ "-z",
           "prompt",
           "-m",
           "model",
           "--provider",
           "provider",
           "--reasoning",
           "high",
           "--usage-file",
           "seat.usage.json",
           "--yolo",
           "--resume",
           "old-sid"
         ]
  let dsh = dshBackend (Just "CARD") "base.patch" (Just "model.patch") "dsh.sid"
  caps <- openCapabilities dsh
  assert "B20b dsh is a cancellable one-shot with file sessions" $
    caps == BackendCapabilities DshBackend FileSessions OneShot True
  hermesCaps <- openCapabilities $ hermesBackend Nothing Nothing Nothing False "hermes.sid"
  kimiCaps <- openCapabilities $ kimiBackend Nothing Nothing "kimi.sid"
  assert "B20c named process constructors expose their actual common capabilities" $
    hermesCaps == BackendCapabilities HermesBackend FileSessions OneShot True
      && kimiCaps == BackendCapabilities KimiBackend FileSessions OneShot True
  directCaps <-
    openCapabilities $
      directHttpBackend (defaultBareConfig {baseUrl = "http://127.0.0.1:1"}) "system"
  assert "B20d direct HTTP is honestly one-shot, stateless, and not cancellable" $
    directCaps == BackendCapabilities DirectHttpBackend NoSessions OneShot False

openCapabilities :: Backend -> IO BackendCapabilities
openCapabilities backend = bracket (openBackend backend) closeSession (pure . sessionCapabilities)

processWitnesses :: IO ()
processWitnesses = do
  root <- tempRoot "free-agent-backend-process"
  let script = root </> "fake.sh"
      sessionFile = root </> "fake.sid"
      calls = root </> "calls"
      started = root </> "started"
  TIO.writeFile script $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "printf '%s\\n' \"$*\" >> '" <> calls <> "'",
          "case \"$*\" in",
          "  *cancel*)",
          "    touch '" <> started <> "'",
          "    trap 'exit 0' TERM",
          "    while :; do sleep 1; done ;;",
          "  *fail*)",
          "    printf '@runner: partial output\\n'",
          "    printf 'failure detail\\n' >&2",
          "    exit 7 ;;",
          "  *ceiling*)",
          "    printf 'spent budget\\n\\nturn_partial:true\\n'",
          "    exit 2 ;;",
          "  *exhausted*)",
          "    printf 'max-tokens\\n'",
          "    exit 1 ;;",
          "  *interrupted*)",
          "    printf 'Operation interrupted: local witness\\n'",
          "    exit 130 ;;",
          "  *stale*)",
          "    printf 'No session found matching stale-sid\\n'",
          "    exit 2 ;;",
          "esac",
          "printf 'session_id: fake-sid\\n@runner: %s\\n' \"$*\""
        ]
  setFileMode script 0o755
  let cli =
        Cli
          { cliCommand = script,
            cliArgv = \prompt sid -> maybe [] (\s -> ["--resume", T.unpack s]) sid <> [T.unpack prompt],
            cliStdin = const "",
            cliSessionFile = sessionFile,
            cliSessionId = parseSessionId,
            cliStale = \_ out -> "No session found matching" `T.isInfixOf` out,
            cliExhausted = \_ out -> "max-tokens" `T.isInfixOf` out,
            cliScrub = T.strip . T.unlines . filter (not . ("session_id:" `T.isPrefixOf`)) . T.lines,
            cliStderr = StderrMark,
            cliStderrTee = Nothing,
            cliSessionReport = Nothing
          }
  session <- openBackend (processBackend cli)

  first <- runRequest session (BackendRequest "first" FreshSession)
  assert "B21a output and successful settlement are separate events" $
    case first of
      [Output (BackendOutput ReplyOutput "@runner: first"), Settled (Completed evidence)] ->
        evidenceExitCode evidence == Just 0
          && evidenceSessionId evidence == Just "fake-sid"
      _ -> False
  stored <- TIO.readFile sessionFile
  assert "B21b a completed fresh invocation persists its session id" $ T.strip stored == "fake-sid"

  _ <- runRequest session (defaultRequest "second")
  _ <- runRequest session (BackendRequest "third" FreshSession)
  beforeStale <- TIO.readFile calls
  stale <- runRequest session (BackendRequest "stale" (ResumeSession "stale-sid"))
  afterStale <- TIO.readFile calls
  assert "B21c automatic resume, explicit fresh, and explicit resume remain distinct" $
    "first" `T.isInfixOf` beforeStale
      && "--resume fake-sid second" `T.isInfixOf` beforeStale
      && not ("--resume fake-sid third" `T.isInfixOf` beforeStale)
      && "--resume stale-sid stale" `T.isInfixOf` afterStale
  assert "B21d stale is terminal for that attempt, with no hidden fresh replay" $
    T.length afterStale == T.length beforeStale + T.length "--resume stale-sid stale\n"
      && case stale of
        [Output (BackendOutput StdoutOutput _), Settled (Stale _ evidence)] ->
          evidenceExitCode evidence == Just 2
        _ -> False
  storedAfterStale <- TIO.readFile sessionFile
  assert "B21e a stale attempt does not rotate the healthy stored session" $
    T.strip storedAfterStale == "fake-sid"

  failed <- runRequest session (BackendRequest "fail" FreshSession)
  assert "B22a output before a started failure is retained beside the typed failure" $
    case failed of
      [ Output (BackendOutput StdoutOutput out),
        Output (BackendOutput StderrOutput err),
        Settled (Failed ProcessFailure _ evidence)
        ] ->
          "@runner: partial output" `T.isInfixOf` out
            && "failure detail" `T.isInfixOf` err
            && evidenceExitCode evidence == Just 7
      _ -> False
  ceilingResult <- runRequest session (BackendRequest "ceiling" FreshSession)
  exhausted <- runRequest session (BackendRequest "exhausted" FreshSession)
  interrupted <- runRequest session (BackendRequest "interrupted" FreshSession)
  assert "B22b ceiling, exhaustion, and interruption are not collapsed into failure" $
    hasOutcome isCeiling ceilingResult && hasOutcome isExhausted exhausted && hasOutcome isInterrupted interrupted

  accepted <- sendSession session (BackendRequest "cancel" FreshSession)
  assert "B23a the cancellation input is accepted" $ accepted == Right ()
  waitForFile started
  won <- cancelSession session
  cancelled <- drainEvents session
  assert "B23b cancellation terminates the real process and settles as Cancelled" $
    won && hasOutcome isCancelled cancelled
  cancelCalls <- TIO.readFile calls
  assert "B23c cancellation does not trigger a fresh-session replay" $
    length (filter ("cancel" `T.isInfixOf`) (T.lines cancelCalls)) == 1

  unavailable <- openBackend (processBackend cli {cliCommand = root </> "missing"})
  unavailableEvents <- runRequest unavailable (defaultRequest "unavailable")
  assert "B24a an executable that cannot start is environment-unavailable" $
    case unavailableEvents of
      [Settled (Unavailable _)] -> True
      _ -> False
  closeSession unavailable

  active <- sendSession session (defaultRequest "busy")
  second <- sendSession session (defaultRequest "concurrent")
  assert "B24b concurrent input is rejected explicitly rather than queued invisibly" $
    (active == Right () && second == Left SendActive) || active == Left SendActive
  _ <- cancelSession session
  closeSession session
  hasLog <- doesFileExist (root </> "log.jsonl")
  assert "B24c Backend output is not response-scraped into a bus publication" $ not hasLog
  removePathForcibly root

commandWitness :: IO ()
commandWitness = do
  session <- openBackend (commandBackend "/bin/echo" ["fixed"])
  events <- runRequest session (defaultRequest "whole input")
  unsupported <- sendSession session (BackendRequest "again" (ResumeSession "unsupported"))
  assert "B25a external commands receive fixed args followed by one whole input argument" $
    case events of
      [Output (BackendOutput ReplyOutput out), Settled (Completed _)] -> T.strip out == "fixed whole input"
      _ -> False
  assert "B25b a no-session command rejects explicit resume" $ unsupported == Left SendUnsupportedSession
  closeSession session

httpWitnesses :: IO ()
httpWitnesses = do
  root <- tempRoot "free-agent-backend-http"
  setEnv "FREE_AGENT_BACKEND_TEST_KEY" "local"
  withLocalServer root $ \base capture -> do
    let directConfig =
          defaultBareConfig
            { baseUrl = T.pack (base <> "/ok"),
              model = "local-model",
              key = "local-key"
            }
    direct <- openBackend (directHttpBackend directConfig "local system")
    directEvents <- runRequest direct (defaultRequest "local user")
    assert "B26a direct HTTP separates decoded reply content from completion" $
      case directEvents of
        [Output (BackendOutput ReplyOutput "http reply"), Settled (Completed _)] -> True
        _ -> False
    closeSession direct

    bad <- openBackend (directHttpBackend directConfig {baseUrl = T.pack (base <> "/bad")} "system")
    badEvents <- runRequest bad (defaultRequest "bad")
    assert "B26b non-2xx HTTP is a typed failure, not red response prose" $
      case badEvents of
        [Output (BackendOutput RawOutput _), Settled (Failed (HttpFailure 503) _ _)] -> True
        _ -> False
    closeSession bad

    invalid <- openBackend (directHttpBackend directConfig {baseUrl = T.pack (base <> "/invalid")} "system")
    invalidEvents <- runRequest invalid (defaultRequest "invalid")
    assert "B26c response decode failure is distinct from transport availability" $
      hasOutcome isDecodeFailure invalidEvents
    closeSession invalid

    let gatewayConfig =
          defaultGatewayConfig
            { gwBaseUrl = T.pack (base <> "/gateway"),
              gwKeyEnv = "FREE_AGENT_BACKEND_TEST_KEY",
              gwCwd = root
            }
    gateway <- openBackend (gatewayBackend gatewayConfig "gateway system")
    assert "B27a Gateway exposes incremental SSE events without claiming cancellation" $
      sessionCapabilities gateway == BackendCapabilities GatewayBackend ServerSessions IncrementalEvents False
    gatewayEvents <- runRequest gateway (defaultRequest "gateway user")
    assert "B27b deltas, completed content, and terminal completion stay separate" $
      case gatewayEvents of
        [ Output (BackendOutput DeltaOutput "hel"),
          Output (BackendOutput DeltaOutput "lo"),
          Output (BackendOutput ReplyOutput "hello"),
          Settled (Completed evidence)
          ] -> evidenceSessionId evidence == Nothing && evidenceStopReason evidence == Just "done"
        _ -> False
    unsupported <- sendSession gateway (BackendRequest "resume" (ResumeSession "other"))
    assert "B27c a newly opened server session does not claim resume" $ unsupported == Left SendUnsupportedSession

    gatewayFailure <- openBackend (gatewayBackend gatewayConfig "gateway system")
    failedEvents <- runRequest gatewayFailure (defaultRequest "fail")
    assert "B27d run.failed is a protocol outcome after any earlier delta" $
      case failedEvents of
        [Output (BackendOutput DeltaOutput "partial"), Settled (Failed ProtocolFailure _ _)] -> True
        _ -> False
    closeSession gatewayFailure
    closeSession gateway

    requests <- TIO.readFile capture
    assert "B28a local witnesses exercised the exact HTTP request recipes" $
      "\"model\":\"local-model\"" `T.isInfixOf` requests
        && "\"max_tokens\":4096" `T.isInfixOf` requests
        && "\"role\":\"system\",\"content\":\"local system\"" `T.isInfixOf` requests
        && "\"cwd\":\"" `T.isInfixOf` requests
  removePathForcibly root

  unavailable <-
    openBackend $
      directHttpBackend (defaultBareConfig {baseUrl = "http://127.0.0.1:1"}) "system"
  events <- runRequest unavailable (defaultRequest "unavailable")
  assert "B28b a local connection failure is environment-unavailable" $
    case events of
      [Settled (Unavailable _)] -> True
      _ -> False
  closeSession unavailable

acpWitness :: IO ()
acpWitness = do
  root <- tempRoot "free-agent-backend-acp"
  let script = root </> "fake-acp.sh"
  TIO.writeFile script $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "while IFS= read -r line; do",
          "  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
          "  case \"$line\" in",
          "    *'\"method\":\"initialize\"'*)",
          "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":1,\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"0\"}}}\\n' \"$id\" ;;",
          "    *'\"method\":\"session/new\"'*)",
          "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"sessionId\":\"local-acp\"}}\\n' \"$id\" ;;",
          "    *'\"method\":\"session/prompt\"'*)",
          "      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"acp reply\"}}}}'",
          "      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"agent_thought_chunk\",\"content\":{\"type\":\"text\",\"text\":\"acp thought\"}}}}'",
          "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\"",
          "      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"usage_update\",\"used\":7}}}' ;;",
          "  esac",
          "done"
        ]
  setFileMode script 0o755
  session <-
    openBackend $
      acpBackend
        defaultAcpConfig {acpCommand = script, acpArgs = [], acpWorkDir = root}
        2_000_000
        "acp system"
  assert "B29a ACP retains collected updates without claiming streaming or turn cancellation" $
    sessionCapabilities session == BackendCapabilities AcpBackend ServerSessions CollectedEvents False
  events <- runRequest session (defaultRequest "acp user")
  assert "B29b reply, thought, usage, stop reason, and raw evidence survive the wrapper" $
    case events of
      [ Output (BackendOutput ReplyOutput "acp reply"),
        Output (BackendOutput ThoughtOutput "acp thought"),
        Output (BackendOutput UsageOutput usage),
        Settled (Completed evidence)
        ] ->
          "\"used\":7" `T.isInfixOf` usage
            && evidenceSessionId evidence == Just "local-acp"
            && evidenceStopReason evidence == Just "end_turn"
            && "\"stopReason\":\"end_turn\"" `T.isInfixOf` evidenceRawOutput evidence
            && isJust (evidenceUsage evidence)
      _ -> False
  cancelled <- cancelSession session
  assert "B29c whole-session close is not misreported as per-turn cancellation" $ not cancelled
  closeSession session
  removePathForcibly root

runRequest :: Session -> BackendRequest -> IO [BackendEvent]
runRequest session request = do
  accepted <- sendSession session request
  assert "Backend request accepted" $ accepted == Right ()
  drainEvents session

drainEvents :: Session -> IO [BackendEvent]
drainEvents session = do
  events <- timeout 10_000_000 (go [])
  assert "Backend settles within the local witness deadline" (isJust events)
  pure (fromMaybe [] events)
  where
    go acc = do
      event <- nextEvent session
      case event of
        Output _ -> go (event : acc)
        Settled _ -> pure (reverse (event : acc))

hasOutcome :: (BackendOutcome -> Bool) -> [BackendEvent] -> Bool
hasOutcome predicate events = case events of
  [Settled outcome] -> predicate outcome
  [Output _, Settled outcome] -> predicate outcome
  _ -> False

isCeiling :: BackendOutcome -> Bool
isCeiling (Ceiling _ _) = True
isCeiling _ = False

isExhausted :: BackendOutcome -> Bool
isExhausted (Exhausted _ _) = True
isExhausted _ = False

isInterrupted :: BackendOutcome -> Bool
isInterrupted (Interrupted _ _) = True
isInterrupted _ = False

isCancelled :: BackendOutcome -> Bool
isCancelled (Cancelled _) = True
isCancelled _ = False

isDecodeFailure :: BackendOutcome -> Bool
isDecodeFailure (Failed DecodeFailure _ _) = True
isDecodeFailure _ = False

waitForFile :: FilePath -> IO ()
waitForFile path = go (100 :: Int)
  where
    go 0 = error ("timed out waiting for " <> path)
    go remaining = do
      exists <- doesFileExist path
      unless exists (threadDelay 50_000 >> go (remaining - 1))

withLocalServer :: FilePath -> (String -> FilePath -> IO a) -> IO a
withLocalServer root action = bracket start stop use
  where
    script = root </> "server.py"
    portFile = root </> "port"
    capture = root </> "requests.jsonl"
    start = do
      TIO.writeFile script serverSource
      (_, _, _, handle) <- createProcess (proc "python3" [script, portFile, capture])
      waitForFile portFile
      port <- T.strip <$> TIO.readFile portFile
      pure ("http://127.0.0.1:" <> T.unpack port, handle)
    stop (_, handle) = do
      terminateProcess handle
      _ <- waitForProcess handle
      pure ()
    use (base, _) = action base capture

serverSource :: Text
serverSource =
  T.pack $
    unlines
      [ "from http.server import HTTPServer, BaseHTTPRequestHandler",
        "import json, sys",
        "port_file, capture_file = sys.argv[1], sys.argv[2]",
        "class Handler(BaseHTTPRequestHandler):",
        "  def log_message(self, fmt, *args): pass",
        "  def do_POST(self):",
        "    body = self.rfile.read(int(self.headers.get('Content-Length', '0')))",
        "    with open(capture_file, 'ab') as f: f.write(body + b'\\n')",
        "    if self.path.endswith('/chat/stream'):",
        "      if b'fail' in body:",
        "        payload = b'event: assistant.delta\\ndata: {\"delta\":\"partial\"}\\n\\nevent: run.failed\\ndata: {\"error\":\"local failure\"}\\n\\nevent: done\\ndata: {\"seq\":2}\\n\\n'",
        "      else:",
        "        payload = b'event: assistant.delta\\ndata: {\"delta\":\"hel\"}\\n\\nevent: assistant.delta\\ndata: {\"delta\":\"lo\"}\\n\\nevent: assistant.completed\\ndata: {\"content\":\"hello\"}\\n\\nevent: done\\ndata: {\"seq\":1}\\n\\n'",
        "      return self.reply(200, payload, 'text/event-stream')",
        "    if self.path == '/gateway/api/sessions':",
        "      return self.reply(200, b'{\"session\":{\"id\":\"local-gateway\"}}')",
        "    if self.path == '/ok/chat/completions':",
        "      return self.reply(200, b'{\"choices\":[{\"message\":{\"content\":\"http reply\"}}]}')",
        "    if self.path == '/bad/chat/completions':",
        "      return self.reply(503, b'{\"error\":\"local unavailable\"}')",
        "    if self.path == '/invalid/chat/completions':",
        "      return self.reply(200, b'{')",
        "    return self.reply(404, b'{}')",
        "  def reply(self, status, payload, content_type='application/json'):",
        "    self.send_response(status)",
        "    self.send_header('Content-Type', content_type)",
        "    self.send_header('Content-Length', str(len(payload)))",
        "    self.end_headers()",
        "    self.wfile.write(payload)",
        "server = HTTPServer(('127.0.0.1', 0), Handler)",
        "with open(port_file, 'w') as f: f.write(str(server.server_port))",
        "server.serve_forever()"
      ]
