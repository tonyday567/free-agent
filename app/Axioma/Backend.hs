{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Backend recipes, sessions, outcomes, and evidence without a provider.
module Axioma.Backend
  ( backendTopic,
  )
where

import Axioma.Common (assert, tempRoot)
import Circuit.Parser.Json (Json (..), decodeJson)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Free.Agent.Acp (AcpConfig (..), defaultAcpConfig)
import Free.Agent.Backend
import Free.Agent.Cli (Cli (..), StderrPolicy (..), parseSessionId)
import Free.Agent.Gateway (GatewayConfig (..), defaultGatewayConfig)
import Free.Agent.Host (BareConfig (..), defaultBareConfig, hermesCli)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Environment (getEnv, setEnv)
import System.FilePath ((-<.>), (</>))
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
  dshAcpWitness

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

  immediate <- openBackend (commandBackend "/bin/true" [])
  accepted <- mapM (immediateResend immediate) [1 .. 100 :: Int]
  assert "B28c observing Settled always leaves the next send free to start" $
    all (== Right ()) accepted
  closeSession immediate

immediateResend :: Session -> Int -> IO (Either SendError ())
immediateResend session n = do
  accepted <- sendSession session (defaultRequest (T.pack (show n)))
  case accepted of
    Left err -> pure (Left err)
    Right () -> do
      _ <- drainEvents session
      pure (Right ())

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

    deltaOnly <- openBackend (gatewayBackend gatewayConfig "gateway system")
    deltaEvents <- runRequest deltaOnly (defaultRequest "delta-only")
    assert "B27e done assembles and emits a delta-only reply exactly once" $
      case deltaEvents of
        [ Output (BackendOutput DeltaOutput "hel"),
          Output (BackendOutput DeltaOutput "lo"),
          Output (BackendOutput ReplyOutput "hello"),
          Settled (Completed _)
          ] -> True
        _ -> False
    closeSession deltaOnly

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
  assert "B28b an HTTP request failure does not claim that no turn started" $
    case events of
      [Settled (Failed ProcessFailure _ _)] -> True
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
          "      case \"$line\" in",
          "        *max_tokens*)",
          "          printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"max_tokens\"}}\\n' \"$id\" ;;",
          "        *empty-reply*)",
          "          printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
          "        *reply-then-error*)",
          "          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"partial acp reply\"}}}}'",
          "          printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"error\":{\"code\":-32000,\"message\":\"local turn failure\"}}\\n' \"$id\" ;;",
          "        *timeout-turn*)",
          "          sleep 1",
          "          printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
          "        *)",
          "          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"acp reply\"}}}}'",
          "          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"agent_thought_chunk\",\"content\":{\"type\":\"text\",\"text\":\"acp thought\"}}}}'",
          "          printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\"",
          "          printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"local-acp\",\"sessionUpdate\":\"usage_update\",\"used\":7}}}' ;;",
          "      esac ;;",
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
            && evidenceContextUsage evidence == Just (BackendContextUsage 7 Nothing)
      _ -> False
  exhausted <- runRequest session (defaultRequest "max_tokens")
  empty <- runRequest session (defaultRequest "empty-reply")
  replyThenError <- runRequest session (defaultRequest "reply-then-error")
  assert "B29d ACP stop reasons and empty replies are typed outcomes, never silent completion" $
    hasOutcome isExhausted exhausted
      && case empty of
        [Settled (Failed EmptyReply _ _)] -> True
        _ -> False
  assert "B29e ACP retains a partial reply beside a correlated error response" $
    case replyThenError of
      [Output (BackendOutput ReplyOutput "partial acp reply"), Settled (Failed ProtocolFailure why _)] ->
        "local turn failure" `T.isInfixOf` why
      _ -> False
  cancelled <- cancelSession session
  assert "B29c whole-session close is not misreported as per-turn cancellation" $ not cancelled
  closeSession session

  timedOut <-
    openBackend $
      acpBackend
        defaultAcpConfig {acpCommand = script, acpArgs = [], acpWorkDir = root}
        500_000
        "acp system"
  timeoutEvents <- runRequest timedOut (defaultRequest "timeout-turn")
  nextSend <- sendSession timedOut (defaultRequest "must-not-read-late-response")
  assert "B29f ACP timeout closes the poisoned session instead of consuming a late response" $
    case timeoutEvents of
      [Settled (Failed TimeoutFailure _ _)] -> nextSend == Left SendClosed
      _ -> False
  closeSession timedOut
  removePathForcibly root

dshAcpWitness :: IO ()
dshAcpWitness = do
  root <- tempRoot "free-agent-backend-dsh-acp"
  let bin = root </> "bin"
      fake = bin </> "dsh"
      workspace = root </> "workspace"
      dshHome = root </> "dsh-home"
      sessionFile = root </> "session"
      transcriptFile = sessionFile -<.> "acp.jsonl"
      basePatch = root </> "base.yml"
      modelPatch = root </> "model.yml"
      wire = root </> "wire.jsonl"
      cancelMark = root </> "cancel-started"
      backend =
        dshAcpBackend
          (Just "CARD")
          basePatch
          (Just modelPatch)
          workspace
          dshHome
          sessionFile
  createDirectoryIfMissing True bin
  TIO.writeFile basePatch "[]"
  TIO.writeFile modelPatch "[]"
  TIO.writeFile fake (dshFakeSource root wire cancelMark)
  setFileMode fake 0o755
  bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
    setEnv "PATH" (bin <> ":" <> oldPath)
    session <- openBackend backend
    assert "B30a dsh ACP declares resumable, cancellable incremental events" $
      sessionCapabilities session == BackendCapabilities DshAcpBackend ServerSessions IncrementalEvents True
    first <- runRequest session (defaultRequest "first")
    second <- runRequest session (defaultRequest "second")
    assert "B30b fresh dsh prompts settle against the persisted session" $
      all (completedWithSession "dsh-session") [first, second]
    incrementalAccepted <- sendSession session (defaultRequest "incremental-turn")
    assert "B30h incremental dsh prompt is accepted" $ incrementalAccepted == Right ()
    thoughtEvent <- nextEvent session
    thoughtTranscript <- readRawTranscript transcriptFile
    assert "B30i dsh appends sent prompt and received thought before later frames or settlement" $
      thoughtEvent == Output (BackendOutput ThoughtOutput "dsh thought")
        && transcriptHas "send" "\"method\":\"session/prompt\"" thoughtTranscript
        && transcriptHas "recv" "dsh thought" thoughtTranscript
        && not (transcriptHas "recv" "dsh live reply" thoughtTranscript)
        && not (transcriptHas "recv" "test/maintenance" thoughtTranscript)
    TIO.writeFile (root </> "thought-observed") ""
    replyEvent <- nextEvent session
    replyTranscript <- readRawTranscript transcriptFile
    assert "B30j dsh transcript includes reply and non-text frames in both directions as they arrive" $
      replyEvent == Output (BackendOutput ReplyOutput "dsh live reply")
        && transcriptHas "recv" "test/maintenance" replyTranscript
        && transcriptHas "send" "\"id\":41" replyTranscript
        && transcriptHas "send" "method not found: test/maintenance" replyTranscript
        && transcriptHas "recv" "dsh live reply" replyTranscript
    TIO.writeFile (root </> "reply-observed") ""
    usageEvent <- nextEvent session
    settledEvent <- nextEvent session
    let incremental = [thoughtEvent, replyEvent, usageEvent, settledEvent]
    assert "B30k dsh emits thought, reply, usage, and one settlement without maintenance output" $
      case incremental of
        [ Output (BackendOutput ThoughtOutput "dsh thought"),
          Output (BackendOutput ReplyOutput "dsh live reply"),
          Output (BackendOutput UsageOutput usage),
          Settled (Completed evidence)
          ] ->
            "\"used\":9" `T.isInfixOf` usage
              && evidenceContextUsage evidence == Just (BackendContextUsage 9 (Just 30))
              && "test/maintenance" `T.isInfixOf` evidenceRawOutput evidence
        _ -> False
    busExists <- doesFileExist (root </> "log.jsonl")
    assert "B30l raw ACP transcript is independent of bus and text projections" $ not busExists
    closeSession session

    resumed <- openBackend backend
    afterRestart <- runRequest resumed (defaultRequest "after-restart")
    assert "B30c restart resumes the stored dsh session without echoing a new id" $
      completedWithSession "dsh-session" afterRestart

    accepted <- sendSession resumed (defaultRequest "cancel-turn")
    assert "B30d dsh cancellation prompt starts" $ accepted == Right ()
    waitForFile cancelMark
    won <- cancelSession resumed
    cancelled <- drainEvents resumed
    assert "B30e session/cancel settles the active prompt as Cancelled" $
      won && hasOutcome isCancelled cancelled
    closeSession resumed

    provenance <- TIO.readFile (root </> "provenance")
    wireText <- TIO.readFile wire
    stored <- TIO.readFile sessionFile
    canonicalWorkspace <- canonicalizePath workspace
    let dshHomeText = T.pack dshHome
        workspaceText = T.pack canonicalWorkspace
        baseText = T.pack basePatch
        modelText = T.pack modelPatch
    assert "B30f argv, absolute workspace, DSH_HOME, and session store are pinned independently" $
      let firstLine = T.takeWhile (/= '\n') provenance
       in (dshHomeText <> "|" <> workspaceText <> "|acp --patch " <> baseText <> " --patch " <> modelText) == firstLine
            && T.strip stored == "dsh-session"
    assert "B30g resume uses session/resume and the seat card is fresh-only" $
      T.count "session/new" wireText == 1
        && T.count "session/resume" wireText == 1
        && T.count "CARD" wireText == 1
  removePathForcibly root
  where
    completedWithSession sid events = case events of
      [Output _, Settled (Completed evidence)] -> evidenceSessionId evidence == Just sid
      _ -> False

readRawTranscript :: FilePath -> IO [(Text, Text)]
readRawTranscript path = do
  rows <- T.lines <$> TIO.readFile path
  let decoded = traverse decodeTranscriptRow rows
  assert "raw ACP transcript retains exactly the {ts,dir,raw} JSONL shape" (isJust decoded)
  pure (fromMaybe [] decoded)
  where
    decodeTranscriptRow line = case decodeJson (encodeUtf8 line) of
      Right (JObject fields)
        | length fields == 3,
          Just (JString ts) <- lookup "ts" fields,
          not (T.null ts),
          Just (JString dir) <- lookup "dir" fields,
          dir == "send" || dir == "recv",
          Just (JString raw) <- lookup "raw" fields ->
            Just (dir, raw)
      _ -> Nothing

transcriptHas :: Text -> Text -> [(Text, Text)] -> Bool
transcriptHas dir fragment = any (\(actualDir, raw) -> actualDir == dir && fragment `T.isInfixOf` raw)

dshFakeSource :: FilePath -> FilePath -> FilePath -> Text
dshFakeSource root wire cancelMark =
  T.pack $
    unlines
      [ "#!/usr/bin/env python3",
        "import json, os, pathlib, sys, time",
        "root = " <> show root,
        "wire = " <> show wire,
        "cancel_mark = " <> show cancelMark,
        "with open(root + '/provenance', 'a') as f:",
        "  f.write(os.environ.get('DSH_HOME', '') + '|' + os.getcwd() + '|' + ' '.join(sys.argv[1:]) + '\\n')",
        "def wait(path):",
        "  while not pathlib.Path(path).exists(): time.sleep(0.01)",
        "def log(value):",
        "  with open(wire, 'a') as f: f.write(json.dumps(value, separators=(',', ':')) + '\\n')",
        "def send(value):",
        "  log(value)",
        "  print(json.dumps(value, separators=(',', ':')), flush=True)",
        "def response(rid, result): send({'jsonrpc':'2.0','id':rid,'result':result})",
        "for line in sys.stdin:",
        "  value = json.loads(line)",
        "  log(value)",
        "  method = value.get('method')",
        "  rid = value.get('id')",
        "  if method == 'initialize':",
        "    response(rid, {'protocolVersion':1,'capabilities':{},'agentInfo':{'name':'fake-dsh','version':'0'}})",
        "  elif method == 'session/new':",
        "    response(rid, {'sessionId':'dsh-session','configOptions':[]})",
        "  elif method == 'session/resume':",
        "    response(rid, {'configOptions':[]})",
        "  elif method == 'session/prompt':",
        "    if 'incremental-turn' in json.dumps(value):",
        "      send({'jsonrpc':'2.0','method':'session/update','params':{'update':{'sessionId':'dsh-session','sessionUpdate':'agent_thought_chunk','content':{'type':'text','text':'dsh thought'}}}})",
        "      wait(root + '/thought-observed')",
        "      send({'jsonrpc':'2.0','id':41,'method':'test/maintenance','params':{}})",
        "      send({'jsonrpc':'2.0','method':'session/update','params':{'update':{'sessionId':'dsh-session','sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'dsh live reply'}}}})",
        "      wait(root + '/reply-observed')",
        "      response(rid, {'stopReason':'end_turn'})",
        "      send({'jsonrpc':'2.0','method':'session/update','params':{'update':{'sessionId':'dsh-session','sessionUpdate':'usage_update','used':9,'size':30}}})",
        "    elif 'cancel-turn' in json.dumps(value):",
        "      pathlib.Path(cancel_mark).touch()",
        "      for cancel_line in sys.stdin:",
        "        cancel = json.loads(cancel_line)",
        "        log(cancel)",
        "        if cancel.get('method') == 'session/cancel':",
        "          response(rid, {'stopReason':'cancelled'})",
        "          break",
        "    else:",
        "      send({'jsonrpc':'2.0','method':'session/update','params':{'update':{'sessionId':'dsh-session','sessionUpdate':'agent_message_chunk','content':{'type':'text','text':'dsh acp reply'}}}})",
        "      response(rid, {'stopReason':'end_turn'})"
      ]

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
        "      if b'delta-only' in body:",
        "        payload = b'event: assistant.delta\\ndata: {\"delta\":\"hel\"}\\n\\nevent: assistant.delta\\ndata: {\"delta\":\"lo\"}\\n\\nevent: done\\ndata: {\"seq\":1}\\n\\n'",
        "      elif b'fail' in body:",
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
