{-# LANGUAGE OverloadedStrings #-}

-- | ACP (Agent Client Protocol) JSON-RPC 2.0 NDJSON client over
-- 'Free.Agent.StdPorts' ends.
--
-- The wire: one JSON object per line on the child's stdout (no
-- Content-Length headers); stdin takes the same framing; stderr is
-- diagnostics, not protocol.  Requests are id-correlated; notifications
-- carry no id; kimi also issues reverse-RPC requests (with an id) that the
-- client must answer.
--
-- Ground truth is the leg-1 probe (@~\/lab\/acp-probe\/report.md@,
-- kimi acp v0.33.0):
--
--   * @session\/set_mode@ hangs — mode is switched with
--     @session\/set_config_option {configId: "mode", value: "auto"}@.
--   * @session\/request_permission@ responses are broken in kimi 0.33.0;
--     in @auto@ mode file I\/O arrives as @fs\/*@ reverse-RPCs instead.
--   * @fs\/*@ params carry @uri@ (with a @file:\/\/\/\/@ prefix), not the
--     spec's @path@ — both are accepted here.
--
-- Requests travel through the shared stdin 'commit' port of
-- 'Free.Agent.StdPorts'; replies are line-framed blocking reads from the
-- stdout queue.
module Free.Agent.Acp
  ( -- * Configuration
    AcpConfig (..),
    defaultAcpConfig,

    -- * Client
    AcpClient (..),
    openAcp,
    closeAcp,

    -- * Wire frames
    Frame (..),
    classifyFrame,
    jsonToInt,

    -- * Session updates
    Update (..),
    parseUpdate,
    frameUpdate,

    -- * Requests
    acpSendValue,
    acpReadFrame,
    acpRequest,
    acpInitialize,
    acpNewSession,
    acpResumeSession,
    acpSetConfigOption,
    acpCancel,
    acpPrompt,
    acpPromptWithResponse,
    acpPromptWithResponseOnSend,
    PromptResult (..),
    promptResult,

    -- * Prompt body
    SessionId,
    DshBody,
    DshAct (..),
    DshChannel (..),
    AcpPhase (..),
    AcpEmission (..),
    dshBody,

    -- * Seat
    acpHost,
  )
where

import Circuit.Category (K (..))
import Circuit.Body (Body (Body), Poles (..))
import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Circuit.Syntax (eval)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Vector qualified as V
import Free.Agent.Host (BodyMode (..), Host (..))
import Free.Agent.StdPorts
  ( ProcConfig (..),
    StdPorts (..),
    lineMarks,
    openStdPorts,
  )
import Free.Agent.StdPorts qualified as Ports
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory)
import System.Timeout (timeout)
import Prelude hiding (read)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | What to spawn and where the plumbing lives.
data AcpConfig = AcpConfig
  { -- | The executable (default @"kimi"@).
    acpCommand :: FilePath,
    -- | argv (default @["acp"]@).
    acpArgs :: [String],
    -- | The child's working directory.  The ACP session cwd is pinned
    -- separately via @session/new@.
    acpWorkDir :: FilePath,
    -- | Environment overrides for the child, merged over the inherited
    -- environment so PATH and credentials remain available.
    acpEnv :: [(String, String)],
    -- | Optional raw-frame transcript (NDJSON, one @{ts,dir,raw}@ per line,
    -- @dir@ ∈ send\/recv), mirroring the python probe's transcript.
    acpTranscript :: Maybe FilePath
  }
  deriving (Show, Eq)

-- | @kimi acp@ with the child working directory at @\/tmp\/free-agent-acp@.
defaultAcpConfig :: AcpConfig
defaultAcpConfig =
  AcpConfig
    { acpCommand = "kimi",
      acpArgs = ["acp"],
      acpWorkDir = "/tmp/free-agent-acp",
      acpEnv = [],
      acpTranscript = Nothing
    }

-- ---------------------------------------------------------------------------
-- Client
-- ---------------------------------------------------------------------------

-- | A live ACP child process plus client-side protocol state.
data AcpClient = AcpClient
  { acpPorts :: StdPorts Text Text Text,
    acpConfig :: AcpConfig,
    -- | Next JSON-RPC request id (monotonic from 1).
    acpNextId :: IORef Int
  }

-- | Spawn the ACP child and open the protocol state.  Pipes all the way
-- down: no FIFO, no log files — stdout frames arrive line by line on the
-- blocking emit end.
openAcp :: AcpConfig -> IO AcpClient
openAcp cfg = do
  let wd = acpWorkDir cfg
  createDirectoryIfMissing True wd
  forM_ (acpTranscript cfg) $ createDirectoryIfMissing True . takeDirectory
  inherited <- getEnvironment
  let overridden = map fst (acpEnv cfg)
      childEnv =
        if null (acpEnv cfg)
          then Nothing
          else Just (acpEnv cfg <> filter ((`notElem` overridden) . fst) inherited)
      replCfg =
        ProcConfig
          { procCommand = acpCommand cfg,
            procArgs = acpArgs cfg,
            procWorkingDir = wd,
            procEnv = childEnv,
            procMarks = lineMarks,
            procMergeStderr = False
          }
  pp <- runK (eval (openStdPorts encodeUtf8 decodeUtf8 replCfg)) ()
  nref <- newIORef 1
  pure
    AcpClient
      { acpPorts = pp,
        acpConfig = cfg,
        acpNextId = nref
      }

-- | Terminate the child (kills the pumps, closes the pipes).
closeAcp :: AcpClient -> IO ()
closeAcp c = stdClose (acpPorts c)

-- ---------------------------------------------------------------------------
-- Transcript
-- ---------------------------------------------------------------------------

logFrame :: AcpClient -> Text -> Text -> IO ()
logFrame c dir raw =
  forM_ (acpTranscript (acpConfig c)) $ \fp -> do
    now <- getCurrentTime
    let line = decodeUtf8 (encodeJson (JObject [("ts", JString (T.pack (show now))), ("dir", JString dir), ("raw", JString raw)]))
    TIO.appendFile fp (line <> "\n")

-- ---------------------------------------------------------------------------
-- JSON plumbing
-- ---------------------------------------------------------------------------

-- | Look up a key in a JSON object.
objLookup :: Text -> Json -> Maybe Json
objLookup k (JObject ps) = lookup k ps
objLookup _ _ = Nothing

-- | Extract a text value from a JSON object field.
textAt :: Text -> Json -> Maybe Text
textAt k j = case objLookup k j of
  Just (JString t) -> Just t
  _ -> Nothing

resultOf :: Json -> Maybe Json
resultOf = objLookup "result"

jsonToInt :: Json -> Maybe Int
jsonToInt (JNumber n) = toBoundedInteger n
jsonToInt _ = Nothing

-- | A classified incoming frame.
data Frame
  = -- | Response to one of our requests: id and the whole message
    -- (@result@ or @error@ lives inside).
    Response Int Json
  | -- | Reverse-RPC: kimi requests something from the client.
    AgentRequest Int Text Json
  | -- | Notification (no id): method and params.
    Notification Text Json
  deriving (Show)

-- | Classify a decoded message.  Note ids are 'Int's and may be 0 (kimi
-- sends @id: 0@ on @session\/request_permission@).
classifyFrame :: Json -> Maybe Frame
classifyFrame j = case (objLookup "id" j, objLookup "method" j) of
  (Just idV, Just (JString m))
    | Just i <- jsonToInt idV ->
        Just (AgentRequest i m (fromMaybe JNull (objLookup "params" j)))
  (Just idV, _)
    | Just i <- jsonToInt idV -> Just (Response i j)
  (_, Just (JString m)) ->
    Just (Notification m (fromMaybe JNull (objLookup "params" j)))
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Session updates
-- ---------------------------------------------------------------------------

-- | Parsed @session\/update@ notification payload (@params.update@).
data Update
  = AgentMessageChunk Text
  | AgentThoughtChunk Text
  | ToolCall Json
  | ToolCallUpdate Json
  | AvailableCommandsUpdate Json
  | SessionInfoUpdate Json
  | UsageUpdate Json
  | UnknownUpdate Text Json
  deriving (Eq, Show)

-- | Parse the @params@ of a @session\/update@ notification.  'Nothing' if
-- the @update@ object or its @sessionUpdate@ tag is missing.
parseUpdate :: Json -> Maybe Update
parseUpdate params = do
  upd <- objLookup "update" params
  tag <- textAt "sessionUpdate" upd
  pure $ case tag of
    "agent_message_chunk" -> AgentMessageChunk (contentText upd)
    "agent_thought_chunk" -> AgentThoughtChunk (contentText upd)
    "tool_call" -> ToolCall upd
    "tool_call_update" -> ToolCallUpdate upd
    "available_commands_update" -> AvailableCommandsUpdate upd
    "session_info_update" -> SessionInfoUpdate upd
    "usage_update" -> UsageUpdate upd
    other -> UnknownUpdate other upd
  where
    contentText upd =
      fromMaybe "" (objLookup "content" upd >>= textAt "text")

-- | Project one raw frame into a session update. Other frame classes and
-- non-update notifications remain protocol evidence rather than model output.
frameUpdate :: Json -> Maybe Update
frameUpdate raw = case classifyFrame raw of
  Just (Notification "session/update" params) -> parseUpdate params
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Send / receive
-- ---------------------------------------------------------------------------

-- | Encode and commit one JSON-RPC message line, logging the raw frame.
acpSendValue :: AcpClient -> Json -> IO ()
acpSendValue c v = do
  let line = decodeUtf8 (encodeJson v)
  logFrame c "send" line
  runK (commit (stdIn (acpPorts c))) line

-- | Read the next stdout line, blocking until a complete line frame
-- arrives, logging raw frames.  The child's death is an 'IOException' —
-- the stream ended before the next payload.
acpReadLine :: AcpClient -> IO Text
acpReadLine c = do
  f <- runK (emit (stdOut (acpPorts c))) ()
  case f of
    Ports.Payload t -> do
      logFrame c "recv" t
      pure t
    Ports.Exited code -> ioError (userError ("acp: process exited " ++ show code))

-- | Read the next decodable JSON message, blocking on the stdout queue
-- until one arrives or the timeout (microseconds) expires.  Lines that fail
-- to parse are skipped (they stay in the transcript).
acpReadFrame :: AcpClient -> Int -> IO (Maybe Json)
acpReadFrame c micros = timeout micros go
  where
    go = do
      t <- acpReadLine c
      if T.null t
        then go
        else case decodeJson (encodeUtf8 t) of
          Right v -> pure v
          Left _ -> go

-- ---------------------------------------------------------------------------
-- Reverse-RPC
-- ---------------------------------------------------------------------------

-- | kimi sends @uri@ with a @file:\/\/\/\/@ prefix where the ACP schema says
-- @path@; accept both.
uriPath :: Json -> Maybe FilePath
uriPath params = case textAt "uri" params of
  Just u -> Just (T.unpack (fromMaybe u (T.stripPrefix "file://" u)))
  Nothing -> T.unpack <$> textAt "path" params

sendResult :: AcpClient -> Int -> Json -> IO ()
sendResult c rid r =
  acpSendValue c (JObject [("jsonrpc", JString t2), ("id", JNumber (fromIntegral rid)), ("result", r)])
  where
    t2 = "2.0" :: Text

sendError :: AcpClient -> Int -> Int -> Text -> IO ()
sendError c rid code msg =
  acpSendValue
    c
    ( JObject
        [ ("jsonrpc", JString t2),
          ("id", JNumber (fromIntegral rid)),
          ("error", JObject [("code", JNumber (fromIntegral code)), ("message", JString msg)])
        ]
    )
  where
    t2 = "2.0" :: Text

-- | Answer a reverse-RPC from the agent.
--
-- @fs\/*@ are the auto-mode file channel (answered for real);
-- @session\/request_permission@ gets a best-effort schema-shaped approval
-- (kimi 0.33.0 mishandles every known response shape — switch to @auto@
-- mode instead); anything else gets @-32601@.
acpAnswer :: AcpClient -> Int -> Text -> Json -> IO ()
acpAnswer c rid method params = case method of
  "fs/read_text_file" -> case uriPath params of
    Nothing -> sendError c rid (-32602) "fs/read_text_file: missing uri/path"
    Just p -> do
      r <- try @SomeException (TIO.readFile p)
      case r of
        Right content -> sendResult c rid (JObject [("content", JString content)])
        Left e -> sendError c rid (-32000) (T.pack (show e))
  "fs/write_text_file" -> case uriPath params of
    Nothing -> sendError c rid (-32602) "fs/write_text_file: missing uri/path"
    Just p -> do
      let content = fromMaybe "" (textAt "content" params)
      r <-
        try @SomeException
          (createDirectoryIfMissing True (takeDirectory p) >> TIO.writeFile p content)
      case r of
        Right () -> sendResult c rid (JObject [])
        Left e -> sendError c rid (-32000) (T.pack (show e))
  "session/request_permission" ->
    sendResult
      c
      rid
      ( JObject
          [ ( "outcome",
              JObject
                [("outcome", JString ("selected" :: Text)), ("optionId", JString firstOption)]
            )
          ]
      )
  _ -> sendError c rid (-32601) ("method not found: " <> method)
  where
    firstOption = case objLookup "options" params of
      Just (JArray os) ->
        fromMaybe "approve_once" (listToMaybe (mapMaybe (textAt "optionId") (foldr (:) [] os)))
      _ -> "approve_once"

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

-- | Send a request and block until its response arrives or the timeout
-- (microseconds) expires. Interleaved reverse-RPCs are answered (see
-- 'acpAnswer'); every frame seen (including the response) is returned in
-- arrival order alongside the response.
acpRequest :: AcpClient -> Int -> Text -> Json -> IO (Maybe Json, [Json])
acpRequest = acpRequestOnSend (pure ())

-- | Allocate and send one correlated request. Reading its response is a
-- separate leg so prompt runners can choose collection or incrementality.
acpSendRequest :: AcpClient -> Text -> Json -> IO Int
acpSendRequest c method params = do
  rid <- atomicModifyIORef' (acpNextId c) (\i -> (i + 1, i))
  acpSendValue
    c
    ( JObject
        [ ("jsonrpc", JString t2),
          ("id", JNumber (fromIntegral rid)),
          ("method", JString method),
          ("params", params)
        ]
    )
  pure rid
  where
    t2 = "2.0" :: Text

-- | Read one frame and service reverse-RPC before returning it. The raw
-- message remains available even when it has no classified protocol shape.
acpReadClassified :: AcpClient -> Int -> IO (Maybe (Json, Maybe Frame))
acpReadClassified c micros = do
  mv <- acpReadFrame c micros
  case mv of
    Nothing -> pure Nothing
    Just value -> do
      case classifyFrame value of
        Just (AgentRequest i m p) -> acpAnswer c i m p
        _ -> pure ()
      pure (Just (value, classifyFrame value))

-- | Send one request, run @onSend@ after its correlated id is on the wire,
-- then read until that response arrives or the timeout expires. The hook is
-- where a caller installs cancellation without a pre-prompt race.
acpRequestOnSend :: IO () -> AcpClient -> Int -> Text -> Json -> IO (Maybe Json, [Json])
acpRequestOnSend onSend c micros method params = do
  rid <- acpSendRequest c method params
  onSend
  start <- getCurrentTime
  loop rid [] start
  where
    loop rid acc start = do
      left <- budgetLeft micros start
      if left <= 0
        then pure (Nothing, reverse acc)
        else do
          mv <- acpReadClassified c left
          case mv of
            Nothing -> pure (Nothing, reverse acc)
            Just (v, frame) -> case frame of
              Just (Response i _) | i == rid -> pure (Just v, reverse (v : acc))
              _ -> loop rid (v : acc) start

-- | @initialize@ handshake: protocolVersion 1, fs client capabilities on,
-- terminal off.
acpInitialize :: AcpClient -> Int -> IO (Maybe Json, [Json])
acpInitialize c micros =
  acpRequest
    c
    micros
    "initialize"
    ( JObject
        [ ("protocolVersion", JNumber 1),
          ( "clientInfo",
            JObject
              [("name", JString ("free-agent-acp" :: Text)), ("version", JString ("0.1.0" :: Text))]
          ),
          ( "clientCapabilities",
            JObject
              [("fs", JObject [("readTextFile", JBool True), ("writeTextFile", JBool True)]), ("terminal", JBool False)]
          )
        ]
    )

-- | @session/new@ with the session cwd pinned.  Returns the sessionId on
-- success.
acpNewSession :: AcpClient -> Int -> FilePath -> IO (Maybe SessionId, [Json])
acpNewSession c micros cwd = do
  (mresp, msgs) <-
    acpRequest
      c
      micros
      "session/new"
      (JObject [("cwd", JString (T.pack cwd)), ("mcpServers", JArray V.empty)])
  pure (mresp >>= resultOf >>= textAt "sessionId", msgs)

-- | @session/resume@ for a persisted session at the same absolute cwd.
-- dsh returns configOptions rather than echoing the session id, so the raw
-- response is retained and the caller keeps the requested id as evidence.
acpResumeSession :: AcpClient -> Int -> SessionId -> FilePath -> IO (Maybe Json, [Json])
acpResumeSession c micros sid cwd =
  acpRequest
    c
    micros
    "session/resume"
    (JObject [("sessionId", JString sid), ("cwd", JString (T.pack cwd))])

-- | @session/set_config_option@ — returns the full configOptions array.
acpSetConfigOption ::
  AcpClient -> Int -> SessionId -> Text -> Text -> IO (Maybe Json, [Json])
acpSetConfigOption c micros sid configId value =
  acpRequest
    c
    micros
    "session/set_config_option"
    ( JObject
        [ ("sessionId", JString sid),
          ("configId", JString configId),
          ("value", JString value)
        ]
    )

-- | @session/cancel@ notification for the live session.
acpCancel :: AcpClient -> SessionId -> IO ()
acpCancel client sid =
  acpSendValue
    client
    ( JObject
        [ ("jsonrpc", JString "2.0"),
          ("method", JString "session/cancel"),
          ("params", JObject [("sessionId", JString sid)])
        ]
    )

-- | The result of one prompt turn.
data PromptResult = PromptResult
  { -- | Assembled @agent_message_chunk@ text (the reply).
    prReply :: Text,
    -- | Assembled @agent_thought_chunk@ text (interiority).
    prThoughts :: Text,
    -- | @stopReason@ from the @session\/prompt@ response.
    prStopReason :: Maybe Text,
    -- | All parsed session updates, in arrival order.
    prUpdates :: [Update],
    -- | Every raw frame seen during the turn.
    prMessages :: [Json]
  }
  deriving (Show)

-- | ACP server session identity.
type SessionId = Text

-- | One ordinary dsh act. First-input seeding does not give either constructor
-- a separate protocol path.
data DshAct
  = Prompt Text
  | Cancel
  deriving (Eq, Show)

-- | Prompt correlation and the fixed post-response drain phase. A timed-out
-- channel is poisoned so no later turn can consume its late response.
data AcpPhase
  = AcpAwaiting Int UTCTime
  | AcpDraining Json UTCTime
  | AcpDone
  | AcpPoisoned
  deriving (Show)

-- | Live dsh protocol state. Raw frames are reversed internally and restored
-- to arrival order at the terminal emission; no folded 'DshAct' is retained.
data DshChannel = DshChannel
  { dshClient :: AcpClient,
    dshSession :: SessionId,
    dshPhase :: AcpPhase,
    dshMessages :: [Json]
  }

-- | One raw protocol frame or the terminal raw-frame boundary. Correlation,
-- reverse-RPC servicing, and the bounded drain belong to the machine; text,
-- activity, and collected results are runner projections of these frames.
data AcpEmission
  = AcpProtocol Json
  | AcpPromptDone (Maybe Json) [Json]
  deriving (Show)

-- | The decomposed dsh ACP machine. Its acquisition action constructs a live
-- client and session quietly. 'init' seeds that channel with its first
-- 'DshAct' through the same commit function as every later act. The supplied
-- post-prompt action is runner-local and is neither act nor channel state.
type DshBody = Body (,) (,) DshChannel DshChannel (K IO) DshAct AcpEmission

dshBody :: IO DshChannel -> Int -> IO () -> DshBody
dshBody acquire micros afterPrompt =
  Body
    (K $ \act -> acquire >>= (`commitAct` act))
    (K (uncurry commitAct))
    (K emitPrompt)
  where
    commitAct channel act = case act of
      Cancel ->
        acpCancel (dshClient channel) (dshSession channel) >> pure channel
      Prompt promptText -> do
        case dshPhase channel of
          AcpDone -> pure ()
          AcpPoisoned -> ioError (userError "acp: commit on a timed-out channel")
          _ -> ioError (userError "acp: prompt committed while another prompt is active")
        rid <-
          acpSendRequest
            (dshClient channel)
            "session/prompt"
            ( JObject
                [ ("sessionId", JString (dshSession channel)),
                  ( "prompt",
                    JArray (V.fromList [JObject [("type", JString ("text" :: Text)), ("text", JString promptText)]])
                  )
                ]
            )
        afterPrompt
        started <- getCurrentTime
        pure channel {dshPhase = AcpAwaiting rid started, dshMessages = []}

    emitPrompt channel = case dshPhase channel of
      AcpDone -> ioError (userError "acp: emit after prompt settlement")
      AcpPoisoned -> ioError (userError "acp: emit after prompt timeout")
      AcpAwaiting rid started -> do
        received <- readWithin micros started channel
        case received of
          Nothing -> settle channel Nothing AcpPoisoned
          Just (raw, frame, updated) -> case frame of
            Just (Response i _) | i == rid -> do
              now <- getCurrentTime
              protocol raw updated {dshPhase = AcpDraining raw now}
            _ -> protocol raw updated
      AcpDraining response started -> do
        received <- readWithin 500_000 started channel
        case received of
          Nothing -> settle channel (Just response) AcpDone
          Just (raw, _, updated) -> protocol raw updated

    readWithin budget started channel = do
      left <- budgetLeft budget started
      if left <= 0
        then pure Nothing
        else do
          received <- acpReadClassified (dshClient channel) left
          pure (fmap (record channel) received)

    record channel (raw, frame) =
      (raw, frame, channel {dshMessages = raw : dshMessages channel})

    protocol raw channel = pure (channel, AcpProtocol raw)

    settle channel response phase =
      pure
        ( channel {dshPhase = phase},
          AcpPromptDone response (reverse (dshMessages channel))
        )

budgetLeft :: Int -> UTCTime -> IO Int
budgetLeft micros started = do
  now <- getCurrentTime
  pure (micros - round (realToFrac (diffUTCTime now started) * 1e6 :: Double))

-- | Collect raw prompt frames into the compatibility result.
promptResult :: Maybe Json -> [Json] -> PromptResult
promptResult response messages =
  PromptResult
    { prReply = T.concat [text | AgentMessageChunk text <- updates],
      prThoughts = T.concat [text | AgentThoughtChunk text <- updates],
      prStopReason = response >>= resultOf >>= textAt "stopReason",
      prUpdates = updates,
      prMessages = messages
    }
  where
    updates = mapMaybe frameUpdate messages

-- | @session/prompt@ with a single text block. This compatibility runner
-- collects the Body's emissions until its one terminal projection.
acpPrompt :: AcpClient -> Int -> SessionId -> Text -> IO PromptResult
acpPrompt client micros sid promptText = snd <$> acpPromptWithResponse client micros sid promptText

-- | Prompt with the exact correlated response exposed beside collected
-- updates. A timed-out request is not drained: its late response must stay
-- distinguishable from the next turn's response.
acpPromptWithResponse :: AcpClient -> Int -> SessionId -> Text -> IO (Maybe Json, PromptResult)
acpPromptWithResponse = acpPromptWithResponseOnSend (pure ())

-- | Prompt with an action run after the request is on the wire. This keeps
-- cancellation installation ordered after prompt submission.
acpPromptWithResponseOnSend :: IO () -> AcpClient -> Int -> SessionId -> Text -> IO (Maybe Json, PromptResult)
acpPromptWithResponseOnSend onSend client micros sid promptText = do
  let Body initialise _ emitBody = dshBody (pure (DshChannel client sid AcpDone [])) micros onSend
  channel <- runK initialise (Prompt promptText)
  collect emitBody channel
  where
    collect emitBody channel = do
      (next, emission) <- runK emitBody channel
      case emission of
        AcpProtocol _ -> collect emitBody next
        AcpPromptDone response messages -> pure (response, promptResult response messages)

-- ---------------------------------------------------------------------------
-- Seat
-- ---------------------------------------------------------------------------

-- | ACP seat: one persistent child, one session, one prompt per body — the
-- multi-turn repl the one-shot CLI seats cannot be.  The caller owns the
-- lifecycle: 'openAcp', 'acpInitialize', 'acpNewSession' (whose session id
-- is passed here), and 'closeAcp' when the seat stands down.
acpHost ::
  -- | Seat name (used as the 'from' field of reply posts).
  Text ->
  -- | Deadline for initialize/new/prompt turns, in microseconds.
  Int ->
  -- | System prompt text prepended to every message.
  Text ->
  AcpClient ->
  -- | The ACP session id, from 'acpNewSession'.
  SessionId ->
  Host
acpHost name micros systemPrompt c sid =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = \bodies -> do
        r <- acpPrompt c micros sid (systemPrompt <> "\n\nUser messages:\n" <> T.unlines bodies)
        pure [prReply r]
    }
