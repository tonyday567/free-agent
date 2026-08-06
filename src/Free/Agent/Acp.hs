{-# LANGUAGE OverloadedStrings #-}

-- | ACP (Agent Client Protocol) JSON-RPC 2.0 NDJSON client over
-- 'Circuit.Agent.Process' ends.
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
--   * @fs\/*@ params carry @uri@ (with a @file:\/\/@ prefix), not the
--     spec's @path@ — both are accepted here.
--
-- Requests travel through the shared stdin 'commit' port of
-- 'openProcessPorts'; replies are polled from the stdout log cursor.
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

    -- * Session updates
    Update (..),
    parseUpdate,

    -- * Requests
    acpSendValue,
    acpReadFrame,
    acpRequest,
    acpInitialize,
    acpNewSession,
    acpSetConfigOption,
    acpSetModeAuto,
    acpPrompt,
    PromptResult (..),
    acpCancel,
    acpReadStderr,
  )
where

import Circuit.Agent.Process
  ( ProcessPorts (..),
    ReplConfig (..),
    defaultReplConfig,
    openProcessPorts,
  )
import Circuit.Ends (Ends, commit, companion, conjoint, emit, open)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.Aeson (Result (..), Value (..), eitherDecodeStrict, encode, fromJSON, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldr)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, openFile)
import Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | What to spawn and where the plumbing lives.
data AcpConfig = AcpConfig
  { -- | The executable (default @"kimi"@).
    acpCommand :: FilePath,
    -- | argv (default @["acp"]@).
    acpArgs :: [String],
    -- | Scratch directory: stdin FIFO, stdout/stderr logs, and the child's
    -- working directory.  The ACP session cwd is pinned separately via
    -- @session/new@.
    acpWorkDir :: FilePath,
    -- | Optional raw-frame transcript (NDJSON, one @{ts,dir,raw}@ per line,
    -- @dir@ ∈ send\/recv), mirroring the python probe's transcript.
    acpTranscript :: Maybe FilePath
  }
  deriving (Show, Eq)

-- | @kimi acp@ with plumbing under @\/tmp\/free-agent-acp@.
defaultAcpConfig :: AcpConfig
defaultAcpConfig =
  AcpConfig
    { acpCommand = "kimi",
      acpArgs = ["acp"],
      acpWorkDir = "/tmp/free-agent-acp",
      acpTranscript = Nothing
    }

-- ---------------------------------------------------------------------------
-- Client
-- ---------------------------------------------------------------------------

-- | A live ACP child process plus client-side protocol state.
data AcpClient = AcpClient
  { acpPorts :: ProcessPorts [Text] [Text] [Text],
    acpConfig :: AcpConfig,
    -- | Next JSON-RPC request id (monotonic from 1).
    acpNextId :: IORef Int,
    -- | Received raw lines not yet consumed by a read.
    acpQueue :: IORef [Text],
    -- | Persistent writer on the stdin FIFO so the child never sees EOF
    -- between per-line commits.
    acpKeepAlive :: Handle
  }

-- | Spawn the ACP child and open the protocol state.
--
-- Stale stdout/stderr logs from a previous run at the same paths are
-- truncated first (the ports' cursors rewind to 0 on open).
openAcp :: AcpConfig -> IO AcpClient
openAcp cfg = do
  let wd = acpWorkDir cfg
  createDirectoryIfMissing True wd
  writeFile (wd </> "acp-stdout.log") ""
  writeFile (wd </> "acp-stderr.log") ""
  pp <-
    openProcessPorts
      defaultReplConfig
        { replCommand = acpCommand cfg,
          replArgs = acpArgs cfg,
          replStdinPath = wd </> "acp-stdin",
          replStdoutPath = wd </> "acp-stdout.log",
          replStderrPath = wd </> "acp-stderr.log",
          replWorkingDir = wd,
          replCursorPath = Nothing
        }
  keepAlive <- openFile (wd </> "acp-stdin") WriteMode
  nref <- newIORef 1
  qref <- newIORef []
  pure
    AcpClient
      { acpPorts = pp,
        acpConfig = cfg,
        acpNextId = nref,
        acpQueue = qref,
        acpKeepAlive = keepAlive
      }

-- | Close the keep-alive writer and terminate the child.
closeAcp :: AcpClient -> IO ()
closeAcp c = do
  void $ try @SomeException (hClose (acpKeepAlive c))
  peClose (acpPorts c)

-- ---------------------------------------------------------------------------
-- Transcript
-- ---------------------------------------------------------------------------

logFrame :: AcpClient -> Text -> Text -> IO ()
logFrame c dir raw =
  forM_ (acpTranscript (acpConfig c)) $ \fp -> do
    now <- getCurrentTime
    let line = encodeText (object ["ts" .= show now, "dir" .= dir, "raw" .= raw])
    TIO.appendFile fp (line <> "\n")

-- ---------------------------------------------------------------------------
-- JSON plumbing
-- ---------------------------------------------------------------------------

encodeText :: Value -> Text
encodeText = decodeUtf8 . BL.toStrict . encode

objGet :: Text -> Value -> Maybe Value
objGet k (Object o) = KM.lookup (K.fromText k) o
objGet _ _ = Nothing

textAt :: Text -> Value -> Maybe Text
textAt k v = case objGet k v of
  Just (String t) -> Just t
  _ -> Nothing

resultOf :: Value -> Maybe Value
resultOf = objGet "result"

-- | A classified incoming frame.
data Frame
  = -- | Response to one of our requests: id and the whole message
    -- (@result@ or @error@ lives inside).
    Response Int Value
  | -- | Reverse-RPC: kimi requests something from the client.
    AgentRequest Int Text Value
  | -- | Notification (no id): method and params.
    Notification Text Value
  deriving (Show)

-- | Classify a decoded message.  Note ids are 'Int's and may be 0 (kimi
-- sends @id: 0@ on @session\/request_permission@).
classifyFrame :: Value -> Maybe Frame
classifyFrame v = case (objGet "id" v, objGet "method" v) of
  (Just idV, Just (String m))
    | Success i <- fromJSON @Int idV ->
        Just (AgentRequest i m (fromMaybe Null (objGet "params" v)))
  (Just idV, _)
    | Success i <- fromJSON @Int idV -> Just (Response i v)
  (_, Just (String m)) ->
    Just (Notification m (fromMaybe Null (objGet "params" v)))
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Session updates
-- ---------------------------------------------------------------------------

-- | Parsed @session\/update@ notification payload (@params.update@).
data Update
  = AgentMessageChunk Text
  | AgentThoughtChunk Text
  | ToolCall Value
  | ToolCallUpdate Value
  | AvailableCommandsUpdate Value
  | SessionInfoUpdate Value
  | UsageUpdate Value
  | UnknownUpdate Text Value
  deriving (Eq, Show)

-- | Parse the @params@ of a @session\/update@ notification.  'Nothing' if
-- the @update@ object or its @sessionUpdate@ tag is missing.
parseUpdate :: Value -> Maybe Update
parseUpdate params = do
  upd <- objGet "update" params
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
      fromMaybe "" (objGet "content" upd >>= textAt "text")

-- ---------------------------------------------------------------------------
-- Send / receive
-- ---------------------------------------------------------------------------

-- | Encode and commit one JSON-RPC message line, logging the raw frame.
acpSendValue :: AcpClient -> Value -> IO ()
acpSendValue c v = do
  let line = encodeText v
  logFrame c "send" line
  runKleisli
    (commit (peIn (acpPorts c)) (companion (open :: Ends (Kleisli IO) () ())))
    [line]

-- | Poll the stdout log cursor for new lines, logging raw frames.
-- Blank lines are dropped.
acpPoll :: AcpClient -> IO [Text]
acpPoll c = do
  ls <-
    runKleisli
      (emit (peOut (acpPorts c)) (conjoint (open :: Ends (Kleisli IO) () ())))
      ()
  let ls' = filter (not . T.null) ls
  mapM_ (logFrame c "recv") ls'
  pure ls'

-- | Read the next decodable JSON message, polling the stdout log until one
-- arrives or the timeout (microseconds) expires.  Lines that fail to parse
-- are skipped (they stay in the transcript).
acpReadFrame :: AcpClient -> Int -> IO (Maybe Value)
acpReadFrame c micros = loop its
  where
    step = 20000
    its = max 1 (micros `div` step)
    loop 0 = pure Nothing
    loop n = do
      mv <- popQueue
      case mv of
        Just v -> pure (Just v)
        Nothing -> do
          ls <- acpPoll c
          unless (null ls) $ modifyIORef' (acpQueue c) (<> ls)
          if null ls
            then threadDelay step
            else pure ()
          loop (n - 1)
    popQueue = do
      q <- readIORef (acpQueue c)
      case q of
        [] -> pure Nothing
        (l : rest) -> do
          writeIORef (acpQueue c) rest
          case eitherDecodeStrict (encodeUtf8 l) of
            Right v -> pure (Just v)
            Left _ -> popQueue

-- ---------------------------------------------------------------------------
-- Reverse-RPC
-- ---------------------------------------------------------------------------

-- | kimi sends @uri@ with a @file:\/\/@ prefix where the ACP schema says
-- @path@; accept both.
uriPath :: Value -> Maybe FilePath
uriPath params = case textAt "uri" params of
  Just u -> Just (T.unpack (fromMaybe u (T.stripPrefix "file://" u)))
  Nothing -> T.unpack <$> textAt "path" params

sendResult :: AcpClient -> Int -> Value -> IO ()
sendResult c rid r =
  acpSendValue c (object ["jsonrpc" .= t2, "id" .= rid, "result" .= r])
  where
    t2 = "2.0" :: Text

sendError :: AcpClient -> Int -> Int -> Text -> IO ()
sendError c rid code msg =
  acpSendValue
    c
    ( object
        [ "jsonrpc" .= t2,
          "id" .= rid,
          "error" .= object ["code" .= code, "message" .= msg]
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
acpAnswer :: AcpClient -> Int -> Text -> Value -> IO ()
acpAnswer c rid method params = case method of
  "fs/read_text_file" -> case uriPath params of
    Nothing -> sendError c rid (-32602) "fs/read_text_file: missing uri/path"
    Just p -> do
      r <- try @SomeException (TIO.readFile p)
      case r of
        Right content -> sendResult c rid (object ["content" .= content])
        Left e -> sendError c rid (-32000) (T.pack (show e))
  "fs/write_text_file" -> case uriPath params of
    Nothing -> sendError c rid (-32602) "fs/write_text_file: missing uri/path"
    Just p -> do
      let content = fromMaybe "" (textAt "content" params)
      r <-
        try @SomeException
          (createDirectoryIfMissing True (takeDirectory p) >> TIO.writeFile p content)
      case r of
        Right () -> sendResult c rid (object [])
        Left e -> sendError c rid (-32000) (T.pack (show e))
  "session/request_permission" ->
    sendResult
      c
      rid
      ( object
          [ "outcome"
              .= object
                ["outcome" .= ("selected" :: Text), "optionId" .= firstOption]
          ]
      )
  _ -> sendError c rid (-32601) ("method not found: " <> method)
  where
    firstOption = case objGet "options" params of
      Just (Array os) ->
        fromMaybe "approve_once" (listToMaybe (mapMaybe (textAt "optionId") (foldr (:) [] os)))
      _ -> "approve_once"

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

-- | Send a request and poll until its response arrives or the timeout
-- (microseconds) expires.  Interleaved reverse-RPCs are answered (see
-- 'acpAnswer'); every frame seen (including the response) is returned in
-- arrival order alongside the response.
acpRequest :: AcpClient -> Int -> Text -> Value -> IO (Maybe Value, [Value])
acpRequest c micros method params = do
  rid <- atomicModifyIORef' (acpNextId c) (\i -> (i + 1, i))
  acpSendValue
    c
    ( object
        [ "jsonrpc" .= t2,
          "id" .= rid,
          "method" .= method,
          "params" .= params
        ]
    )
  loop rid [] its
  where
    t2 = "2.0" :: Text
    step = 20000
    its = max 1 (micros `div` step)
    loop _ acc 0 = pure (Nothing, reverse acc)
    loop rid acc n = do
      mv <- acpReadFrame c step
      case mv of
        Nothing -> loop rid acc (n - 1)
        Just v -> case classifyFrame v of
          Just (Response i _) | i == rid -> pure (Just v, reverse (v : acc))
          Just (AgentRequest i m p) -> do
            acpAnswer c i m p
            loop rid (v : acc) n
          _ -> loop rid (v : acc) n

-- | @initialize@ handshake: protocolVersion 1, fs client capabilities on,
-- terminal off.
acpInitialize :: AcpClient -> Int -> IO (Maybe Value, [Value])
acpInitialize c micros =
  acpRequest
    c
    micros
    "initialize"
    ( object
        [ "protocolVersion" .= (1 :: Int),
          "clientInfo"
            .= object
              [ "name" .= ("free-agent-acp" :: Text),
                "version" .= ("0.1.0" :: Text)
              ],
          "clientCapabilities"
            .= object
              [ "fs" .= object ["readTextFile" .= True, "writeTextFile" .= True],
                "terminal" .= False
              ]
        ]
    )

-- | @session/new@ with the session cwd pinned.  Returns the sessionId on
-- success.
acpNewSession :: AcpClient -> Int -> FilePath -> IO (Maybe Text, [Value])
acpNewSession c micros cwd = do
  (mresp, msgs) <-
    acpRequest
      c
      micros
      "session/new"
      (object ["cwd" .= cwd, "mcpServers" .= ([] :: [Value])])
  pure (mresp >>= resultOf >>= textAt "sessionId", msgs)

-- | @session/set_config_option@ — returns the full configOptions array.
acpSetConfigOption ::
  AcpClient -> Int -> Text -> Text -> Text -> IO (Maybe Value, [Value])
acpSetConfigOption c micros sid configId value =
  acpRequest
    c
    micros
    "session/set_config_option"
    ( object
        [ "sessionId" .= sid,
          "configId" .= configId,
          "value" .= value
        ]
    )

-- | Switch a session to @auto@ mode.  Do NOT use @session\/set_mode@ — it
-- hangs in kimi 0.33.0.
acpSetModeAuto :: AcpClient -> Int -> Text -> IO (Maybe Value, [Value])
acpSetModeAuto c micros sid = acpSetConfigOption c micros sid "mode" "auto"

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
    prMessages :: [Value]
  }
  deriving (Show)

-- | @session/prompt@ with a single text block.  Streams are collected
-- until the prompt response, then drained briefly for trailing
-- notifications (@usage_update@ arrives after the response).
acpPrompt :: AcpClient -> Int -> Text -> Text -> IO PromptResult
acpPrompt c micros sid promptText = do
  (mresp, msgs) <-
    acpRequest
      c
      micros
      "session/prompt"
      ( object
          [ "sessionId" .= sid,
            "prompt"
              .= [object ["type" .= ("text" :: Text), "text" .= promptText]]
          ]
      )
  trailing <- drain
  let allMsgs = msgs <> trailing
      updates =
        [ u
          | Just (Notification "session/update" ps) <- map classifyFrame allMsgs,
            Just u <- [parseUpdate ps]
        ]
      reply = T.concat [t | AgentMessageChunk t <- updates]
      thoughts = T.concat [t | AgentThoughtChunk t <- updates]
      stop = mresp >>= resultOf >>= textAt "stopReason"
  pure
    PromptResult
      { prReply = reply,
        prThoughts = thoughts,
        prStopReason = stop,
        prUpdates = updates,
        prMessages = allMsgs
      }
  where
    -- Read until 500ms idle, answering any late reverse-RPCs.
    drain = go []
      where
        go acc = do
          mv <- acpReadFrame c 500000
          case mv of
            Nothing -> pure (reverse acc)
            Just v -> do
              case classifyFrame v of
                Just (AgentRequest i m p) -> acpAnswer c i m p
                _ -> pure ()
              go (v : acc)

-- | @session/cancel@ notification — cancels the current turn.
acpCancel :: AcpClient -> Text -> IO ()
acpCancel c sid =
  acpSendValue
    c
    ( object
        [ "jsonrpc" .= t2,
          "method" .= ("session/cancel" :: Text),
          "params" .= object ["sessionId" .= sid]
        ]
    )
  where
    t2 = "2.0" :: Text

-- ---------------------------------------------------------------------------
-- Misc
-- ---------------------------------------------------------------------------

-- | Read any pending stderr diagnostics (non-blocking poll).
acpReadStderr :: AcpClient -> IO [Text]
acpReadStderr c =
  runKleisli
    (emit (peErr (acpPorts c)) (conjoint (open :: Ends (Kleisli IO) () ())))
    ()
