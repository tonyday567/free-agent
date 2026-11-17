{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Explicit backend sessions, separate from mailbox and publication policy.
--
-- A 'Session' has independent input and output legs. Process recipes run one
-- attempt and report stale, failure, interruption, exhaustion, and ceilings
-- without replay. HTTP and protocol backends report outcomes from status and
-- frames rather than response prose. Capabilities state which backends can
-- resume, emit incrementally, or cancel a turn.
module Free.Agent.Backend
  ( Backend,
    BackendKind (..),
    BackendCapabilities (..),
    SessionSupport (..),
    EventDelivery (..),
    BackendRequest (..),
    BackendSession (..),
    defaultRequest,
    SendError (..),
    Session,
    sessionCapabilities,
    sessionStartupFailure,
    BackendEvent (..),
    BackendOutput (..),
    OutputChannel (..),
    BackendOutcome (..),
    FailureKind (..),
    BackendEvidence (..),
    hermesBackend,
    kimiBackend,
    dshBackend,
    dshAcpBackend,
    commandBackend,
    processBackend,
    directHttpBackend,
    gatewayBackend,
    acpBackend,
    openBackend,
    sendSession,
    nextEvent,
    cancelSession,
    closeSession,
  )
where

import Circuit.Machine (Body (Body), Moore)
import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Circuit.Tensor (unConst)
import Control.Applicative ((<|>))
import Control.Concurrent (Chan, MVar, forkIO, modifyMVar, newChan, newEmptyMVar, newMVar, putMVar, readChan, readMVar, writeChan)
import Control.Exception (AsyncException, IOException, SomeException, catch, displayException, fromException, onException, throwIO, try)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Aeson (FromJSON (parseJSON), decodeStrict, withObject, (.:!))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Free.Agent.Acp (AcpClient, AcpConfig (..), Frame (..), Update (..), acpInitialize, acpNewSession, acpPromptWithResponseOnSend, acpResumeSession, acpSendValue, classifyFrame, closeAcp, defaultAcpConfig, openAcp, prMessages, prReply, prStopReason, prThoughts, prUpdates)
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cliCeiling, cliExhausted, cliInterrupted, cliScrub, cliStale, dshCli, kimiCli)
import Free.Agent.Gateway (GatewayClient (..), GatewayConfig (..), openGateway, parseSseFrame)
import Free.Agent.Host (BareConfig (..), hermesCli)
import Free.Agent.StdPorts (frameAgent, sseMarks)
import Network.HTTP.Client hiding (defaultRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hClose, hPutStr)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, terminateProcess, waitForProcess)
import System.Process qualified as Process

-- | Backend implementation selected by a constructor.
data BackendKind
  = HermesBackend
  | KimiBackend
  | DshBackend
  | CommandBackend
  | CustomProcessBackend
  | DirectHttpBackend
  | GatewayBackend
  | AcpBackend
  | DshAcpBackend
  deriving (Eq, Show)

-- | Session continuity actually supported by the backend.
data SessionSupport = NoSessions | FileSessions | ServerSessions
  deriving (Eq, Show)

-- | How output events become observable.
data EventDelivery = OneShot | CollectedEvents | IncrementalEvents
  deriving (Eq, Show)

-- | Observable capabilities; no streaming or cancellation is inferred from I/O.
data BackendCapabilities = BackendCapabilities
  { backendKind :: BackendKind,
    backendSessions :: SessionSupport,
    backendDelivery :: EventDelivery,
    backendCancellation :: Bool
  }
  deriving (Eq, Show)

-- | Session choice for one process invocation.
data BackendSession
  = -- | Read the configured session file when the backend has one.
    AutomaticSession
  | -- | Do not present a stored session id.
    FreshSession
  | -- | Present this session id without reading the session file.
    ResumeSession Text
  deriving (Eq, Show)

-- | One input and its session choice.
data BackendRequest = BackendRequest
  { requestText :: Text,
    requestSession :: BackendSession
  }
  deriving (Eq, Show)

-- | Automatic stored-session behaviour where supported, no session otherwise.
defaultRequest :: Text -> BackendRequest
defaultRequest text = BackendRequest text AutomaticSession

-- | Why an input was not started.
data SendError = SendActive | SendClosed | SendUnsupportedSession
  deriving (Eq, Show)

-- | Output is content; it is not a terminal outcome.
data BackendOutput = BackendOutput
  { outputChannel :: OutputChannel,
    outputText :: Text
  }
  deriving (Eq, Show)

data OutputChannel
  = ReplyOutput
  | ThoughtOutput
  | DeltaOutput
  | UsageOutput
  | StdoutOutput
  | StderrOutput
  | RawOutput
  deriving (Eq, Show)

-- | Evidence retained at settlement. Fields not supplied by a backend stay empty.
data BackendEvidence = BackendEvidence
  { evidenceExitCode :: Maybe Int,
    evidenceSessionId :: Maybe Text,
    evidenceRawOutput :: Text,
    evidenceStopReason :: Maybe Text,
    evidenceUsage :: Maybe Text
  }
  deriving (Eq, Show)

-- | Failure mechanism, distinct from stale/interrupted/exhausted/ceiling outcomes.
data FailureKind
  = SpawnFailure
  | ProcessFailure
  | HttpFailure Int
  | DecodeFailure
  | ProtocolFailure
  | TimeoutFailure
  | EmptyReply
  deriving (Eq, Show)

-- | Terminal fact for one input.
data BackendOutcome
  = Completed BackendEvidence
  | Cancelled BackendEvidence
  | Failed FailureKind Text BackendEvidence
  | Stale Text BackendEvidence
  | Exhausted Text BackendEvidence
  | Interrupted Text BackendEvidence
  | Ceiling Text BackendEvidence
  | Unavailable Text
  deriving (Eq, Show)

-- | Zero or more outputs precede one terminal outcome for each accepted input.
data BackendEvent
  = Output BackendOutput
  | Settled BackendOutcome
  deriving (Eq, Show)

-- | Constructor configuration, before resources are opened.
data Backend
  = ProcessBackend BackendKind Cli
  | DirectHttp BareConfig Text
  | Gateway GatewayConfig Text
  | Acp AcpConfig Int Text
  | DshAcp AcpConfig Int Text FilePath

hermesBackend :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> FilePath -> Backend
hermesBackend model provider reasoning yolo sessionFile =
  ProcessBackend HermesBackend (hermesCli model provider reasoning yolo sessionFile)

kimiBackend :: Maybe Text -> Maybe Text -> FilePath -> Backend
kimiBackend model provider sessionFile =
  ProcessBackend KimiBackend (kimiCli model provider sessionFile)

dshBackend :: Maybe Text -> FilePath -> Maybe FilePath -> FilePath -> Backend
dshBackend card basePatch modelPatch sessionFile =
  ProcessBackend DshBackend (dshCli card basePatch modelPatch sessionFile)

-- | Persistent dsh through ACP, with its workspace, session store, and patch
-- route pinned independently of the bus root and launcher's environment.
-- The card is presented on a newly created session, not replayed on resume.
dshAcpBackend :: Maybe Text -> FilePath -> Maybe FilePath -> FilePath -> FilePath -> FilePath -> Backend
dshAcpBackend card basePatch modelPatch workspace dshHome sessionFile =
  DshAcp config 600_000_000 (fromMaybe "" card) sessionFile
  where
    config =
      defaultAcpConfig
        { acpCommand = "dsh",
          acpArgs =
            ["acp", "--patch", basePatch]
              <> maybe [] (\path -> ["--patch", path]) modelPatch,
          acpWorkDir = workspace,
          acpEnv = [("DSH_HOME", dshHome)]
        }

-- | A generic process recipe, kept separate from the named backend constructors.
processBackend :: Cli -> Backend
processBackend = ProcessBackend CustomProcessBackend

-- | An external command receives fixed argv followed by the whole input.
commandBackend :: FilePath -> [String] -> Backend
commandBackend command args =
  ProcessBackend CommandBackend $
    Cli
      { cliCommand = command,
        cliArgv = \prompt _ -> args <> [T.unpack prompt],
        cliStdin = const "",
        cliSessionFile = "",
        cliSessionId = const Nothing,
        cliStale = \_ _ -> False,
        cliExhausted = \_ _ -> False,
        cliScrub = T.strip,
        cliStderr = StderrMerge,
        cliStderrTee = Nothing,
        cliSessionReport = Nothing
      }

directHttpBackend :: BareConfig -> Text -> Backend
directHttpBackend = DirectHttp

gatewayBackend :: GatewayConfig -> Text -> Backend
gatewayBackend = Gateway

acpBackend :: AcpConfig -> Int -> Text -> Backend
acpBackend = Acp

-- | An open backend session. Use 'sendSession' and 'nextEvent' for its two legs.
data Session = Session
  { sessionCapabilities :: BackendCapabilities,
    sessionStartupFailure :: Maybe BackendOutcome,
    sessionSend :: BackendRequest -> IO (Either SendError ()),
    sessionNext :: IO BackendEvent,
    sessionCancel :: IO Bool,
    sessionClose :: IO ()
  }

data Active = Active
  { activeCancelled :: IORef Bool,
    activeProcess :: IORef (Maybe ProcessHandle),
    activeCancel :: IORef (IO ()),
    activeDone :: MVar (),
    activeOutcome :: MVar BackendOutcome
  }

data SessionState = SessionState
  { stateCapabilities :: BackendCapabilities,
    stateEvents :: Chan BackendEvent,
    stateActive :: MVar (Maybe Active),
    stateClosed :: IORef Bool,
    stateCloseResource :: IO (),
    stateRun :: SessionState -> Active -> BackendRequest -> IO BackendOutcome
  }

sendSession :: Session -> BackendRequest -> IO (Either SendError ())
sendSession = sessionSend

nextEvent :: Session -> IO BackendEvent
nextEvent = sessionNext

-- | Cancel the active process and wait for its terminal evidence.
-- Returns true only when the recorded outcome is 'Cancelled'.
cancelSession :: Session -> IO Bool
cancelSession = sessionCancel

-- | Close local/session resources. This does not claim per-turn cancellation
-- for backends whose capabilities say they have none.
closeSession :: Session -> IO ()
closeSession = sessionClose

newActive :: IO Active
newActive = Active <$> newIORef False <*> newIORef Nothing <*> newIORef (pure ()) <*> newEmptyMVar <*> newEmptyMVar

emptyEvidence :: BackendEvidence
emptyEvidence = BackendEvidence Nothing Nothing "" Nothing Nothing

capabilitiesFor :: Backend -> BackendCapabilities
capabilitiesFor backend = case backend of
  ProcessBackend kind _ ->
    BackendCapabilities
      { backendKind = kind,
        backendSessions = if kind == CommandBackend then NoSessions else FileSessions,
        backendDelivery = OneShot,
        backendCancellation = True
      }
  DirectHttp {} -> BackendCapabilities DirectHttpBackend NoSessions OneShot False
  Gateway {} -> BackendCapabilities GatewayBackend ServerSessions IncrementalEvents False
  Acp {} -> BackendCapabilities AcpBackend ServerSessions CollectedEvents False
  DshAcp {} -> BackendCapabilities DshAcpBackend ServerSessions CollectedEvents True

-- | Open configured resources. Open failures become a closed session with one
-- terminal event, so environment failure is observable through the same leg.
openBackend :: Backend -> IO Session
openBackend backend = case backend of
  ProcessBackend _ cli ->
    newSession (capabilitiesFor backend) (pure ()) (runProcess cli)
  DirectHttp cfg systemPrompt -> do
    opened <- trySync $ do
      manager <- newManager tlsManagerSettings
      request <- parseRequest (T.unpack (baseUrl cfg <> "/chat/completions"))
      pure (manager, request)
    case opened of
      Left err -> failedSession (capabilitiesFor backend) (Unavailable err)
      Right (manager, request) ->
        newSession (capabilitiesFor backend) (pure ()) (runHttp cfg systemPrompt manager request)
  Gateway cfg systemPrompt -> do
    opened <- trySync (openGateway cfg)
    case opened of
      Left err -> failedSession (capabilitiesFor backend) (Unavailable err)
      Right client ->
        newSession (capabilitiesFor backend) (pure ()) (runGateway systemPrompt client)
  Acp cfg micros systemPrompt ->
    openAcpBackend backend cfg micros systemPrompt Nothing False
  DshAcp cfg micros systemPrompt sessionFile ->
    openAcpBackend backend cfg micros systemPrompt (Just sessionFile) True

openAcpBackend :: Backend -> AcpConfig -> Int -> Text -> Maybe FilePath -> Bool -> IO Session
openAcpBackend backend cfg micros systemPrompt sessionFile cancellable = do
  workspace <- makeAbsolute (acpWorkDir cfg)
  let config = cfg {acpWorkDir = workspace}
  stored <- maybe (pure Nothing) readStoredSession sessionFile
  opened <- trySync (openAcp config)
  case opened of
    Left err -> failedSession (capabilitiesFor backend) (Unavailable err)
    Right client -> do
      initial <-
        trySync $
          case stored of
            Just sid -> fmap (,False) <$> resumeAcpSession client micros workspace sid
            Nothing -> fmap (,True) <$> openAcpSession client micros workspace
      case initial of
        Left err -> do
          closeAcp client
          failedSession (capabilitiesFor backend) (Failed ProtocolFailure err emptyEvidence)
        Right (Left outcome) -> do
          closeAcp client
          failedSession (capabilitiesFor backend) outcome
        Right (Right (sid, fresh)) -> do
          persisted <-
            case sessionFile of
              Just path -> trySync (writeStoredSession path sid)
              Nothing -> pure (Right ())
          case persisted of
            Left err -> do
              closeAcp client
              failedSession (capabilitiesFor backend) (Unavailable err)
            Right () -> do
              cardState <-
                if cancellable
                  then Just <$> newIORef fresh
                  else pure Nothing
              newSession
                (capabilitiesFor backend)
                (closeAcp client)
                (runAcp micros systemPrompt client sid cancellable cardState)

failedSession :: BackendCapabilities -> BackendOutcome -> IO Session
failedSession caps outcome = do
  events <- newChan
  writeChan events (Settled outcome)
  closed <- newIORef True
  pure
    Session
      { sessionCapabilities = caps,
        sessionStartupFailure = Just outcome,
        sessionSend = \_ -> pure (Left SendClosed),
        sessionNext = readChan events,
        sessionCancel = pure False,
        sessionClose = writeIORef closed True
      }

newSession :: BackendCapabilities -> IO () -> (SessionState -> Active -> BackendRequest -> IO BackendOutcome) -> IO Session
newSession caps closeResource run = do
  events <- newChan
  active <- newMVar Nothing
  closed <- newIORef False
  let state = SessionState caps events active closed closeResource run
      send request = do
        isClosed <- readIORef (stateClosed state)
        if isClosed
          then pure (Left SendClosed)
          else
            if not (sessionChoiceSupported (backendSessions caps) (requestSession request))
              then pure (Left SendUnsupportedSession)
              else modifyMVar active $ \current -> case current of
                Just _ -> pure (current, Left SendActive)
                Nothing -> do
                  closedNow <- readIORef (stateClosed state)
                  if closedNow
                    then pure (current, Left SendClosed)
                    else do
                      turn <- newActive
                      _ <- forkIO (worker state turn request)
                      pure (Just turn, Right ())
      cancel =
        if not (backendCancellation caps)
          then pure False
          else do
            current <- readMVar active
            case current of
              Nothing -> pure False
              Just turn -> do
                writeIORef (activeCancelled turn) True
                process <- readIORef (activeProcess turn)
                forM_ process $ \handle ->
                  catch (terminateProcess handle) (\(_ :: IOException) -> pure ())
                cancelAction <- readIORef (activeCancel turn)
                catch cancelAction (\(_ :: IOException) -> pure ())
                readMVar (activeDone turn)
                outcome <- readMVar (activeOutcome turn)
                pure (case outcome of Cancelled _ -> True; _ -> False)
      close = do
        writeIORef closed True
        when (backendCancellation caps) (void cancel)
        stateCloseResource state
  pure (Session caps Nothing send (readChan events) cancel close)

worker :: SessionState -> Active -> BackendRequest -> IO ()
worker state turn request = do
  result <- trySync (stateRun state state turn request)
  let outcome = case result of
        Left err -> Failed ProcessFailure err emptyEvidence
        Right value -> value
  modifyMVar (stateActive state) $ \_ -> do
    writeChan (stateEvents state) (Settled outcome)
    pure (Nothing, ())
  putMVar (activeOutcome turn) outcome
  putMVar (activeDone turn) ()

sessionChoiceSupported :: SessionSupport -> BackendSession -> Bool
sessionChoiceSupported support choice = case (support, choice) of
  (NoSessions, ResumeSession _) -> False
  (NoSessions, _) -> True
  (FileSessions, _) -> True
  (ServerSessions, AutomaticSession) -> True
  (ServerSessions, _) -> False

trySync :: IO a -> IO (Either Text a)
trySync action = do
  result <- try @SomeException action
  case result of
    Right value -> pure (Right value)
    Left err
      | isJust (fromException err :: Maybe AsyncException) -> throwIO err
      | otherwise -> pure (Left (T.pack (displayException err)))

-- Process execution ---------------------------------------------------------

data RunReport = RunReport
  { reportSession :: Maybe Text,
    reportPartial :: Bool,
    reportInterrupted :: Bool,
    reportExitReason :: Maybe Text
  }

instance FromJSON RunReport where
  parseJSON = withObject "backend run report" $ \object ->
    RunReport
      <$> object .:! "session_id"
      <*> (fromMaybe False <$> object .:! "partial")
      <*> (fromMaybe False <$> object .:! "interrupted")
      <*> object .:! "turn_exit_reason"

runProcess :: Cli -> SessionState -> Active -> BackendRequest -> IO BackendOutcome
runProcess cli state turn request = do
  stored <- case requestSession request of
    AutomaticSession
      | backendSessions (stateCapabilities state) == FileSessions ->
          readStoredSession (cliSessionFile cli)
    ResumeSession sid -> pure (Just sid)
    _ -> pure Nothing
  cancelled <- readIORef (activeCancelled turn)
  if cancelled
    then pure (Cancelled emptyEvidence)
    else do
      forM_ (cliSessionReport cli) $ \path ->
        catch (removeFile path) (\(_ :: IOException) -> pure ())
      created <-
        try @IOException $
          createProcess
            (proc (cliCommand cli) (cliArgv cli (requestText request) stored))
              { Process.std_in = CreatePipe,
                Process.std_out = CreatePipe,
                Process.std_err = CreatePipe
              }
      case created of
        Left err -> pure (Unavailable (T.pack (displayException err)))
        Right (Just stdinH, Just stdoutH, Just stderrH, handle) ->
          flip onException (terminateProcess handle >> void (waitForProcess handle)) $ do
            writeIORef (activeProcess turn) (Just handle)
            cancelledNow <- readIORef (activeCancelled turn)
            when cancelledNow (terminateProcess handle)
            outVar <- newEmptyMVar
            errVar <- newEmptyMVar
            _ <- forkIO (BS.hGetContents stdoutH >>= putMVar outVar)
            _ <- forkIO (BS.hGetContents stderrH >>= putMVar errVar)
            let input = cliStdin cli (requestText request)
            if null input
              then hClose stdinH
              else void $ forkIO $ catch (hPutStr stdinH input >> hClose stdinH) (\(_ :: IOException) -> pure ())
            code <- waitForProcess handle
            out <- TE.decodeUtf8 <$> readMVar outVar
            err <- TE.decodeUtf8 <$> readMVar errVar
            writeIORef (activeProcess turn) Nothing
            forM_ (cliStderrTee cli) $ \path ->
              catch
                (createDirectoryIfMissing True (takeDirectory path) >> TIO.appendFile path err)
                (\(_ :: IOException) -> pure ())
            (reportRaw, report) <- maybe (pure (Nothing, Nothing)) readRunReport (cliSessionReport cli)
            wasCancelled <- readIORef (activeCancelled turn)
            let raw = out <> err
                routed = case cliStderr cli of
                  StderrDrop -> ""
                  StderrMerge -> err
                  StderrMark
                    | T.null (T.strip err) -> ""
                    | otherwise -> "\n-- stderr --\n" <> err
                reply = cliScrub cli (out <> routed)
                sid = (report >>= reportSession) <|> cliSessionId cli raw
                exit = case code of ExitSuccess -> 0; ExitFailure n -> n
                evidence = BackendEvidence (Just exit) sid raw (report >>= reportExitReason) reportRaw
                ceilingByReport = code == ExitFailure 2 && maybe False reportPartial report
                interruptedByReport = maybe False reportInterrupted report
                outcome
                  | wasCancelled = Cancelled evidence
                  | isJust stored && cliStale cli code raw = Stale raw evidence
                  | cliCeiling code raw || ceilingByReport = Ceiling raw evidence
                  | cliInterrupted code raw || interruptedByReport = Interrupted raw evidence
                  | cliExhausted cli code raw = Exhausted raw evidence
                  | code /= ExitSuccess = Failed ProcessFailure ("process exited " <> T.pack (show exit)) evidence
                  | otherwise = Completed evidence
            finalOutcome <- case outcome of
              Completed _ ->
                case sid of
                  Just newSid -> do
                    written <- try @IOException (writeStoredSession (cliSessionFile cli) newSid)
                    pure $ case written of
                      Left writeErr -> Failed ProcessFailure (T.pack (displayException writeErr)) evidence
                      Right () -> outcome
                  Nothing -> pure outcome
              _ -> pure outcome
            case finalOutcome of
              Completed _ -> emitText state ReplyOutput reply
              _ -> do
                emitText state StdoutOutput out
                emitText state StderrOutput err
            pure finalOutcome
        Right _ -> pure (Unavailable "createProcess did not return all requested pipes")

readStoredSession :: FilePath -> IO (Maybe Text)
readStoredSession path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException (TIO.readFile path)
      pure $ case result of
        Left _ -> Nothing
        Right text -> let sid = T.strip text in if T.null sid then Nothing else Just sid

writeStoredSession :: FilePath -> Text -> IO ()
writeStoredSession path sid = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path sid

readRunReport :: FilePath -> IO (Maybe Text, Maybe RunReport)
readRunReport path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left _ -> (Nothing, Nothing)
    Right bytes -> (Just (TE.decodeUtf8 bytes), decodeStrict bytes)

emitText :: SessionState -> OutputChannel -> Text -> IO ()
emitText state channel text =
  unless (T.null text) $ writeChan (stateEvents state) (Output (BackendOutput channel text))

-- Direct HTTP ---------------------------------------------------------------

runHttp :: BareConfig -> Text -> Manager -> Request -> SessionState -> Active -> BackendRequest -> IO BackendOutcome
runHttp cfg systemPrompt manager initial state _ request = do
  let body =
        BL.fromStrict $
          encodeJson $
            JObject
              [ ("model", JString (model cfg)),
                ( "messages",
                  JArray
                    ( V.fromList
                        [ JObject [("role", JString "system"), ("content", JString systemPrompt)],
                          JObject [("role", JString "user"), ("content", JString (requestText request))]
                        ]
                    )
                ),
                ("max_tokens", JNumber 4096)
              ]
      httpRequest =
        initial
          { method = "POST",
            requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer " <> BC8.pack (T.unpack (key cfg)))],
            requestBody = RequestBodyLBS body
          }
  response <- try @HttpException (httpLbs httpRequest manager)
  case response of
    Left err -> pure (Failed ProcessFailure (T.pack (displayException err)) emptyEvidence)
    Right value -> do
      let status = statusCode (responseStatus value)
          raw = TE.decodeUtf8 (BL.toStrict (responseBody value))
          evidence = emptyEvidence {evidenceRawOutput = raw}
      if status < 200 || status >= 300
        then do
          emitText state RawOutput raw
          pure (Failed (HttpFailure status) ("HTTP " <> T.pack (show status)) evidence)
        else case decodeJson (BL.toStrict (responseBody value)) of
          Left err -> pure (Failed DecodeFailure (T.pack err) evidence)
          Right json -> case chatContent json of
            Nothing -> pure (Failed DecodeFailure "missing choices[0].message.content" evidence)
            Just reply -> do
              emitText state ReplyOutput reply
              pure (Completed evidence)

chatContent :: Json -> Maybe Text
chatContent json = do
  JArray choices <- jsonField "choices" json
  first <- listToMaybe (V.toList choices)
  message <- jsonField "message" first
  JString content <- jsonField "content" message
  pure content

jsonField :: Text -> Json -> Maybe Json
jsonField name (JObject fields) = lookup name fields
jsonField _ _ = Nothing

-- Gateway SSE ---------------------------------------------------------------

runCell :: Moore (,) s (->) a b -> s -> a -> (b, s)
runCell (Body _ step output) state input =
  let next = step (state, input) in (unConst (output next), next)

runGateway :: Text -> GatewayClient -> SessionState -> Active -> BackendRequest -> IO BackendOutcome
runGateway systemPrompt client state _ request = do
  created <- try @HttpException $ do
    initial <- parseRequest (T.unpack (gwBaseUrl (gcConfig client)) <> "/api/sessions/" <> T.unpack (gcSessionId client) <> "/chat/stream")
    pure $
      initial
        { method = "POST",
          requestHeaders = [("Authorization", "Bearer " <> gcKey client), ("Content-Type", "application/json")],
          requestBody = RequestBodyLBS (BL.fromStrict (encodeJson (JObject [("message", JString (systemPrompt <> "\n\nUser messages:\n" <> requestText request))])))
        }
  case created of
    Left err -> pure (Unavailable (T.pack (displayException err)))
    Right httpRequest -> do
      response <- try @HttpException (withResponse httpRequest (gcManager client) (consumeGateway state))
      pure $ case response of
        Left err -> Failed ProcessFailure (T.pack (displayException err)) emptyEvidence
        Right outcome -> outcome

consumeGateway :: SessionState -> Response BodyReader -> IO BackendOutcome
consumeGateway state response = do
  let status = statusCode (responseStatus response)
  if status < 200 || status >= 300
    then do
      bytes <- brReadSome (responseBody response) 4096
      let raw = TE.decodeUtf8 (BL.toStrict bytes)
          evidence = emptyEvidence {evidenceRawOutput = raw}
      emitText state RawOutput raw
      pure (Failed (HttpFailure status) ("HTTP " <> T.pack (show status)) evidence)
    else loop (responseBody response) (BS.empty, []) "" Nothing []
  where
    framing = frameAgent sseMarks id
    loop body framingState deltas completed rawEvents = do
      chunk <- brRead body
      let input = if BS.null chunk then Nothing else Just chunk
          (frames, nextState) = runCell framing framingState input
          events = mapMaybe parseSseFrame frames
      if BS.null chunk
        then pure (Failed ProtocolFailure "stream ended before done" (gatewayEvidence deltas rawEvents))
        else do
          stepped <- foldM (stepGateway state) (deltas, completed, rawEvents, Nothing) events
          let (deltas', completed', rawEvents', terminal) = stepped
          case terminal of
            Just outcome -> pure outcome
            Nothing
              | any ((== "done") . fst) events ->
                  let reply = fromMaybe deltas' completed'
                      evidence = gatewayEvidence reply rawEvents'
                   in if T.null (T.strip reply)
                        then pure (Failed EmptyReply "empty reply stream" evidence)
                        else do
                          unless (isJust completed') (emitText state ReplyOutput reply)
                          pure (Completed evidence)
              | otherwise -> loop body nextState deltas' completed' rawEvents'

stepGateway :: SessionState -> (Text, Maybe Text, [Text], Maybe BackendOutcome) -> (Text, Json) -> IO (Text, Maybe Text, [Text], Maybe BackendOutcome)
stepGateway _ (deltas, completed, rawEvents, Just outcome) (event, value) = do
  let raw = event <> " " <> TE.decodeUtf8 (encodeJson value)
  pure (deltas, completed, rawEvents <> [raw], Just outcome)
stepGateway state (deltas, completed, rawEvents, Nothing) (event, value) = do
  let raw = event <> " " <> TE.decodeUtf8 (encodeJson value)
      rawEvents' = rawEvents <> [raw]
  case event of
    "assistant.delta" -> do
      let delta = textAt ["delta"] value
      emitText state DeltaOutput delta
      pure (deltas <> delta, completed, rawEvents', Nothing)
    "assistant.completed" -> do
      let reply = textAt ["content"] value
      emitText state ReplyOutput reply
      pure (deltas, Just reply, rawEvents', Nothing)
    "run.failed" ->
      pure (deltas, completed, rawEvents', Just (Failed ProtocolFailure ("run.failed: " <> textAt ["error"] value) (gatewayEvidence deltas rawEvents')))
    _ -> pure (deltas, completed, rawEvents', Nothing)

gatewayEvidence :: Text -> [Text] -> BackendEvidence
gatewayEvidence reply rawEvents = emptyEvidence {evidenceRawOutput = T.unlines rawEvents, evidenceStopReason = if T.null reply then Nothing else Just "done"}

textAt :: [Text] -> Json -> Text
textAt path value = go path value
  where
    go [] (JString text) = text
    go [] _ = ""
    go (name : rest) (JObject fields) = maybe "" (go rest) (lookup name fields)
    go _ _ = ""

-- ACP -----------------------------------------------------------------------

openAcpSession :: AcpClient -> Int -> FilePath -> IO (Either BackendOutcome Text)
openAcpSession client micros cwd = do
  (response, messages) <- acpInitialize client micros
  case response of
    Nothing -> pure (Left (Failed TimeoutFailure "ACP initialize timed out" (acpEvidence Nothing messages Nothing [])))
    Just value
      | Just err <- jsonField "error" value ->
          pure (Left (Failed ProtocolFailure (TE.decodeUtf8 (encodeJson err)) (acpEvidence Nothing messages Nothing [])))
      | otherwise -> do
          (session, newMessages) <- acpNewSession client micros cwd
          let allMessages = messages <> newMessages
          pure $ case session of
            Just sid -> Right sid
            Nothing
              | not (any (isResponse . classifyFrame) newMessages) ->
                  Left (Failed TimeoutFailure "ACP session/new timed out" (acpEvidence Nothing allMessages Nothing []))
              | otherwise ->
                  Left (Failed ProtocolFailure "ACP session/new returned no session id" (acpEvidence Nothing allMessages Nothing []))
  where
    isResponse (Just (Response _ _)) = True
    isResponse _ = False

resumeAcpSession :: AcpClient -> Int -> FilePath -> Text -> IO (Either BackendOutcome Text)
resumeAcpSession client micros cwd sid = do
  (response, messages) <- acpInitialize client micros
  case response of
    Nothing -> pure (Left (Failed TimeoutFailure "ACP initialize timed out" (acpEvidence Nothing messages Nothing [])))
    Just value
      | Just err <- jsonField "error" value ->
          pure (Left (Failed ProtocolFailure (TE.decodeUtf8 (encodeJson err)) (acpEvidence Nothing messages Nothing [])))
      | otherwise -> do
          (resumed, resumeMessages) <- acpResumeSession client micros sid cwd
          let allMessages = messages <> resumeMessages
              evidence = acpEvidence (Just sid) allMessages Nothing []
          pure $ case resumed of
            Nothing -> Left (Failed TimeoutFailure "ACP session/resume timed out" evidence)
            Just value'
              | Just err <- jsonField "error" value' ->
                  Left (Failed ProtocolFailure (TE.decodeUtf8 (encodeJson err)) evidence)
              | otherwise -> Right sid

runAcp :: Int -> Text -> AcpClient -> Text -> Bool -> Maybe (IORef Bool) -> SessionState -> Active -> BackendRequest -> IO BackendOutcome
runAcp micros systemPrompt client sid cancellable cardState state turn request = do
  cancelled <- readIORef (activeCancelled turn)
  if cancelled
    then pure (Cancelled emptyEvidence {evidenceSessionId = Just sid})
    else do
      includeCard <- maybe (pure True) readIORef cardState
      let prompt =
            (if includeCard && not (T.null systemPrompt) then systemPrompt <> "\n\n" else "")
              <> "User messages:\n"
              <> requestText request
          cancelPrompt =
            acpSendValue
              client
              ( JObject
                  [ ("jsonrpc", JString "2.0"),
                    ("method", JString "session/cancel"),
                    ("params", JObject [("sessionId", JString sid)])
                  ]
              )
          installCancel =
            when cancellable $ do
              writeIORef (activeCancel turn) cancelPrompt
              cancelledNow <- readIORef (activeCancelled turn)
              when cancelledNow cancelPrompt
      result <- try @IOException (acpPromptWithResponseOnSend installCancel client micros sid prompt)
      writeIORef (activeCancel turn) (pure ())
      forM_ cardState $ \fresh -> writeIORef fresh False
      case result of
        Left err -> do
          writeIORef (stateClosed state) True
          catch (closeAcp client) (\(_ :: IOException) -> pure ())
          pure (Failed ProcessFailure (T.pack (displayException err)) emptyEvidence {evidenceSessionId = Just sid})
        Right (response, promptResult) -> do
          let messages = prMessages promptResult
              reply = prReply promptResult
              thoughts = prThoughts promptResult
              stop = prStopReason promptResult
              usage = [json | UsageUpdate json <- prUpdates promptResult]
              evidence = acpEvidence (Just sid) messages stop usage
          emitText state ReplyOutput reply
          emitText state ThoughtOutput thoughts
          forM_ usage $ \json -> emitText state UsageOutput (TE.decodeUtf8 (encodeJson json))
          wasCancelled <- readIORef (activeCancelled turn)
          let outcome = case response of
                Nothing -> Failed TimeoutFailure "ACP prompt timed out; session closed against late responses" evidence
                Just value
                  | Just err <- jsonField "error" value -> Failed ProtocolFailure (TE.decodeUtf8 (encodeJson err)) evidence
                  | wasCancelled -> Cancelled evidence
                  | otherwise -> case stop of
                      Just "cancelled" -> Cancelled evidence
                      Just "max_tokens" -> Exhausted "ACP stop reason max_tokens" evidence
                      Just "max_turn_requests" -> Ceiling "ACP stop reason max_turn_requests" evidence
                      Just "end_turn"
                        | T.null (T.strip reply) -> Failed EmptyReply "ACP end_turn with an empty reply" evidence
                        | otherwise -> Completed evidence
                      Nothing -> Failed ProtocolFailure "ACP prompt response has no stopReason" evidence
                      Just other -> Failed ProtocolFailure ("unsupported ACP stop reason: " <> other) evidence
          case outcome of
            Failed TimeoutFailure _ _ -> do
              -- A timed-out request may still be running remotely. Do not let a
              -- later turn consume its response or reuse its in-flight session.
              writeIORef (stateClosed state) True
              when cancellable (catch cancelPrompt (\(_ :: IOException) -> pure ()))
              catch (closeAcp client) (\(_ :: IOException) -> pure ())
            _ -> pure ()
          pure outcome

acpEvidence :: Maybe Text -> [Json] -> Maybe Text -> [Json] -> BackendEvidence
acpEvidence sid messages stop usage =
  BackendEvidence
    { evidenceExitCode = Nothing,
      evidenceSessionId = sid,
      evidenceRawOutput = T.unlines (map (TE.decodeUtf8 . encodeJson) messages),
      evidenceStopReason = stop,
      evidenceUsage = if null usage then Nothing else Just (T.unlines (map (TE.decodeUtf8 . encodeJson) usage))
    }
