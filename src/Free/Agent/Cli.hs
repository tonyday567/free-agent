{-# LANGUAGE OverloadedStrings #-}

-- | Live CLI agents as opaque shards.
--
-- A 'Cli' recipe describes how to invoke an external CLI agent (hermes, kimi,
-- grok, or any shell command). 'cliDeliverTo' runs one interruptible turn with
-- session resume / stale fallback and explicit transcript causation.
module Free.Agent.Cli
  ( -- * Invocation recipe
    Cli (..),
    StderrPolicy (..),
    kimiCli,
    grokCli,
    dshCli,
    parseSessionId,
    cleanCliOut,

    -- * Query
    cliDeliverTo,

    -- * Shard adapters

    -- * Transcript
    TranscriptOutcome (..),
    TranscriptRecord (..),
    encodeTranscriptLine,

    -- * Generic adapters (re-exported from 'Circuit.Agent.Query')
    queryShard,
    queryShardWith,
    synthShard,
    echoShard,
    runShardIO,
    sessionPrompt,
    replyPosts,
    synthesisPosts,
  )
where

import Circuit.Agent (PostId)
import Circuit.Agent.Query
  ( echoShard,
    queryShard,
    queryShardWith,
    replyPosts,
    runShardIO,
    sessionPrompt,
    synthShard,
    synthesisPosts,
  )
import Control.Concurrent
  ( MVar,
    forkIO,
    modifyMVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    threadDelay,
  )
import Control.Exception (IOException, SomeException, catch, onException, try)
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.Char (chr, ord)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Free.Agent.Delivery (Invocation (..), Turn, TurnResult (..), deliverTo)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (-<.>))
import System.IO (hClose, hPutStr)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, terminateProcess, waitForProcess)
import System.Process qualified as Process

-- | Invocation recipe for a CLI agent.
--
-- Everything a session needs is plain data; there are no laws here beyond
-- what the CLI itself honours.
data Cli = Cli
  { -- | The executable (e.g. @"hermes"@, @"/bin/sh"@).
    cliCommand :: FilePath,
    -- | Full argv (excluding the command) for one query, given the prompt
    -- and any stored session id ('Nothing' = fresh session).
    cliArgv :: Text -> Maybe Text -> [String],
    -- | Stdin for the process, from the prompt ('const ""' for argv-only CLIs).
    cliStdin :: Text -> String,
    -- | Where the session id is persisted between calls.
    cliSessionFile :: FilePath,
    -- | Scrape a session id from CLI output.  @const Nothing@ for CLIs
    -- without sessions; no session file is then ever written.
    cliSessionId :: Text -> Maybe Text,
    -- | Is this (exit code, output) pair a stale-session response?
    cliStale :: ExitCode -> Text -> Bool,
    -- | Noise filter applied to output before it becomes a reply body.
    cliScrub :: Text -> Text,
    -- | What to do with the process's stderr channel.
    cliStderr :: StderrPolicy,
    -- | Optional tee: raw stderr appended to this log file on every call,
    -- regardless of the policy (interiority stays searchable, never
    -- silently dropped).
    cliStderrTee :: Maybe FilePath
  }

-- | stderr routing for a CLI agent's output channels.
--
-- Precedent: @Muster.Connector@ posts @-- stdout --@ \/ @-- stderr --@
-- marked sections; 'StderrMark' is the in-body equivalent.
data StderrPolicy
  = -- | Discard stderr (use with 'cliStderrTee' to keep a log).
    StderrDrop
  | -- | Concatenate stdout and stderr (the historical behaviour).
    StderrMerge
  | -- | Append stderr after a @-- stderr --@ section marker.
    StderrMark
  deriving (Eq, Show)

-- | Recipe for the kimi CLI: @kimi -p \<prompt\> [-m \<model\>] [--provider \<provider\>] [-r \<sid\>]@,
-- text output.  kimi prints a plain-text resume hint line, so scraping and
-- scrubbing are line-oriented — no JSON needed.  Note: kimi exits 0 even
-- when the prompt fails (and @--auto@ cannot combine with @-p@), so stale
-- detection is output-based.
--
-- stderr (thinking / tool progress / notices) is dropped from the reply
-- but teed raw to @\<sessionFile\>.stderr.log@ — interiority stays
-- searchable, never silently dropped.
kimiCli :: Maybe Text -> Maybe Text -> FilePath -> Cli
kimiCli model provider sessionFile =
  Cli
    { cliCommand = "kimi",
      cliArgv = \prompt mSid ->
        ["-p", T.unpack prompt]
          <> maybe [] (\m -> ["-m", T.unpack m]) model
          <> maybe [] (\p -> ["--provider", T.unpack p]) provider
          <> maybe [] (\sid -> ["-r", T.unpack sid]) mSid,
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = kimiSessionId,
      cliStale = \_ out ->
        "Session \"" `T.isInfixOf` out && "not found" `T.isInfixOf` out,
      cliScrub = kimiText,
      cliStderr = StderrDrop,
      -- Interiority log: NAME.sid -> NAME.stderr.log
      cliStderrTee = Just (sessionFile -<.> "stderr.log")
    }

-- | Scrape the @To resume this session: kimi -r \<id\>@ hint line.
kimiSessionId :: Text -> Maybe Text
kimiSessionId out =
  case filter ("To resume this session:" `T.isPrefixOf`) (T.lines out) of
    (l : _) -> listToMaybe (reverse (T.words l))
    [] -> Nothing

-- | Drop the resume-hint line; keep the reply text.
kimiText :: Text -> Text
kimiText =
  T.strip
    . T.unlines
    . filter (not . ("To resume this session:" `T.isPrefixOf`))
    . T.lines

-- | Recipe for the grok CLI: @grok -p \<prompt\> --output-format json
-- [--resume \<sid\>]@.  Plain output carries no session id, so the JSON
-- format is used and the @text\/@sessionId@ fields are extracted.
grokCli :: Maybe Text -> FilePath -> Cli
grokCli model sessionFile =
  Cli
    { cliCommand = "grok",
      cliArgv = \prompt mSid ->
        ["-p", T.unpack prompt, "--output-format", "json"]
          <> maybe [] (\m -> ["-m", T.unpack m]) model
          <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = jsonField "sessionId",
      cliStale = \code out ->
        code /= ExitSuccess || "Failed to restore session" `T.isInfixOf` out,
      cliScrub = grokText,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing
    }

-- | Reply text is the JSON @text@ field, unescaped; if there is no such
-- field (an error page), keep the whole output so failures stay visible.
grokText :: Text -> Text
grokText out = maybe (T.strip out) unescapeJson (jsonField "text" out)

-- | Best-effort extraction of a top-level @"key": "value"@ string field
-- (space after the colon optional; escapes respected).  Not a JSON parser —
-- good enough for one-line NDJSON records and flat pretty-printed objects.
jsonField :: Text -> Text -> Maybe Text
jsonField key src =
  case T.breakOn pat src of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise -> jsonString (T.dropWhile (== ' ') (T.drop (T.length pat) rest))
  where
    pat = "\"" <> key <> "\":"

-- | Read a JSON string body after the opening quote, honouring backslash
-- escapes; 'Nothing' if the opening quote is missing or the string is
-- unterminated.
jsonString :: Text -> Maybe Text
jsonString t0 = case T.uncons t0 of
  Just ('"', t) -> go t []
  _ -> Nothing
  where
    go rest acc = case T.uncons rest of
      Nothing -> Nothing
      Just ('\\', r) -> case T.uncons r of
        Just (c, r') -> go r' (c : '\\' : acc)
        Nothing -> Nothing
      Just ('"', _) -> Just (T.pack (reverse acc))
      Just (c, r') -> go r' (c : acc)

-- | Unescape the common JSON string escapes; unknown escapes are kept
-- literally.  Best-effort, not a full @\\u@ decoder.
unescapeJson :: Text -> Text
unescapeJson t = case T.uncons t of
  Nothing -> t
  Just ('\\', r) -> case T.uncons r of
    Just (c, r') -> case esc c of
      Just u -> T.cons u (unescapeJson r')
      Nothing -> T.cons '\\' (T.cons c (unescapeJson r'))
    Nothing -> "\\"
  Just (c, r) -> T.cons c (unescapeJson r)
  where
    esc 'n' = Just '\n'
    esc 'r' = Just '\r'
    esc 't' = Just '\t'
    esc '"' = Just '"'
    esc '\\' = Just '\\'
    esc '/' = Just '/'
    esc _ = Nothing

-- | Recipe for the dsh CLI (the local ollama seat):
-- @dsh headless --patch BASE [--patch MODEL] --json [--session-id \<sid\>] TASK@.
--
-- The task rides the argv — @CARD\n\nUser message:\n\<prompt\>@ on a fresh
-- session, @User message:\n\<prompt\>@ on a resumed one: a resumed session
-- already carries the card, and re-prepending would burn the 16k context
-- window by duplication.  The "User message:" lead also keeps the task
-- safely non-dash-leading for the CLI's own argument parser.
--
-- Output is NDJSON: the session id is scraped from the
-- @{"type":"session",…}@ event; the reply is scrubbed to the final
-- message's text — the raw stream stays in the transcript sink.
dshCli :: Maybe Text -> FilePath -> Maybe FilePath -> FilePath -> Cli
dshCli mCard basePatch mModelPatch sessionFile =
  Cli
    { cliCommand = "dsh",
      cliArgv = \prompt mSid ->
        ["headless", "--patch", basePatch]
          <> maybe [] (\p -> ["--patch", p]) mModelPatch
          <> ["--json"]
          <> maybe [] (\sid -> ["--session-id", T.unpack sid]) mSid
          <> [T.unpack (taskFor mSid prompt)],
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = dshSessionId,
      cliStale = \_ out -> "does not exist; omit --session-id" `T.isInfixOf` out,
      cliScrub = dshText,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing
    }
  where
    taskFor mSid prompt = case (mCard, mSid) of
      (Just card, Nothing) -> card <> "\n\nUser message:\n" <> prompt
      _ -> "User message:\n" <> prompt

-- | Scrape the session id from the NDJSON @session@ event.
dshSessionId :: Text -> Maybe Text
dshSessionId out =
  case filter ("\"type\":\"session\"" `T.isInfixOf`) (T.lines out) of
    (l : _) -> jsonField "sessionId" l
    [] -> Nothing

-- | The reply is the last NDJSON @final@ event's text, unescaped; if no
-- final event exists (a crashed run), keep the whole stream visible.
dshText :: Text -> Text
dshText out =
  maybe
    (T.strip out)
    unescapeJson
    ( listToMaybe
        ( reverse
            ( mapMaybe
                (jsonField "text")
                (filter ("\"type\":\"final\"" `T.isInfixOf`) (T.lines out))
            )
        )
    )

-- | Scrape a @session_id:@ line from CLI output.
parseSessionId :: Text -> Maybe Text
parseSessionId out =
  case filter ("session_id:" `T.isPrefixOf`) (T.lines out) of
    (line : _) ->
      let sid = T.strip (T.drop (T.length "session_id:") line)
       in if T.null sid then Nothing else Just sid
    [] -> Nothing

-- | Maximum retries for transient failures when resuming a session.
-- A transient failure (non-zero exit, not stale) is retried with the
-- same session id before giving up.
cliMaxRetries :: Int
cliMaxRetries = 3

data CliControl = CliControl
  { cliActive :: MVar Bool,
    cliCancelled :: IORef Bool,
    cliProcess :: IORef (Maybe ProcessHandle)
  }

newCliControl :: IO CliControl
newCliControl =
  CliControl
    <$> newMVar True
    <*> newIORef False
    <*> newIORef Nothing

cancelCli :: CliControl -> IO Bool
cancelCli control =
  modifyMVar (cliActive control) $ \isActive ->
    if not isActive
      then pure (False, False)
      else do
        writeIORef (cliCancelled control) True
        mProcess <- readIORef (cliProcess control)
        catch
          (for_ mProcess terminateProcess)
          (\(_ :: IOException) -> pure ())
        pure (False, True)

finishCli :: Maybe CliControl -> IO ()
finishCli Nothing = pure ()
finishCli (Just control) =
  modifyMVar_ (cliActive control) (const (pure False))

isCliCancelled :: Maybe CliControl -> IO Bool
isCliCancelled Nothing = pure False
isCliCancelled (Just control) = readIORef (cliCancelled control)

-- | Deliver one bus-caused prompt without publishing its response. The
-- existing transcript record carries the supplied post id directly.
cliDeliverTo :: Cli -> FilePath -> PostId -> Text -> IO (Turn Text)
cliDeliverTo cli transcriptPath =
  deliverTo $ \postId prompt -> do
    control <- newCliControl
    pure
      Invocation
        { runInvocation = cliQueryWithDelivery (postId, transcriptPath) (Just control) cli prompt,
          cancelInvocation = cancelCli control
        }

-- First call (or no stored session) runs fresh; subsequent calls resume the
-- stored session id. A stale session falls back to fresh and records the new
-- id. Scraped ids are re-persisted on every settled call, so server-side
-- session rotation is followed.
cliQueryWithDelivery :: (PostId, FilePath) -> Maybe CliControl -> Cli -> Text -> IO (TurnResult Text)
cliQueryWithDelivery delivery mControl cli prompt = do
  t0 <- getCurrentTime
  mSid <- readStoredSession (cliSessionFile cli)
  case mSid of
    Nothing -> fresh t0
    Just sid -> do
      (code, raw, routedOut, elapsed) <- run t0 (Just sid)
      cancelled <- isCliCancelled mControl
      if cancelled
        then cancelledTurn t0 code raw elapsed
        else
          if cliStale cli code raw
            then fresh t0
            else
              if code /= ExitSuccess
                then retryWithSession t0 sid 1
                else settle t0 code raw routedOut elapsed
  where
    retryWithSession t0' sid attempt
      | attempt > cliMaxRetries = do
          finishCli mControl
          fail
            ( "cliQuery: "
                <> cliCommand cli
                <> " exited non-zero "
                <> show cliMaxRetries
                <> " times; last attempt with session "
                <> T.unpack (T.take 20 sid)
            )
      | otherwise = do
          threadDelay (100000 * attempt)
          (code', raw', routedOut', elapsed') <- run t0' (Just sid)
          cancelled <- isCliCancelled mControl
          if cancelled
            then cancelledTurn t0' code' raw' elapsed'
            else
              if cliStale cli code' raw'
                then fresh t0'
                else
                  if code' /= ExitSuccess
                    then retryWithSession t0' sid (attempt + 1)
                    else settle t0' code' raw' routedOut' elapsed'
    run t0' mSid = do
      cancelled <- isCliCancelled mControl
      if cancelled
        then do
          t1 <- getCurrentTime
          pure (ExitFailure 130, "", "", elapsedMs t0' t1)
        else do
          created <-
            createProcess
              (proc (cliCommand cli) (cliArgv cli prompt mSid))
                { Process.std_in = CreatePipe,
                  Process.std_out = CreatePipe,
                  Process.std_err = CreatePipe
                }
          case created of
            (Just stdinH, Just stdoutH, Just stderrH, process) ->
              flip onException (terminateProcess process >> void (waitForProcess process)) $ do
                for_ mControl $ \(CliControl _ cancelledRef processRef) -> do
                  writeIORef processRef (Just process)
                  cancelledNow <- readIORef cancelledRef
                  when cancelledNow (terminateProcess process)
                outVar <- newEmptyMVar
                errVar <- newEmptyMVar
                _ <- forkIO (BS.hGetContents stdoutH >>= putMVar outVar)
                _ <- forkIO (BS.hGetContents stderrH >>= putMVar errVar)
                _ <-
                  forkIO $
                    catch
                      (hPutStr stdinH (cliStdin cli prompt) >> hClose stdinH)
                      (\(_ :: IOException) -> pure ())
                code <- waitForProcess process
                out <- TE.decodeUtf8 <$> readMVar outVar
                err <- TE.decodeUtf8 <$> readMVar errVar
                for_ mControl $ \(CliControl _ _ processRef) -> writeIORef processRef Nothing
                t1 <- getCurrentTime
                tee err
                pure (code, out <> err, out <> routed err, elapsedMs t0' t1)
            _ -> fail "cliQuery: createProcess did not return all requested pipes"
    elapsedMs t0' t1 = floor (realToFrac (diffUTCTime t1 t0') * (1000 :: Double))
    tee err =
      for_ (cliStderrTee cli) $ \path -> do
        createDirectoryIfMissing True (takeDirectory path)
        TIO.appendFile path err
    routed err = case cliStderr cli of
      StderrDrop -> ""
      StderrMerge -> err
      StderrMark
        | T.null (T.strip err) -> ""
        | otherwise -> "\n-- stderr --\n" <> err
    fresh t0' = do
      (code, raw, routedOut, elapsed) <- run t0' Nothing
      cancelled <- isCliCancelled mControl
      if cancelled
        then cancelledTurn t0' code raw elapsed
        else do
          when (code /= ExitSuccess) $ do
            finishCli mControl
            fail
              ( "cliQuery: "
                  <> cliCommand cli
                  <> " exited "
                  <> show code
                  <> ": "
                  <> T.unpack (T.take 200 raw)
              )
          settle t0' code raw routedOut elapsed
    settle t0' code raw routedOut elapsed = do
      finishCli mControl
      let mSid' = cliSessionId cli raw
      for_ mSid' (writeStoredSession (cliSessionFile cli))
      writeTranscript TranscriptSettled t0' code raw elapsed mSid'
      pure (TurnResponse (cliScrub cli routedOut))
    cancelledTurn t0' code raw elapsed = do
      let mSid' = cliSessionId cli raw
      for_ mSid' (writeStoredSession (cliSessionFile cli))
      writeTranscript TranscriptCancelled t0' code raw elapsed mSid'
      pure TurnCancelled
    writeTranscript outcome t0' code raw elapsed mSid' = do
      let writeAt pid path = do
            let rec =
                  TranscriptRecord
                    { trPostId = pid,
                      trTimestamp = t0',
                      trExitCode = case code of
                        ExitSuccess -> 0
                        ExitFailure n -> n,
                      trElapsedMs = elapsed,
                      trOutcome = outcome,
                      trSessionId = mSid',
                      trRaw = raw
                    }
                line = encodeTranscriptLine rec <> "\n"
            catch
              ( do
                  createDirectoryIfMissing True (takeDirectory path)
                  TIO.appendFile path line
              )
              (\(_ :: SomeException) -> pure ())
      let (pid, path) = delivery
      writeAt (fromIntegral pid) path

readStoredSession :: FilePath -> IO (Maybe Text)
readStoredSession path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      res <- try @SomeException (TIO.readFile path)
      pure $ case res of
        Left _ -> Nothing
        Right t ->
          let sid = T.strip t
           in if T.null sid then Nothing else Just sid

writeStoredSession :: FilePath -> Text -> IO ()
writeStoredSession path sid = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path sid

-- | Hermes-flavoured TUI noise filter: drops session chatter, decorative
-- rules, and ANSI lines; keeps plain reply text with no trailing newline.
cleanCliOut :: Text -> Text
cleanCliOut =
  T.strip
    . T.unlines
    . filter keep
    . map T.strip
    . T.lines
  where
    keep l
      | T.null l = False
      | "(empty)" == l = False
      | "↪" `T.isPrefixOf` l = False
      | "kimi version" `T.isPrefixOf` l = False
      | "To resume this session:" `T.isPrefixOf` l = False
      | "session_id:" `T.isPrefixOf` l = False
      | "Warning:" `T.isPrefixOf` l = False
      | "Resumed session" `T.isInfixOf` l = False
      | "Reached maximum" `T.isInfixOf` l = False
      | "Requesting summary" `T.isInfixOf` l = False
      | "No session found matching" `T.isInfixOf` l = False
      | "Use 'hermes sessions list'" `T.isInfixOf` l = False
      | "Resume this session with:" `T.isInfixOf` l = False
      | "Shutting down" `T.isInfixOf` l = False
      | "Session:" `T.isPrefixOf` l = False
      | "Duration:" `T.isPrefixOf` l = False
      | "Messages:" `T.isPrefixOf` l = False
      | "⚕" `T.isPrefixOf` l = False
      | "❯" `T.isPrefixOf` l = False
      | "Query:" `T.isPrefixOf` l = False
      | "Initializing agent" `T.isInfixOf` l = False
      | "┊" `T.isPrefixOf` l = False
      | "hermes --resume" `T.isInfixOf` l = False
      | "hermes chat" `T.isInfixOf` l = False
      | T.any (== '\x1b') l = False
      | isDecorative l = False
      | otherwise = True
    isDecorative t =
      T.all
        ( \c ->
            c == ' '
              || c == '\r'
              || c
                `elem` ("─│┌┐└┘╭╮╰╯" :: String)
        )
        t

data TranscriptOutcome = TranscriptSettled | TranscriptCancelled
  deriving (Eq, Show)

-- | One transcript record, as JSONL appended to the transcript log.
data TranscriptRecord = TranscriptRecord
  { trPostId :: Int,
    trTimestamp :: UTCTime,
    trExitCode :: Int,
    trElapsedMs :: Int,
    trOutcome :: TranscriptOutcome,
    trSessionId :: Maybe Text,
    trRaw :: Text
  }

-- | Encode a transcript record as a single JSON line (no trailing newline).
encodeTranscriptLine :: TranscriptRecord -> Text
encodeTranscriptLine r =
  "{"
    <> kv "post_id" (T.pack (show (trPostId r)))
    <> ","
    <> kv "ts" ("\"" <> escapeText (T.pack (show (trTimestamp r))) <> "\"")
    <> ","
    <> kv "exit_code" (T.pack (show (trExitCode r)))
    <> ","
    <> kv "elapsed_ms" (T.pack (show (trElapsedMs r)))
    <> ","
    <> kv "outcome" (case trOutcome r of TranscriptSettled -> "\"settled\""; TranscriptCancelled -> "\"cancelled\"")
    <> ","
    <> kv "session_id" (maybe "null" (\t -> "\"" <> escapeText t <> "\"") (trSessionId r))
    <> ","
    <> kv "raw" ("\"" <> escapeText (trRaw r) <> "\"")
    <> "}"
  where
    kv k v = "\"" <> k <> "\":" <> v

-- | Minimal JSON string escaping: backslash, double-quote, control chars.
escapeText :: Text -> Text
escapeText = T.concatMap escChar
  where
    escChar c
      | c == '\\' = "\\\\"
      | c == '\"' = "\\\""
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | c == '\t' = "\\t"
      | c < '\x20' = "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (fromEnum c) ""))
      | otherwise = T.singleton c
    showHex n s = case n `divMod` 16 of
      (0, d) -> hexDigit d : s
      (q, d) -> showHex q (hexDigit d : s)
    hexDigit d
      | d < 10 = chr (ord '0' + d)
      | otherwise = chr (ord 'a' + d - 10)
