{-# LANGUAGE OverloadedStrings #-}

-- | Live CLI agents as opaque shards.
--
-- A 'Cli' recipe describes how to invoke an external CLI agent (hermes, kimi,
-- grok, or any shell command). 'cliQuery' runs the recipe with session
-- resume / stale fallback; 'cliShard' seats it as a 'Circuit.Agent.Shard'.
module Free.Agent.Cli
  ( -- * Invocation recipe
    Cli (..),
    StderrPolicy (..),
    hermesCli,
    kimiCli,
    grokCli,
    parseSessionId,
    cleanCliOut,

    -- * Query
    cliQuery,
    cliQueryBS,

    -- * Shard adapters
    cliShard,

    -- * Transcript
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

import Circuit.Agent (Post, Shard)
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
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, try)
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.Foldable (for_)
import Data.IORef (IORef, readIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.IO.Handle (hGetContents)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (-<.>))
import System.IO (hClose)
import System.Process
  ( CreateProcess (std_err, std_out),
    StdStream (CreatePipe),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
  )

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
    cliStderrTee :: Maybe FilePath,
    -- | Optional transcript sink: @(post_id_ref, transcript_jsonl_path)@.
    -- The 'IORef' carries the current post id, set externally before each
    -- query.  One JSONL record is appended per invocation; failure to write
    -- is silent (tee failure is never fatal).
    cliTranscript :: Maybe (IORef Int, FilePath)
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
      cliStderrTee = Just (sessionFile -<.> "stderr.log"),
      cliTranscript = Nothing
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
      cliStderrTee = Nothing,
      cliTranscript = Nothing
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

-- | Scrape a @session_id:@ line from CLI output.
parseSessionId :: Text -> Maybe Text
parseSessionId out =
  case filter ("session_id:" `T.isPrefixOf`) (T.lines out) of
    (line : _) ->
      let sid = T.strip (T.drop (T.length "session_id:") line)
       in if T.null sid then Nothing else Just sid
    [] -> Nothing

-- | Recipe for the hermes CLI: @hermes chat -q \<prompt\> … --resume \<sid\>@.
hermesCli :: Maybe Text -> Maybe Text -> FilePath -> Cli
hermesCli model provider sessionFile =
  Cli
    { cliCommand = "hermes",
      cliArgv = \prompt mSid ->
        ["chat", "-q", T.unpack prompt]
          <> maybe [] (\m -> ["-m", T.unpack m]) model
          <> maybe [] (\p -> ["--provider", T.unpack p]) provider
          <> ["--yolo", "-Q", "--max-turns", "90"]
          <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = parseSessionId,
      cliStale = \_code out ->
        "No session found matching" `T.isInfixOf` out
          || "Session not found" `T.isInfixOf` out,
      cliScrub = cleanCliOut,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing,
      cliTranscript = Nothing
    }

-- | Maximum retries for transient failures when resuming a session.
-- A transient failure (non-zero exit, not stale) is retried with the
-- same session id before giving up.
cliMaxRetries :: Int
cliMaxRetries = 3

-- | One query against a CLI agent.
-- First call (or no stored session) runs fresh; subsequent calls resume the
-- stored session id.  A stale session falls back to fresh and records the
-- new id.  Scraped ids are re-persisted on every successful call, so
-- server-side session rotation is followed.
cliQuery :: Cli -> Text -> IO Text
cliQuery cli prompt = do
  t0 <- getCurrentTime
  mSid <- readStoredSession (cliSessionFile cli)
  case mSid of
    Nothing -> fresh t0
    Just sid -> do
      (code, raw, routedOut, elapsed) <- run t0 (Just sid)
      if cliStale cli code raw
        then fresh t0
        else
          if code /= ExitSuccess
            then retryWithSession t0 sid 1
            else do
              let mSid' = cliSessionId cli raw
              for_ mSid' (writeStoredSession (cliSessionFile cli))
              writeTranscript t0 code raw elapsed mSid'
              pure (cliScrub cli routedOut)
  where
    -- Retry a transient failure with the same session id.  After
    -- 'cliMaxRetries' attempts without success the error is propagated
    -- upward to the seat loop (which posts 🔴 to pitboss).
    retryWithSession t0' sid attempt
      | attempt > cliMaxRetries =
          fail
            ( "cliQuery: "
                <> cliCommand cli
                <> " exited non-zero "
                <> show cliMaxRetries
                <> " times; last attempt with session "
                <> T.unpack (T.take 20 sid)
            )
      | otherwise = do
          -- Linear back-off: 100ms * attempt.
          threadDelay (100000 * attempt)
          (code', raw', routedOut', elapsed') <- run t0' (Just sid)
          if cliStale cli code' raw'
            then fresh t0'
            else
              if code' /= ExitSuccess
                then retryWithSession t0' sid (attempt + 1)
                else do
                  let mSid' = cliSessionId cli raw'
                  for_ mSid' (writeStoredSession (cliSessionFile cli))
                  writeTranscript t0' code' raw' elapsed' mSid'
                  pure (cliScrub cli routedOut')
    -- (exit code, raw merged out<>err pre-policy, policy-routed output, elapsed ms).
    -- cliStale and scrape act on the raw merged stream: stale notices and
    -- resume hints live on stderr for some CLIs, and 'StderrDrop' must not
    -- hide them — it only filters the reply body.
    run t0' mSid = do
      (code, out, err) <-
        readCreateProcessWithExitCode
          (proc (cliCommand cli) (cliArgv cli prompt mSid))
          (cliStdin cli prompt)
      t1 <- getCurrentTime
      let elapsed = floor (realToFrac (diffUTCTime t1 t0') * (1000 :: Double))
      tee err
      pure (code, T.pack out <> T.pack err, T.pack out <> routed (T.pack err), elapsed)
    tee err =
      for_ (cliStderrTee cli) $ \path -> do
        createDirectoryIfMissing True (takeDirectory path)
        TIO.appendFile path (T.pack err)
    routed err = case cliStderr cli of
      StderrDrop -> ""
      StderrMerge -> err
      StderrMark
        | T.null (T.strip err) -> ""
        | otherwise -> "\n-- stderr --\n" <> err
    fresh t0' = do
      (code, raw, routedOut, elapsed) <- run t0' Nothing
      when (code /= ExitSuccess) $
        fail
          ( "cliQuery: "
              <> cliCommand cli
              <> " exited "
              <> show code
              <> ": "
              <> T.unpack (T.take 200 raw)
          )
      let mSid' = cliSessionId cli raw
      for_ mSid' (writeStoredSession (cliSessionFile cli))
      writeTranscript t0' code raw elapsed mSid'
      pure (cliScrub cli routedOut)
    writeTranscript t0' code raw elapsed mSid' =
      for_ (cliTranscript cli) $ \(ref, path) -> do
        pid <- readIORef ref
        let rec =
              TranscriptRecord
                { trPostId = pid,
                  trTimestamp = t0',
                  trExitCode = case code of
                    ExitSuccess -> 0
                    ExitFailure n -> n,
                  trElapsedMs = elapsed,
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

-- | Like 'cliQuery' but returns raw stdout as 'BS.ByteString' before
-- any decoding or filtering.  Uses 'CreatePipe' to read bytes directly
-- from the process rather than going through the locale-aware 'String'
-- path of 'readCreateProcessWithExitCode'.
cliQueryBS :: Cli -> Text -> IO BS.ByteString
cliQueryBS cli prompt = do
  let cp =
        (proc (cliCommand cli) (cliArgv cli prompt Nothing))
          { std_out = CreatePipe,
            std_err = CreatePipe
          }
  (_, Just outH, Just errH, ph) <- createProcess cp
  -- Read the error handle fully so the process doesn't block.
  _ <- BS.hGetContents errH
  raw <- BS.hGetContents outH
  code <- waitForProcess ph
  hClose errH
  hClose outH
  if code /= ExitSuccess
    then fail ("cliQueryBS: " <> cliCommand cli <> " exited " <> show code)
    else pure raw

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

-- | One transcript record, as JSONL appended to the transcript log.
data TranscriptRecord = TranscriptRecord
  { trPostId :: Int,
    trTimestamp :: UTCTime,
    trExitCode :: Int,
    trElapsedMs :: Int,
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

-- | A live CLI agent as a list 'Shard'.  Session file and process stay
-- inside @IO@ — apply-only at this boundary.  @who@ is the agent nick
-- (from on emitted posts).
cliShard :: Text -> Cli -> IO (Shard IO [Post Text] [Post Text])
cliShard who cli = queryShard who (cliQuery cli)
