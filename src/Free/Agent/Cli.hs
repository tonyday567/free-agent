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

    -- * Shard adapters
    cliShard,

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
import Data.Text (Text)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (-<.>))
import System.Process (proc, readCreateProcessWithExitCode)

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
      cliStale = \code out ->
        code /= ExitSuccess
          || "No session found matching" `T.isInfixOf` out
          || "Session not found" `T.isInfixOf` out,
      cliScrub = cleanCliOut,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing
    }

-- | One query against a CLI agent.
--
-- First call (or no stored session) runs fresh; subsequent calls resume the
-- stored session id.  A stale session falls back to fresh and records the
-- new id.  Scraped ids are re-persisted on every successful call, so
-- server-side session rotation is followed.
cliQuery :: Cli -> Text -> IO Text
cliQuery cli prompt = do
  mSid <- readStoredSession (cliSessionFile cli)
  case mSid of
    Nothing -> fresh
    Just sid -> do
      (code, raw, routedOut) <- run (Just sid)
      if cliStale cli code raw
        then fresh
        else do
          scrape raw
          pure (cliScrub cli routedOut)
  where
    -- (exit code, raw merged out<>err pre-policy, policy-routed output).
    -- cliStale and scrape act on the raw merged stream: stale notices and
    -- resume hints live on stderr for some CLIs, and 'StderrDrop' must not
    -- hide them — it only filters the reply body.
    run mSid = do
      (code, out, err) <-
        readCreateProcessWithExitCode
          (proc (cliCommand cli) (cliArgv cli prompt mSid))
          (cliStdin cli prompt)
      tee err
      pure (code, T.pack out <> T.pack err, T.pack out <> routed (T.pack err))
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
    fresh = do
      (code, raw, routedOut) <- run Nothing
      when (code /= ExitSuccess) $
        fail
          ( "cliQuery: "
              <> cliCommand cli
              <> " exited "
              <> show code
              <> ": "
              <> T.unpack (T.take 200 raw)
          )
      scrape raw
      pure (cliScrub cli routedOut)
    scrape out =
      for_ (cliSessionId cli out) (writeStoredSession (cliSessionFile cli))

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
      | T.any (== '\x1b') l = False
      | isDecorative l = False
      | otherwise = True
    isDecorative t =
      T.all (\c -> c == ' ' || c == '\r' || c `elem` ("─│┌┐└┘" :: String)) t

-- | A live CLI agent as a list 'Shard'.  Session file and process stay
-- inside @IO@ — apply-only at this boundary.  @who@ is the agent nick
-- (from on emitted posts).
cliShard :: Text -> Cli -> IO (Shard IO [Post Text] [Post Text])
cliShard who cli = queryShard who (cliQuery cli)
