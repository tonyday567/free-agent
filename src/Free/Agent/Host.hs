{-# LANGUAGE OverloadedStrings #-}

-- | One-shot host algebra over circuits-agent seats.
--
-- A 'Host' is a named effectful seat that receives arguments drawn from the
-- body of an incoming post and produces lines of output.  Live CLI agents
-- are 'Cli' recipes from 'Circuit.Agent.Cli' — session management (scrape,
-- resume, stale fallback) lives there, not here.
module Free.Agent.Host
  ( BodyMode (..),
    Host (..),
    BareConfig (..),
    mkHost,
    hostShard,
    processHost,
    cliHost,
    hermesHostBatch,
    hermesCli,
    kimiHost,
    dshHost,
    bareHost,
    defaultBareConfig,
  )
where

import Circuit.Agent (Post (..), mkPost)
import Circuit.Agent.Tensor (AgentShard, ioShard)
import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliQuery, dshCli, kimiCli, parseSessionId)
import Free.Agent.Json (objLookup)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.FilePath (takeDirectory, (</>))
import System.Process (proc, readCreateProcessWithExitCode, readProcess)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Free.Agent.Host
-- >>> import Circuit.Agent

-- | How to turn a post body into arguments for 'hostRun'.
data BodyMode
  = -- | Split on whitespace (default).
    BodyWords
  | -- | Split on newlines.
    BodyLines
  | -- | Pass the whole body as a single argument.
    BodyWhole
  deriving (Show, Eq)

-- | A one-shot host seat.
--
-- The host receives arguments derived from the body of an incoming post and
-- produces output lines.  The caller decides how to turn those lines back into
-- posts; 'hostShard' uses the default mapping (reply to sender).
data Host = Host
  { -- | Name used as the 'from' field of reply posts.
    hostName :: Text,
    -- | How to split the incoming post body before calling 'hostRun'.
    hostBodyMode :: BodyMode,
    -- | Run the host on the prepared arguments.
    hostRun :: [Text] -> IO [Text]
  }

-- | Smart constructor with the default 'BodyWords' mode.
mkHost :: Text -> ([Text] -> IO [Text]) -> Host
mkHost name f = Host name BodyWords f

-- | Split a post body according to the host's 'BodyMode'.
bodyArgs :: BodyMode -> Text -> [Text]
bodyArgs BodyWords = T.words
bodyArgs BodyLines = T.lines
bodyArgs BodyWhole = (: [])

-- | Turn a host into a stateful shard that consumes every committed post.
--
-- The shard remembers the committed posts in its state.  On emit it runs the
-- host on each post's body (prepared by 'hostBodyMode'), in order, and emits
-- one reply post per output line per input post.  Each reply is addressed back
-- to the sender of its input post.
--
-- Note: a generic host has no access to stamped log ids, so emitted replies
-- carry no thread edge.  Callers that need provenance should thread by id
-- outside the host.
hostShard :: Host -> AgentShard [Post Text] [Post Text]
hostShard h =
  ioShard $
    fmap concat
      . traverse
        ( \p -> do
            outs <- hostRun h (bodyArgs (hostBodyMode h) (body p))
            pure [mkPost (hostName h) [from p] o | o <- outs]
        )

-- | A host backed by an external process.
--
-- The command receives the fixed @args@ followed by the prepared post body
-- (one argument when 'BodyWhole', whitespace-split words by default).  Output
-- lines become reply posts.  Uses 'Moore.Process.readProcess'.
processHost ::
  -- | Host name.
  Text ->
  -- | Command to run.
  FilePath ->
  -- | Fixed command arguments.
  [String] ->
  Host
processHost name cmd args =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = \ws -> do
        out <- readProcess cmd (args ++ map T.unpack ws) ""
        pure (map T.pack (lines out))
    }

-- | A host backed by a live CLI agent recipe ('Circuit.Agent.Cli').
--
-- Each prepared body becomes one 'cliQuery'; session scrape/resume/stale
-- fallback happens inside the recipe.  One reply text per body (multi-line
-- bodies and replies are preserved).
cliHost ::
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | Invocation recipe.
  Cli ->
  Host
cliHost name cli =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = traverse (cliQuery cli)
    }

-- | Kimi host on the shared 'Cli' seat.
--
-- Runs @kimi -p@ per body (see 'kimiCli'), with optional @-m <model>@,
-- @--provider <provider>@, and session resume via @sessionFile@.  A stale
-- session falls back to fresh.
--
-- When @mTranscript@ is 'Just', one JSONL transcript record is appended to
-- the given file per invocation.  The caller writes the post id to the
-- 'IORef' before each call; see 'Free.Agent.Cli.cliQuery' for details.
kimiHost ::
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | Model name passed to @kimi -m@, if any.
  Maybe Text ->
  -- | Provider passed to @kimi --provider@, if any.
  Maybe Text ->
  -- | Session file for cross-call context.
  FilePath ->
  -- | Optional transcript sink.
  Maybe (IORef Int, FilePath) ->
  Host
kimiHost name model provider sessionFile mTranscript =
  cliHost name (kimiCli model provider sessionFile) {cliTranscript = mTranscript}

-- | DSH host on the local deepseek-harness seat.
--
-- Rides the shared 'Cli' recipe ('dshCli'): runs
-- @dsh headless --patch BASE [--patch MODEL] --json [--session-id SID] TASK@
-- per body, with session resume via the session file (a stale session
-- falls back to fresh), the seat card as the fresh-session preamble, and
-- the NDJSON stream scrubbed to the final text for the reply.  The
-- transcript sink records one JSONL line per invocation, raw stream
-- preserved.
dshHost ::
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | Seat card: the fresh-session preamble, if any.
  Maybe Text ->
  -- | Base patch file (e.g. @etc/dsh/ollama.patch.yml@).
  FilePath ->
  -- | Optional model-override patch file.
  Maybe FilePath ->
  -- | Session file for cross-call context.
  FilePath ->
  -- | Optional transcript sink. The caller writes the post id to the 'IORef'
  -- before each call.
  Maybe (IORef Int, FilePath) ->
  Host
dshHost name mCard basePatch mModelPatch sessionFile mTranscript =
  cliHost name (dshCli mCard basePatch mModelPatch sessionFile) {cliTranscript = mTranscript}

-- | Shared CLI recipe for hermes backends.
-- When @cliTranscript@ is 'Just', one JSONL record is appended per invocation.
hermesCli :: Maybe Text -> Maybe Text -> Bool -> FilePath -> Maybe (IORef Int, FilePath) -> Cli
hermesCli model provider yolo sessionFile mTranscript =
  Cli
    { cliCommand = "hermes",
      cliArgv = \prompt mSid ->
        ["chat", "-q", T.unpack prompt]
          <> maybe [] (\m -> ["-m", T.unpack m]) model
          <> maybe [] (\p -> ["--provider", T.unpack p]) provider
          <> (if yolo then ["--yolo", "-Q"] else [])
          <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = parseSessionId,
      cliStale = \_ out ->
        "No session found matching" `T.isInfixOf` out
          || "Session not found" `T.isInfixOf` out,
      cliScrub = cleanCliOut,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing,
      cliTranscript = mTranscript
    }

-- | Batch variant of 'hermesHost'. Joins all post bodies into a single user
-- message and makes one @hermes chat -q@ call, returning one reply. Use when
-- multiple posts may arrive in a single wake cycle and the agent should see
-- them as a combined conversation turn.
hermesHostBatch ::
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | System prompt text prepended to every body.
  Text ->
  -- | Model name passed to @hermes -m@, if any.
  Maybe Text ->
  -- | Provider passed to @hermes --provider@, if any.
  Maybe Text ->
  -- | Pass @--yolo -Q@ to hermes.
  Bool ->
  -- | Session file for cross-call context.
  FilePath ->
  -- | Optional transcript sink.
  Maybe (IORef Int, FilePath) ->
  Host
hermesHostBatch name systemPrompt model provider yolo sessionFile mTranscript =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = \bodies -> do
        let userMessage = T.unlines bodies
            prompt = systemPrompt <> "\n\nUser messages:\n" <> userMessage
        rsp <- cliQuery cli prompt
        pure [cleanCliOut rsp]
    }
  where
    cli = hermesCli model provider yolo sessionFile mTranscript

-- | Connection configuration for a direct API host.
data BareConfig = BareConfig
  { -- | Identity / from-field for reply posts.
    agentName :: Text,
    -- | API base URL, e.g. "https://api.deepseek.com/v1".
    baseUrl :: Text,
    -- | Model name, e.g. "deepseek-v4-pro".
    model :: Text,
    -- | API key.
    key :: Text
  }
  deriving (Show, Eq)

-- | Sensible defaults for an OpenAI-compatible DeepSeek host.
defaultBareConfig :: BareConfig
defaultBareConfig =
  BareConfig
    { agentName = "agent",
      baseUrl = "https://api.deepseek.com/v1",
      model = "deepseek-v4-pro",
      key = ""
    }

-- | A host backed by a direct OpenAI-compatible chat completions API call.
--
-- The caller supplies the system prompt; the post body becomes the user
-- message. There is no tooling, memory, or context-file injection.
bareHost ::
  -- | Connection configuration.
  BareConfig ->
  -- | System prompt.
  Text ->
  Host
bareHost cfg systemPrompt =
  Host
    { hostName = agentName cfg,
      hostBodyMode = BodyWhole,
      hostRun = \bodies -> do
        let userMessage = T.unlines bodies
        rsp <- chatCompletion cfg systemPrompt userMessage
        pure [rsp]
    }

chatCompletion :: BareConfig -> Text -> Text -> IO Text
chatCompletion cfg systemPrompt userMessage = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest (T.unpack (baseUrl cfg <> "/chat/completions"))
  let request =
        initialRequest
          { method = "POST",
            requestHeaders =
              [ ("Content-Type", "application/json"),
                ("Authorization", "Bearer " <> BC8.pack (T.unpack (key cfg)))
              ],
            requestBody =
              RequestBodyLBS $
                BL.fromStrict $
                  encodeJson $
                    JObject
                      [ ("model", JString (model cfg)),
                        ( "messages",
                          JArray
                            ( V.fromList
                                [ JObject [("role", JString "system"), ("content", JString systemPrompt)],
                                  JObject [("role", JString "user"), ("content", JString userMessage)]
                                ]
                            )
                        ),
                        ("max_tokens", JNumber 4096)
                      ]
          }
  response <- httpLbs request manager
  let status = statusCode (responseStatus response)
      body = responseBody response
  if status < 200 || status >= 300
    then pure ("🔴 HTTP " <> T.pack (show status) <> ": " <> TE.decodeUtf8 (BL.toStrict body))
    else case decodeJson (BL.toStrict body) of
      Left err -> pure ("🔴 JSON error: " <> T.pack err)
      Right j -> case objLookup "choices" j of
        Just (JArray vs) -> case V.toList vs of
          [] -> pure "🔴 empty choices"
          (c : _) -> case objLookup "message" c of
            Just m -> case objLookup "content" m of
              Just (JString t) -> pure t
              _ -> pure "🔴 missing content"
            _ -> pure "🔴 missing message"
        _ -> pure "🔴 empty choices"
