{-# LANGUAGE OverloadedStrings #-}

-- | One-shot host algebra over circuits-agent seats.
--
-- A 'Host' is a named effectful seat that receives arguments drawn from the
-- body of an incoming post and produces lines of output. CLI delivery lives
-- in 'Free.Agent.Cli'; generic host execution and response mapping live here.
module Free.Agent.Host
  ( BodyMode (..),
    Host (..),
    BareConfig (..),
    mkHost,
    hostResponse,
    hostShard,
    processHost,
    hermesCli,
    bareHost,
    defaultBareConfig,
  )
where

import Circuit.Agent (AgentShard, Post (..), ioShard, mkPost)
import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, parseSessionId)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Process (readProcess)

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
--
-- The default is the design card's branching factor exactly 1: a reply
-- addresses the sender only, and the @all@ magic top is localised by
-- 'deliversTo', never propagated into replies (a reply carrying @all@
-- would re-broadcast — the amplifier the card flags).  Reply-all widening
-- is a seat-level choice for the deferred address-type wave.
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

-- | Run a host on one post and apply the default reply-to-sender mapping.
-- Generic hosts have no stamped id; callers add ancestry where it exists.
hostResponse :: Host -> Post Text -> IO [Post Text]
hostResponse h p = do
  outs <- hostRun h (bodyArgs (hostBodyMode h) (body p))
  pure [(mkPost (hostName h) [from p] o) {channel = channel p} | o <- outs]

-- | Turn a host into a stateful shard that consumes every committed post.
-- The shard remembers committed posts and emits 'hostResponse' batches.
hostShard :: Host -> AgentShard [Post Text] [Post Text]
hostShard h = ioShard (fmap concat . traverse (hostResponse h))

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

-- | Shared CLI recipe for hermes backends.
hermesCli :: Maybe Text -> Maybe Text -> Bool -> FilePath -> Cli
hermesCli model provider yolo sessionFile =
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
      cliStderrTee = Nothing
    }

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

-- | Look up a key in a JSON object.
objLookup :: Text -> Json -> Maybe Json
objLookup k (JObject ps) = lookup k ps
objLookup _ _ = Nothing
