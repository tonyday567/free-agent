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
    hermesHost,
    kimiHost,
    bareHost,
    defaultBareConfig,
  )
where

import Circuit (Ends (..), endsK)
import Circuit.Agent (Post (..), Shard, mkPost)
import Circuit.Agent.Cli (Cli, cliQuery, hermesCli, kimiCli)
import Circuit.Parser.Json (Json (..), encodeJson)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (MonadState (..))
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
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
data Host m = Host
  { -- | Name used as the 'from' field of reply posts.
    hostName :: Text,
    -- | How to split the incoming post body before calling 'hostRun'.
    hostBodyMode :: BodyMode,
    -- | Run the host on the prepared arguments.
    hostRun :: [Text] -> m [Text]
  }

-- | Smart constructor with the default 'BodyWords' mode.
mkHost :: Text -> ([Text] -> m [Text]) -> Host m
mkHost name f = Host name BodyWords f

-- | Split a post body according to the host's 'BodyMode'.
bodyArgs :: BodyMode -> Text -> [Text]
bodyArgs BodyWords = T.words
bodyArgs BodyLines = T.lines
bodyArgs BodyWhole = (:[])

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
hostShard ::
  (MonadState [Post Text] m) =>
  Host m ->
  Shard m [Post Text] [Post Text]
hostShard h =
  endsK
    (\xs -> put xs)
    ( do
        xs <- get
        put []
        fmap concat $
          traverse
            ( \p -> do
                outs <- hostRun h (bodyArgs (hostBodyMode h) (body p))
                pure [mkPost (hostName h) [from p] o | o <- outs]
            )
            xs
    )

-- | A host backed by an external process.
--
-- The command receives the fixed @args@ followed by the prepared post body
-- (one argument when 'BodyWhole', whitespace-split words by default).  Output
-- lines become reply posts.  Uses 'System.Process.readProcess'.
processHost ::
  (MonadIO m) =>
  -- | Host name.
  Text ->
  -- | Command to run.
  FilePath ->
  -- | Fixed command arguments.
  [String] ->
  Host m
processHost name cmd args =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = \ws -> do
        out <- liftIO (readProcess cmd (args ++ map T.unpack ws) "")
        pure (map T.pack (lines out))
    }

-- | A host backed by a live CLI agent recipe ('Circuit.Agent.Cli').
--
-- Each prepared body becomes one 'cliQuery'; session scrape/resume/stale
-- fallback happens inside the recipe.  One reply text per body (multi-line
-- bodies and replies are preserved).
cliHost ::
  (MonadIO m) =>
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | Invocation recipe.
  Cli ->
  Host m
cliHost name cli =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = traverse (liftIO . cliQuery cli)
    }

-- | Kimi host on the shared 'Cli' seat.
--
-- Runs @kimi -p@ per body (see 'kimiCli'), with optional @-m <model>@ and
-- session resume via @sessionFile@.  A stale session falls back to fresh.
kimiHost ::
  (MonadIO m) =>
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | Model name passed to @kimi -m@, if any.
  Maybe Text ->
  -- | Session file for cross-call context.
  FilePath ->
  Host m
kimiHost name model sessionFile = cliHost name (kimiCli model sessionFile)

-- | Hermes host on the shared 'Cli' seat.
--
-- Runs @hermes chat -q@ per body (see 'hermesCli'), prepending the supplied
-- system prompt to the body in the query.  Sessions persist across calls
-- via @sessionFile@; a stale session falls back to fresh.
--
-- The caller is responsible for building the system prompt; this function
-- knows nothing about design documents, protocol cards, or magic wording.
hermesHost ::
  (MonadIO m) =>
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | System prompt text prepended to every body.
  Text ->
  -- | Session file for cross-call context.
  FilePath ->
  Host m
hermesHost name systemPrompt sessionFile =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = traverse runOne
    }
  where
    cli = hermesCli (Just "deepseek-v4-pro") (Just "deepseek") sessionFile
    runOne body =
      liftIO (cliQuery cli (systemPrompt <> "\n\nUser message:\n" <> body))

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
  (MonadIO m) =>
  -- | Connection configuration.
  BareConfig ->
  -- | System prompt.
  Text ->
  Host m
bareHost cfg systemPrompt =
  Host
    { hostName = agentName cfg,
      hostBodyMode = BodyWhole,
      hostRun = \bodies -> do
        let userMessage = T.unlines bodies
        rsp <- liftIO $ chatCompletion cfg systemPrompt userMessage
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
    else case eitherDecode body of
      Left err -> pure ("🔴 JSON error: " <> T.pack err)
      Right cr -> case responseChoices cr of
        [] -> pure "🔴 empty choices"
        (c : _) -> pure (messageContent (message c))

newtype ChatResponse = ChatResponse {responseChoices :: [Choice]}
  deriving (Show)

newtype Choice = Choice {message :: Message}
  deriving (Show)

newtype Message = Message {messageContent :: Text}
  deriving (Show)

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \v ->
    ChatResponse <$> v .: "choices"

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v ->
    Choice <$> v .: "message"

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message <$> v .: "content"
