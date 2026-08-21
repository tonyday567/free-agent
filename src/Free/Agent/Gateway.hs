{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hermes gateway api_server client: one persistent server-side session as
-- a 'Host' seat.
--
-- The api_server (part of @hermes gateway run@) offers @POST \/api\/sessions@
-- and @POST \/api\/sessions\/{id}\/chat\/stream@ — SSE event streams with
-- protocol-posted halt marks (@run.completed@, @done@).  Framing is the
-- 'frameAgent' mark machine at 'sseMarks': the blank line ends an event
-- frame, the event names are the halt alphabet.  No polling, no quiet
-- inference — the server posts its own marks.
--
-- Auth is a Bearer token read from the environment variable named by
-- 'gwKeyEnv' (default @API_SERVER_KEY@, the platform's own convention).
--
-- Probe card: @coffee\/loom\/next-yin.md@ ("hermes server (api_server :8642)").
module Free.Agent.Gateway
  ( -- * Configuration
    GatewayConfig (..),
    defaultGatewayConfig,

    -- * Client
    GatewayClient (..),
    openGateway,

    -- * Turns
    gatewayChat,

    -- * Host seat
    gatewayHost,

    -- * SSE framing (exposed for pure tests)
    parseSseFrame,
  )
where

import Circuit.Agent (run1)
import Circuit.Agent.StdPorts (frameAgent, sseMarks)
import Circuit.Parser.Json (Json (..), decodeJson, encodeJson)
import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Free.Agent.Host (BodyMode (..), Host (..))
import Free.Agent.Json (jobject, jtext)
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders, statusCode)
import System.Environment (lookupEnv)
import Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Where the api_server is and how to authenticate to it.
data GatewayConfig = GatewayConfig
  { -- | Base URL, e.g. @http:\/\/127.0.0.1:8642@.
    gwBaseUrl :: Text,
    -- | Environment variable holding the Bearer key.
    gwKeyEnv :: String,
    -- | Working directory pinned on the server-side session.
    gwCwd :: FilePath
  }
  deriving (Show, Eq)

defaultGatewayConfig :: GatewayConfig
defaultGatewayConfig =
  GatewayConfig
    { gwBaseUrl = "http://127.0.0.1:8642/p/pit",
      gwKeyEnv = "API_SERVER_KEY",
      gwCwd = "/Users/tonyday567/pit"
    }

-- ---------------------------------------------------------------------------
-- Client
-- ---------------------------------------------------------------------------

-- | A live api_server session.
data GatewayClient = GatewayClient
  { gcConfig :: GatewayConfig,
    gcManager :: Manager,
    gcKey :: BS.ByteString,
    gcSessionId :: Text
  }

-- | Read the Bearer key from 'gwKeyEnv' and create a server-side session.
openGateway :: GatewayConfig -> IO GatewayClient
openGateway cfg = do
  mKey <- lookupEnv (gwKeyEnv cfg)
  key <- case mKey of
    Nothing -> throwIO (userError ("🔴 " <> gwKeyEnv cfg <> " environment variable not set"))
    Just k -> pure (BSC.pack k)
  mgr <- newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutNone}
  req0 <- parseRequest (T.unpack (gwBaseUrl cfg) <> "/api/sessions")
  let req =
        req0
          { method = "POST",
            requestHeaders = hdrs key,
            requestBody = RequestBodyLBS (BL.fromStrict (encodeJson (jobject [("cwd", jtext (T.pack (gwCwd cfg)))])))
          }
  resp <- httpLbs req mgr
  case decodeJson (BL.toStrict (responseBody resp)) of
    Left e -> throwIO (userError ("🔴 api_server session create: " <> e))
    Right v -> do
      let sid = textAt ["session", "id"] v
      if T.null sid
        then throwIO (userError "🔴 api_server session create: no session.id in response")
        else pure (GatewayClient cfg mgr key sid)

-- ---------------------------------------------------------------------------
-- Turns
-- ---------------------------------------------------------------------------

-- | One turn: POST @chat\/stream@, consume SSE events until the @done@
-- mark, return the completed reply text.
--
-- The reply is the @assistant.completed@ content when it arrives, else the
-- accumulated @assistant.delta@ stream.  HTTP and stream failures come back
-- as 🔴-prefixed text (the 'Free.Agent.Host.chatCompletion' convention), so
-- a failing gateway surfaces as a bus post, not a crash.
gatewayChat :: GatewayClient -> Text -> IO Text
gatewayChat c msg = do
  req0 <- parseRequest (T.unpack (gwBaseUrl (gcConfig c)) <> "/api/sessions/" <> T.unpack (gcSessionId c) <> "/chat/stream")
  let req =
        req0
          { method = "POST",
            requestHeaders = hdrs (gcKey c),
            requestBody = RequestBodyLBS (BL.fromStrict (encodeJson (jobject [("message", jtext msg)])))
          }
  withResponse req (gcManager c) $ \resp -> do
    let status = statusCode (responseStatus resp)
    if status < 200 || status >= 300
      then do
        body <- brReadSome (responseBody resp) 4096
        pure ("🔴 HTTP " <> T.pack (show status) <> ": " <> decodeUtf8 (BL.toStrict body))
      else go (responseBody resp) (BS.empty, []) mempty
  where
    sys = frameAgent sseMarks id

    go body st (deltas, completed) = do
      chunk <- brRead body
      let input = if BS.null chunk then Nothing else Just chunk
          (frames, st') = run1 sys st input
          evs = mapMaybe parseSseFrame frames
          (deltas', completed') = foldl' step (deltas, completed) evs
      if any ((== "done") . fst) evs || BS.null chunk
        then pure (finish (deltas', completed'))
        else go body st' (deltas', completed')

    step (deltas, completed) (ev, v) = case ev of
      "assistant.delta" -> (deltas <> textAt ["delta"] v, completed)
      "assistant.completed" -> (deltas, Just (textAt ["content"] v))
      "run.failed" -> (deltas, Just ("🔴 run.failed: " <> textAt ["error"] v))
      _ -> (deltas, completed)

    finish (deltas, completed) =
      let r = fromMaybe deltas completed
       in if T.null (T.strip r) then "🔴 empty reply stream" else r

-- ---------------------------------------------------------------------------
-- Host seat
-- ---------------------------------------------------------------------------

-- | A 'Host' backed by the gateway session.  All post bodies of a wake
-- cycle join into one user message (the 'Free.Agent.Host.hermesHostBatch'
-- convention); the reply is one text.
gatewayHost ::
  -- | Host name (used as the 'from' field of reply posts).
  Text ->
  -- | System prompt text prepended to every message.
  Text ->
  GatewayClient ->
  Host
gatewayHost name systemPrompt c =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = \bodies ->
        (: []) <$> gatewayChat c (systemPrompt <> "\n\nUser messages:\n" <> T.unlines bodies)
    }

-- ---------------------------------------------------------------------------
-- SSE framing
-- ---------------------------------------------------------------------------

-- | Parse one SSE event frame (@event: x\\ndata: {…}@) into the event name
-- and decoded JSON payload.  'Nothing' for comment\/keep-alive frames.
--
-- >>> parseSseFrame (Data.ByteString.Char8.pack "event: done\\ndata: {\"seq\": 7}")
-- Just ("done",Object ...)
parseSseFrame :: BS.ByteString -> Maybe (Text, Json)
parseSseFrame f = do
  let ls = BSC.lines f
  ev <- listToMaybe [BSC.drop 6 l | l <- ls, "event:" `BS.isPrefixOf` l]
  dat <- listToMaybe [BSC.drop 5 l | l <- ls, "data:" `BS.isPrefixOf` l]
  v <- either (const Nothing) Just (decodeJson (BSC.dropWhile (== ' ') dat))
  pure (decodeUtf8 (BSC.dropWhile (== ' ') ev), v)
  where
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

hdrs :: BS.ByteString -> RequestHeaders
hdrs key = [("Authorization", "Bearer " <> key), ("Content-Type", "application/json")]

textAt :: [Text] -> Json -> Text
textAt path v = go path v
  where
    go [] (JString t) = t
    go [] _ = ""
    go (p : ps) (JObject o) = maybe "" (go ps) (lookup p o)
    go _ _ = ""
