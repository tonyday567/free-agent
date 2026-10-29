{-# LANGUAGE OverloadedStrings #-}

-- | Token usage for Hermes sessions and one-shot usage files.
module Free.Agent.Usage
  ( promptTokensFromExport,
    readHermesUsage,
    UsageIdentity (..),
    UsageTotal (..),
    usageIdentityFromFile,
    usageShares,
    renderUsageIdentity,
    readUsageIdentity,
  )
where

import Circuit.Parser.Json (Json (..), decodeJson)
import Control.Exception (IOException, try)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import Text.Printf (printf)

-- | Extract the latest prompt size, never cumulative session input.
--
-- >>> promptTokensFromExport "{\"model_config\":{\"_usage_anchor\":{\"prompt_tokens\":24111}},\"input_tokens\":18735}"
-- Right 24111
-- >>> promptTokensFromExport "{\"model_config\":\"{\\\"_usage_anchor\\\":{\\\"prompt_tokens\\\":24111}}\"}"
-- Right 24111
-- >>> promptTokensFromExport "{\"input_tokens\":18735}"
-- Left "Hermes export has no integral model_config._usage_anchor.prompt_tokens"
promptTokensFromExport :: Text -> Either Text Int
promptTokensFromExport raw = do
  line <- maybe (Left "Hermes session export was empty") Right (find (not . T.null . T.strip) (T.lines raw))
  doc <- either (Left . T.pack) Right (decodeJson (encodeUtf8 line))
  config <- nested "model_config" doc
  anchor <- nested "_usage_anchor" config
  case field "prompt_tokens" anchor of
    Just (JNumber n) -> maybe (Left invalid) Right (toBoundedInteger n)
    _ -> Left invalid
  where
    invalid = "Hermes export has no integral model_config._usage_anchor.prompt_tokens"
    nested key doc = case field key doc of
      Just obj@(JObject _) -> Right obj
      Just (JString encoded) -> either (Left . T.pack) Right (decodeJson (encodeUtf8 encoded))
      _ -> Left invalid

-- | Read a Hermes session id file and measure its latest prompt tokens.
readHermesUsage :: FilePath -> IO (Either Text (Text, Int))
readHermesUsage path = do
  sidResult <- try (TIO.readFile path) :: IO (Either IOException Text)
  case sidResult of
    Left e -> pure (Left ("Could not read Hermes session file " <> T.pack path <> ": " <> T.pack (show e)))
    Right rawSid
      | T.null sid -> pure (Left ("Hermes session file is empty: " <> T.pack path))
      | otherwise -> do
          result <-
            try (readCreateProcessWithExitCode (proc "hermes" ["sessions", "export", "--session-id", T.unpack sid, "--format", "jsonl", "-"]) "") ::
              IO (Either IOException (ExitCode, String, String))
          pure $ case result of
            Left e -> Left ("Could not run Hermes session export: " <> T.pack (show e))
            Right (ExitFailure code, out, err) ->
              Left ("Hermes session export exited " <> T.pack (show code) <> ": " <> T.takeEnd 200 (T.pack (out <> err)))
            Right (ExitSuccess, out, _) -> (,) sid <$> promptTokensFromExport (T.pack out)
      where
        sid = T.strip rawSid

-- | A request/token total from a Hermes usage report.
data UsageTotal = UsageTotal
  { utRequests :: !Int,
    utTokens :: !Int
  }
  deriving (Eq, Show)

-- | The main-loop cost identity in a completed Hermes one-shot report.
-- C is cache-read traffic, F is fresh input plus cache creation, and O is
-- output. Auxiliary and grand totals remain separate from the main identity.
data UsageIdentity = UsageIdentity
  { uiSession :: !Text,
    uiModel :: !Text,
    uiProvider :: !Text,
    uiRequests :: !Int,
    uiC :: !Int,
    uiF :: !Int,
    uiO :: !Int,
    uiMass :: !Int,
    uiAuxiliary :: !(Maybe UsageTotal),
    uiIncluding :: !(Maybe UsageTotal)
  }
  deriving (Eq, Show)

-- | Decode a completed Hermes @-z --usage-file@ report. The report's
-- @input_tokens@ is total prompt traffic on this path, so F is input minus
-- cache reads and includes cache creation. The @total_tokens@ equality
-- rejects a different accounting dialect rather than guessing at it.
usageIdentityFromFile :: Text -> Either Text UsageIdentity
usageIdentityFromFile raw = do
  doc <- either (Left . T.pack) Right (decodeJson (encodeUtf8 raw))
  failed <- optionalBool "failed" doc
  if failed
    then Left ("Hermes usage file records a failed run" <> maybe "" (" · " <>) (textField "failure" doc))
    else pure ()
  completed <- requiredBool "completed" doc
  if completed then pure () else Left "Hermes usage file records an incomplete run"
  session <- requiredText "session_id" doc
  model <- requiredText "model" doc
  requests <- requiredCount "api_calls" doc
  input <- requiredCount "input_tokens" doc
  output <- requiredCount "output_tokens" doc
  cacheRead <- requiredCount "cache_read_tokens" doc
  cacheWrite <- requiredCount "cache_write_tokens" doc
  total <- requiredCount "total_tokens" doc
  if input < cacheRead + cacheWrite
    then Left "Hermes usage file has cache traffic larger than total input"
    else pure ()
  if total /= input + output
    then Left "Hermes usage file is not total-prompt accounting: total_tokens /= input_tokens + output_tokens"
    else pure ()
  auxiliary <- optionalTotal "auxiliary" doc
  including <- optionalTotal "total_including_auxiliary" doc
  pure
    UsageIdentity
      { uiSession = session,
        uiModel = model,
        uiProvider = fromMaybe "unknown" (textField "provider" doc),
        uiRequests = requests,
        uiC = cacheRead,
        uiF = input - cacheRead,
        uiO = output,
        uiMass = total,
        uiAuxiliary = auxiliary,
        uiIncluding = including
      }

-- | C, F, and O percentages of main-loop mass.
usageShares :: UsageIdentity -> (Double, Double, Double)
usageShares u = (share (uiC u), share (uiF u), share (uiO u))
  where
    share n
      | uiMass u == 0 = 0
      | otherwise = 100 * fromIntegral n / fromIntegral (uiMass u)

-- | Render the proven aggregate and name the context history the source does
-- not carry. A yellow verdict marks a complete cost read with those history
-- fields unavailable.
renderUsageIdentity :: UsageIdentity -> Text
renderUsageIdentity u =
  T.unlines
    ( [ "🟡 session=" <> uiSession u <> " model=" <> uiModel u <> " provider=" <> uiProvider u,
        "main requests=" <> tshow (uiRequests u) <> " C=" <> tshow (uiC u) <> " F=" <> tshow (uiF u) <> " O=" <> tshow (uiO u) <> " mass=" <> tshow (uiMass u),
        "shares C=" <> pct c <> " F=" <> pct f <> " O=" <> pct o,
        "ctx0=unavailable · recent carried injections=unavailable · compaction segments=unavailable — usage-file has run totals, no per-request context series"
      ]
        <> maybe [] (pure . renderTotal "auxiliary") (uiAuxiliary u)
        <> maybe [] (pure . renderTotal "including auxiliary") (uiIncluding u)
    )
  where
    (c, f, o) = usageShares u
    pct = T.pack . printf "%.2f%%"
    renderTotal label t = label <> " requests=" <> tshow (utRequests t) <> " tokens=" <> tshow (utTokens t)

-- | Read a Hermes one-shot usage report from disk.
readUsageIdentity :: FilePath -> IO (Either Text UsageIdentity)
readUsageIdentity path = do
  result <- try (TIO.readFile path) :: IO (Either IOException Text)
  pure $ case result of
    Left e -> Left ("Could not read Hermes usage file " <> T.pack path <> ": " <> T.pack (show e))
    Right raw -> usageIdentityFromFile raw

field :: Text -> Json -> Maybe Json
field key (JObject fields) = lookup key fields
field _ _ = Nothing

textField :: Text -> Json -> Maybe Text
textField key doc = case field key doc of
  Just (JString value) -> Just value
  _ -> Nothing

requiredText :: Text -> Json -> Either Text Text
requiredText key doc = maybe (Left ("Hermes usage file lacks text field " <> key)) Right (textField key doc)

requiredCount :: Text -> Json -> Either Text Int
requiredCount key doc = case field key doc of
  Just (JNumber n) -> case toBoundedInteger n of
    Just value
      | value >= 0 -> Right value
      | otherwise -> Left ("Hermes usage file has negative count " <> key)
    Nothing -> Left ("Hermes usage file lacks integral count " <> key)
  _ -> Left ("Hermes usage file lacks integral count " <> key)

requiredBool :: Text -> Json -> Either Text Bool
requiredBool key doc = case field key doc of
  Just (JBool value) -> Right value
  _ -> Left ("Hermes usage file lacks boolean field " <> key)

optionalBool :: Text -> Json -> Either Text Bool
optionalBool key doc = case field key doc of
  Nothing -> Right False
  Just (JBool value) -> Right value
  _ -> Left ("Hermes usage file has non-boolean field " <> key)

optionalTotal :: Text -> Json -> Either Text (Maybe UsageTotal)
optionalTotal key doc = case field key doc of
  Nothing -> Right Nothing
  Just obj -> Just <$> (UsageTotal <$> requiredCount "api_calls" obj <*> requiredCount "total_tokens" obj)

tshow :: (Show a) => a -> Text
tshow = T.pack . show
