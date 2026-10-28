{-# LANGUAGE OverloadedStrings #-}

-- | Latest-prompt token usage for Hermes sessions.
module Free.Agent.Usage
  ( promptTokensFromExport,
    readHermesUsage,
  )
where

import Circuit.Parser.Json (Json (..), decodeJson)
import Control.Exception (IOException, try)
import Data.List (find)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

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
    field key (JObject fields) = lookup key fields
    field _ _ = Nothing
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
