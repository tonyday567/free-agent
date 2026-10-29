{-# LANGUAGE OverloadedStrings #-}

module Axioma.Usage
  ( usageTopic,
  )
where

import Axioma.Common (assert)
import Data.Either (isLeft)
import Data.Text qualified as T
import Free.Agent.Usage
  ( UsageIdentity (..),
    UsageTotal (..),
    promptTokensFromExport,
    renderUsageIdentity,
    usageIdentityFromFile,
    usageShares,
  )

usageTopic :: IO ()
usageTopic = do
  putStrLn "Hermes latest-prompt usage"
  let valid = "{\"model_config\":{\"_usage_anchor\":{\"prompt_tokens\":24111}},\"input_tokens\":18735}"
  assert "usage anchor beats cumulative input" $ promptTokensFromExport valid == Right 24111
  let stringConfig = "{\"model_config\":\"{\\\"_usage_anchor\\\":{\\\"prompt_tokens\\\":24111}}\"}"
  assert "string-encoded live model_config unwraps" $ promptTokensFromExport stringConfig == Right 24111
  assert "cumulative input alone is not a context measurement" $ isLeft (promptTokensFromExport "{\"input_tokens\":18735}")
  assert "missing anchor is explicit" $ isLeft (promptTokensFromExport "{\"model_config\":{}}")
  assert "fractional anchor is rejected" $ isLeft (promptTokensFromExport "{\"model_config\":{\"_usage_anchor\":{\"prompt_tokens\":1.5}}}")
  assert "malformed export is rejected" $ isLeft (promptTokensFromExport "not-json")
  assert "empty export is rejected" $ isLeft (promptTokensFromExport "")

  let live =
        "{\"session_id\":\"session-live\",\"model\":\"deepseek/deepseek-v4-pro\","
          <> "\"provider\":\"nous\",\"api_calls\":2,\"input_tokens\":33424,"
          <> "\"output_tokens\":246,\"cache_read_tokens\":17408,"
          <> "\"cache_write_tokens\":0,\"total_tokens\":33670,"
          <> "\"completed\":true,\"failed\":false,"
          <> "\"auxiliary\":{\"api_calls\":1,\"total_tokens\":292},"
          <> "\"total_including_auxiliary\":{\"api_calls\":3,\"total_tokens\":33962}}"
      parsed = usageIdentityFromFile live
      expected =
        UsageIdentity
          { uiSession = "session-live",
            uiModel = "deepseek/deepseek-v4-pro",
            uiProvider = "nous",
            uiRequests = 2,
            uiC = 17408,
            uiF = 16016,
            uiO = 246,
            uiMass = 33670,
            uiAuxiliary = Just (UsageTotal 1 292),
            uiIncluding = Just (UsageTotal 3 33962)
          }
  assert "usage-file extracts the real session C/F/O identity" $ parsed == Right expected
  assert "C/F/O shares divide by main mass" $ case parsed of
    Left _ -> False
    Right u ->
      let (c, f, o) = usageShares u
       in abs (c - 51.70) < 0.01 && abs (f - 47.57) < 0.01 && abs (o - 0.73) < 0.01
  assert "usage-file render names unavailable context history" $ case parsed of
    Left _ -> False
    Right u -> "ctx0=unavailable" `T.isInfixOf` renderUsageIdentity u && "compaction segments=unavailable" `T.isInfixOf` renderUsageIdentity u
  assert "total-prompt accounting mismatch fails closed" $
    isLeft (usageIdentityFromFile (T.replace "\"total_tokens\":33670" "\"total_tokens\":33671" live))
  assert "cache traffic cannot exceed input" $
    isLeft (usageIdentityFromFile (T.replace "\"cache_read_tokens\":17408" "\"cache_read_tokens\":33425" live))
  assert "failed runs are not metered as live sessions" $
    isLeft (usageIdentityFromFile (T.replace "\"failed\":false" "\"failed\":true" live))
