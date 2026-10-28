{-# LANGUAGE OverloadedStrings #-}

module Axioma.Usage
  ( usageTopic,
  )
where

import Axioma.Common (assert)
import Data.Either (isLeft)
import Free.Agent.Usage (promptTokensFromExport)

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
