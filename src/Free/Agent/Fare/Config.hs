{-# LANGUAGE OverloadedStrings #-}

-- | Shared fare CLI types and options (cake/fare.md §design principles).
--
-- The end knob: each menu item's layer-1 answer is bounded by its own
-- distillation, including which end the slice takes. --head / --tail
-- override the end for testing untried seats and harnessings; with no
-- flag the item decides.
module Free.Agent.Fare.Config
  ( End (..),
    endOpt,
    rootArg,
    applyEndUnder,
  )
where

import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative

-- | which end a layer-1 slice takes.
data End = Head | Tail
  deriving (Show, Eq)

-- | Testing knob: override which end the layer-1 slice takes. With no
-- flag, the item's own distillation decides (and its bound is its own
-- business either way).
endOpt :: Parser (Maybe End)
endOpt =
  optional
    ( flag' Head (long "head" <> help "Take the layer-1 slice from the head end (testing knob)")
        <|> flag' Tail (long "tail" <> help "Take the layer-1 slice from the tail end (testing knob)")
    )

rootArg :: Parser FilePath
rootArg =
  argument
    str
    ( metavar "ROOT"
        <> value "."
        <> showDefault
        <> help "Working directory for the command"
    )

-- $setup
-- >>> import Data.Text (pack)

-- | Apply a byte budget to a listing, taking lines from the requested
-- end until the budget is spent. A single line over budget is dropped
-- entirely, so the result never exceeds the budget.
--
-- >>> applyEndUnder 5 Head (map pack ["ab", "cd", "ef"])
-- ["ab","cd"]
-- >>> applyEndUnder 5 Tail (map pack ["ab", "cd", "ef"])
-- ["cd","ef"]
-- >>> applyEndUnder 0 Head [pack "ab"]
-- []
applyEndUnder :: Int -> End -> [Text] -> [Text]
applyEndUnder cap end = case end of
  Head -> go 0
  Tail -> reverse . go 0 . reverse
  where
    go _ [] = []
    go acc (x : xs)
      | acc + byteLen x > cap = []
      | otherwise = x : go (acc + byteLen x) xs

byteLen :: Text -> Int
byteLen = B.length . encodeUtf8
