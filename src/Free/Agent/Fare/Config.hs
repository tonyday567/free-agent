{-# LANGUAGE OverloadedStrings #-}

-- | Shared fare CLI types, options, and repository configuration.
--
-- The end knob: each menu item's layer-1 answer is bounded by its own
-- distillation, including which end the slice takes. --head / --tail
-- override the end for testing untried seats and harnessings; with no
-- flag the item decides.
--
-- A repository's optional @.fare@ file selects the default docspec lane:
-- @gate.docspec = none@, @repository@, or @full@. Command flags override it.
module Free.Agent.Fare.Config
  ( End (..),
    endOpt,
    rootArg,
    parseFareConfig,
    readGateDocspec,
    applyEndUnder,
  )
where

import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Free.Agent.Fare.Gate (DocspecScope (..))
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

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

-- | Parse the repository's fare settings. Missing and empty configurations
-- select no docspec lane; callers decide whether the file exists.
parseFareConfig :: Text -> Either Text (Maybe DocspecScope)
parseFareConfig input = case settings of
  [] -> Right Nothing
  [("gate.docspec", settingValue)] -> case settingValue of
    "none" -> Right Nothing
    "repository" -> Right (Just DocspecRepository)
    "full" -> Right (Just DocspecFull)
    _ -> Left "gate.docspec must be none, repository, or full"
  [(key, _)] -> Left ("unknown setting " <> key <> "; expected gate.docspec = none|repository|full")
  _ -> Left "expected one gate.docspec = none|repository|full setting"
  where
    settings =
      [ (T.strip key, T.strip (T.drop 1 settingValue))
      | raw <- T.lines input,
        let line = T.strip raw,
        not (T.null line),
        not ("#" `T.isPrefixOf` line),
        let (key, settingValue) = T.breakOn "=" line
      ]

-- | Read ROOT's docspec default. An invalid configuration is a command error
-- that restates the accepted setting rather than silently narrowing the gate.
readGateDocspec :: FilePath -> IO (Maybe DocspecScope)
readGateDocspec root = do
  let path = root </> ".fare"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else TIO.readFile path >>= either (configError path) pure . parseFareConfig
  where
    configError path msg = do
      TIO.hPutStrLn stderr ("fare gate: " <> T.pack path <> ": " <> msg)
      exitFailure

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
