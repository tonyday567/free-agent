{-# LANGUAGE OverloadedStrings #-}

-- | CLI for bus log flow metrics.
--
-- Usage:
--
--   bus-stats log.jsonl
--   bus-stats --window 15m log.jsonl
--   bus-stats --thread --json log.jsonl
module Main (main) where

import Circuit.Agent.Framing (Jsonl (..), StoredPost, parseLine)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.BusStats
  ( Rules (..),
    SliceMode (..),
    classify,
    computeStats,
    defaultRules,
    renderStats,
    renderStatsJson,
    slicePosts,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr helpText
      exitFailure
    Right opts -> run opts

helpText :: String
helpText =
  unlines
    [ "Usage: bus-stats [OPTIONS] LOG.jsonl",
      "",
      "Options:",
      "  --noise REGEX      override noise regex",
      "  --signal REGEX     override signal regex",
      "  --window DURATION  slice by time bucket (e.g. 15m, 1h)",
      "  --thread           slice by thread root",
      "  --damping N        damping rules count (default 1)",
      "  --json             output JSON",
      "  -h, --help         show this help"
    ]

data Options = Options
  { optLogPath :: FilePath,
    optRules :: Rules,
    optSliceMode :: SliceMode,
    optDamping :: Int,
    optJson :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optLogPath = "",
      optRules = defaultRules,
      optSliceMode = WholeLog,
      optDamping = 1,
      optJson = False
    }

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] =
      if null (optLogPath opts)
        then Left "missing LOG.jsonl"
        else Right opts
    go opts ("--noise" : v : rest) =
      go (opts {optRules = (optRules opts) {noiseRE = T.pack v}}) rest
    go opts ("--signal" : v : rest) =
      go (opts {optRules = (optRules opts) {signalRE = T.pack v}}) rest
    go opts ("--window" : v : rest) =
      case parseDuration v of
        Nothing -> Left ("bad duration: " ++ v)
        Just m -> go (opts {optSliceMode = WindowMinutes m}) rest
    go opts ("--thread" : rest) =
      go (opts {optSliceMode = ByThread}) rest
    go opts ("--damping" : v : rest) =
      case reads v of
        [(n, "")] | n > 0 -> go (opts {optDamping = n}) rest
        _ -> Left ("bad damping count: " ++ v)
    go opts ("--json" : rest) =
      go (opts {optJson = True}) rest
    go _ ("-h" : _) = Left helpText
    go _ ("--help" : _) = Left helpText
    go opts [path] =
      if null (optLogPath opts)
        then go (opts {optLogPath = path}) []
        else Left ("unexpected argument: " ++ path)
    go _ (x : _) = Left ("unknown option: " ++ x)

parseDuration :: String -> Maybe Int
parseDuration s =
  let (numPart, unitPart) = span isDigit s
   in case (numPart, unitPart) of
        ("", _) -> Nothing
        (n, "s") -> Just (max 1 (read n `div` 60))
        (n, "m") -> Just (read n)
        (n, "h") -> Just (read n * 60)
        _ -> Nothing

run :: Options -> IO ()
run opts = do
  contents <- TIO.readFile (optLogPath opts)
  let posts = parseLog contents
      slices = slicePosts (optSliceMode opts) posts
      stats = map (uncurry (computeStats (optRules opts) (optDamping opts))) slices
  if optJson opts
    then TIO.putStrLn (renderStatsJson (optRules opts) stats)
    else TIO.putStrLn (renderStats (optRules opts) stats)

parseLog :: Text -> [StoredPost]
parseLog = foldl' step [] . T.lines
  where
    step acc line = case parseLine line of
      Just p -> acc ++ [p]
      Nothing -> acc
