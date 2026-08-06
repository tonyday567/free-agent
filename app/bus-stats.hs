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
import Data.List (foldl', isPrefixOf)
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
      "  --by-agent         slice by authoring agent",
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
parseArgs args = do
  (flags, positionals) <- parseFlags args [] []
  case positionals of
    [] -> Left "missing LOG.jsonl"
    [path] -> applyFlags flags (defaultOptions {optLogPath = path})
    _ -> Left ("unexpected extra arguments: " ++ unwords (drop 1 positionals))

-- | Separate flags (and their values) from positional arguments.
-- Flags may appear before or after the positional LOG path.
parseFlags :: [String] -> [String] -> [String] -> Either String ([String], [String])
parseFlags [] flags pos = Right (reverse flags, reverse pos)
parseFlags ("-h" : _) _ _ = Left helpText
parseFlags ("--help" : _) _ _ = Left helpText
parseFlags ("--noise" : v : rest) flags pos = parseFlags rest ("--noise" : v : flags) pos
parseFlags ("--signal" : v : rest) flags pos = parseFlags rest ("--signal" : v : flags) pos
parseFlags ("--window" : v : rest) flags pos = parseFlags rest ("--window" : v : flags) pos
parseFlags ("--damping" : v : rest) flags pos = parseFlags rest ("--damping" : v : flags) pos
parseFlags ("--thread" : rest) flags pos = parseFlags rest ("--thread" : flags) pos
parseFlags ("--by-agent" : rest) flags pos = parseFlags rest ("--by-agent" : flags) pos
parseFlags ("--json" : rest) flags pos = parseFlags rest ("--json" : flags) pos
parseFlags (x : rest) flags pos
  | "--" `isPrefixOf` x = Left ("unknown option: " ++ x)
  | otherwise = parseFlags rest flags (x : pos)

applyFlags :: [String] -> Options -> Either String Options
applyFlags [] opts = Right opts
applyFlags ("--noise" : v : rest) opts =
  applyFlags rest (opts {optRules = (optRules opts) {noiseRE = T.pack v}})
applyFlags ("--signal" : v : rest) opts =
  applyFlags rest (opts {optRules = (optRules opts) {signalRE = T.pack v}})
applyFlags ("--window" : v : rest) opts =
  case parseDuration v of
    Nothing -> Left ("bad duration: " ++ v)
    Just m -> applyFlags rest (opts {optSliceMode = WindowMinutes m})
applyFlags ("--thread" : rest) opts =
  applyFlags rest (opts {optSliceMode = ByThread})
applyFlags ("--by-agent" : rest) opts =
  applyFlags rest (opts {optSliceMode = ByAgent})
applyFlags ("--damping" : v : rest) opts =
  case reads v of
    [(n, "")] | n > 0 -> applyFlags rest (opts {optDamping = n})
    _ -> Left ("bad damping count: " ++ v)
applyFlags ("--json" : rest) opts =
  applyFlags rest (opts {optJson = True})
applyFlags (_ : rest) opts = applyFlags rest opts

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
