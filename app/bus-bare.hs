{-# LANGUAGE OverloadedStrings #-}

-- | Bare API agent runner for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), forwards each body to
-- a direct OpenAI-compatible chat completions endpoint, and writes the reply
-- back through the scribe with proper thread edges.
--
-- Configuration is read from environment variables:
--   BUS_BARE_KEY      API key (required)
--   BUS_BARE_BASE     API base URL (default: https://api.deepseek.com/v1)
--   BUS_BARE_MODEL    Model name (default: deepseek-v4-pro)
--
-- Usage:
--   free-agent-bus-bare ROOT NAME [NAME...] PROMPT.md
module Main (main) where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), PostId, sortNub)
import Circuit.Agent.Framing (StoredPost, stampId, stamped)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Monad (guard, when)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File
  ( cursorPath,
    findScribe,
    readCursor,
    scribePost,
    tailLog,
    writeCursor,
  )
import Free.Agent.Host (BareConfig (..), bareHost, defaultBareConfig)
import Free.Agent.Seat (FreeSeat, hostSeat, interpretSeat)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))

usage :: IO ()
usage = do
  TIO.putStrLn "Usage: free-agent-bus-bare ROOT NAME [NAME...] PROMPT.md"
  TIO.putStrLn ""
  TIO.putStrLn "  ROOT            directory containing log.jsonl"
  TIO.putStrLn "  NAME            agent name(s) to subscribe to"
  TIO.putStrLn "  PROMPT.md       system prompt markdown file"
  TIO.putStrLn ""
  TIO.putStrLn "Environment variables:"
  TIO.putStrLn "  BUS_BARE_KEY    API key (required)"
  TIO.putStrLn "  BUS_BARE_BASE   API base URL (default: https://api.deepseek.com/v1)"
  TIO.putStrLn "  BUS_BARE_MODEL  Model name (default: deepseek-v4-pro)"

-- | Parse arguments of the form:
--   ROOT NAME [NAME...] PROMPT.md
parseArgs :: [String] -> Maybe (FilePath, [Name], FilePath)
parseArgs args = case args of
  (root : rest)
    | null rest -> Nothing
    | otherwise -> do
        let promptFile = last rest
            names = map T.pack (init rest)
        guard (not (null names))
        guard (".md" `T.isSuffixOf` T.pack promptFile)
        pure (root, names, promptFile)
  _ -> Nothing

-- | Read configuration from environment, requiring an API key.
readConfig :: Text -> IO BareConfig
readConfig agentName = do
  mKey <- lookupEnv "BUS_BARE_KEY"
  key <- case mKey of
    Nothing -> do
      TIO.putStrLn "🔴 BUS_BARE_KEY environment variable is required"
      exitFailure
    Just k -> pure (T.pack k)
  mBase <- lookupEnv "BUS_BARE_BASE"
  mModel <- lookupEnv "BUS_BARE_MODEL"
  pure
    defaultBareConfig
      { agentName = agentName,
        baseUrl = maybe (baseUrl defaultBareConfig) T.pack mBase,
        model = maybe (model defaultBareConfig) T.pack mModel,
        key = key
      }

-- | Run one stored post through the seat and produce reply posts with thread
-- edges citing the parent id.
runOne :: FreeSeat -> StoredPost -> IO [Post Text]
runOne seat stored = do
  let p = stamped stored
      parentId = stampId stored
      sh = interpretSeat seat
  (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
  pure [out {thread = sortNub (parentId : thread out)} | out <- outs]

main :: IO ()
main = do
  args <- getArgs
  when (any ("-" `isPrefixOf`) args) $ do
    usage
    exitFailure
  case parseArgs args of
    Nothing -> do
      usage
      exitFailure
    Just (root, names, promptFile) -> do
      systemPrompt <- TIO.readFile promptFile
      let agentName = case names of (n : _) -> n; [] -> "agent"
      cfg <- readConfig agentName
      let host = bareHost cfg systemPrompt
          seat = hostSeat host
      scribe <- findScribe
      TIO.putStrLn $ "🟢 bus bare agent starting: " <> T.intercalate "," names
      TIO.putStrLn $ "   root: " <> T.pack root
      TIO.putStrLn $ "   prompt: " <> T.pack promptFile
      TIO.putStrLn $ "   base: " <> baseUrl cfg
      TIO.putStrLn $ "   model: " <> model cfg
      TIO.putStrLn $ "   scribe: " <> T.pack scribe
      let path = root </> "log.jsonl"
          keepReply p =
            let b = T.strip (body p)
             in not (T.null b)
      cursor <- readCursor root agentName
      TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
      tailLog path names cursor Nothing $ \stored -> do
        replies <- filter keepReply <$> runOne seat stored
        traverse_ (scribePost scribe root) replies
        writeCursor root agentName (stampId stored + 1)
