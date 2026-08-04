{-# LANGUAGE OverloadedStrings #-}

-- | Hermes-backed agent runner for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), forwards each body to
-- Hermes with the given system prompt, and writes the cleaned reply lines back
-- through the scribe with proper thread edges.
--
-- Usage:
--   free-agent-bus-hermes ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]
--
-- Example:
--   free-agent-bus-hermes ./bus hermes-bot prompt.md ./bus/hermes-bot.sid
module Main (main) where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), deliversTo, sortNub)
import Circuit.Agent.Framing (StoredPost, framePost, parseLine, stampId, stamped)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forever, guard, unless, when)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Host (hermesHost)
import Free.Agent.Seat (FreeSeat, hostSeat, interpretSeat)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FSNotify
  ( Event (..),
    WatchManager,
    watchDir,
    withManager,
  )
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (AppendMode, ReadMode),
    SeekMode (AbsoluteSeek),
    hFileSize,
    hIsEOF,
    hSeek,
    hSetBuffering,
    openFile,
    stdout,
    withFile,
  )
import System.Process (readProcess)

-- | Parse arguments of the form:
--   ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]
parseArgs :: [String] -> Maybe (FilePath, [Name], FilePath, FilePath)
parseArgs [] = Nothing
parseArgs (root : rest) = do
  let (names, promptAndSess) = break (".md" `isSuffixOf`) rest
  promptFile <- listToMaybe promptAndSess
  let sessCandidates = drop 1 promptAndSess
  let sessionFile =
        case sessCandidates of
          [s] -> s
          _ -> root </> ".sessions" </> agentNameOf names <> ".sid"
  guard (not (null names))
  pure (root, map T.pack names, promptFile, sessionFile)
  where
    agentNameOf [] = "agent"
    agentNameOf (n : _) = n

usage :: IO ()
usage = do
  TIO.putStrLn "Usage: free-agent-bus-hermes ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]"
  TIO.putStrLn ""
  TIO.putStrLn "  ROOT            directory containing log.jsonl"
  TIO.putStrLn "  NAME            agent name(s) to subscribe to"
  TIO.putStrLn "  PROMPT.md       system prompt markdown file"
  TIO.putStrLn "  SESSION-FILE    optional Hermes session file (default: ROOT/.sessions/NAME.sid)"

-- | Locate the scribe executable. Prefer the sibling in the cabal build tree,
-- then PATH.
findScribe :: IO FilePath
findScribe = do
  self <- getExecutablePath
  let buildTree =
        takeDirectory self
          </> ".."
          </> ".."
          </> ".."
          </> "free-agent-bus"
          </> "build"
          </> "free-agent-bus"
          </> "free-agent-bus"
  candidates <- filterM doesFileExist [buildTree, takeDirectory self </> "free-agent-bus"]
  case candidates of
    (p : _) -> pure p
    [] -> do
      mPath <- findExecutable "free-agent-bus"
      case mPath of
        Just p -> pure p
        Nothing -> do
          TIO.putStrLn "🔴 free-agent-bus scribe executable not found"
          exitFailure

-- | Scribe one post by invoking the external scribe executable.
scribePost :: FilePath -> FilePath -> Post Text -> IO ()
scribePost scribe root p = do
  _ <- readProcess scribe [root] (T.unpack (framePost p))
  pure ()

-- | Run one stored post through the seat and produce reply posts with thread
-- edges citing the parent id.
runOne :: FreeSeat -> StoredPost -> IO [Post Text]
runOne seat stored = do
  let p = stamped stored
      parentId = stampId stored
      sh = interpretSeat seat
  (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
  pure [out {thread = sortNub (parentId : thread out)} | out <- outs]

-- | Event-tail a log file and invoke the callback for every new stored post
-- addressed to any of the subscribed names.
tailLog :: FilePath -> [Name] -> (StoredPost -> IO ()) -> IO ()
tailLog path names cb = do
  exists <- doesFileExist path
  unless exists $ do
    -- Touch an empty log so the scribe has a file to append to.
    withFile path AppendMode (\_ -> pure ())
  h <- openFile path ReadMode
  hSetBuffering h LineBuffering
  size <- hFileSize h
  hSeek h AbsoluteSeek size
  let logName = takeFileName path
      dir = takeDirectory path
  withManager $ \mgr -> do
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      drain h names cb
    -- Block forever; the watch listener runs in the background.
    forever (threadDelay 1000000)
  where
    drain h names' cb' = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ cb' (filterStored names' line)
        drain h names' cb'

    filterStored :: [Name] -> Text -> Maybe StoredPost
    filterStored names' line = do
      stored <- parseLine line
      if deliversTo (stamped stored) names' then Just stored else Nothing

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Nothing -> do
      usage
      exitFailure
    Just (root, names, promptFile, sessionFile) -> do
      when (null names) $ do
        TIO.putStrLn "🔴 at least one NAME is required"
        exitFailure
      systemPrompt <- TIO.readFile promptFile
      createDirectoryIfMissing True (takeDirectory sessionFile)
      let agentName = case names of (n : _) -> n; [] -> "agent"
          host = hermesHost agentName systemPrompt sessionFile
          seat = hostSeat host
      scribe <- findScribe
      TIO.putStrLn $ "🟢 bus hermes agent starting: " <> T.intercalate "," names
      TIO.putStrLn $ "   root: " <> T.pack root
      TIO.putStrLn $ "   prompt: " <> T.pack promptFile
      TIO.putStrLn $ "   session: " <> T.pack sessionFile
      TIO.putStrLn $ "   scribe: " <> T.pack scribe
      let path = root </> "log.jsonl"
      tailLog path names $ \stored -> do
        replies <- runOne seat stored
        traverse_ (scribePost scribe root) replies
