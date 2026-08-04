{-# LANGUAGE OverloadedStrings #-}

-- | Minimal CLI for the free-agent bus.
--
-- Two modes:
--
--   scribe (default):
--     echo '{"from":"human","to":["bot"],"thread":[],"body":"hello"}' \
--       | free-agent-bus [ROOT]
--
--   watch:
--     free-agent-bus watch [ROOT] NAME [NAME...]
--
-- Watch tails ROOT/log.jsonl via fsnotify and prints stamped JSONL lines
-- addressed to any of the given names. This is the out-of-process agent read
-- path; agents can pipe watch output into their loop and write replies back
-- through the scribe.
module Main (main) where

import Circuit.Agent (Name, Post (..), deliversTo)
import Circuit.Agent.Framing (StoredPost, frameStored, parseLine, parsePost, stamped)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, when)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (closeBus, openBus, scribeIO)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ("watch" : rest) -> runWatch rest
    [r] -> runScribe r
    [] -> runScribe "."
    _ -> do
      TIO.putStrLn "Usage: free-agent-bus [ROOT]"
      TIO.putStrLn "       free-agent-bus watch [ROOT] NAME [NAME...]"
      exitFailure

-- ---------------------------------------------------------------------------
-- Scribe
-- ---------------------------------------------------------------------------

runScribe :: FilePath -> IO ()
runScribe root = do
  bus <- openBus root
  line <- TIO.getLine
  case parsePost line of
    Nothing -> do
      TIO.putStrLn "🔴 invalid post JSON"
      closeBus bus
      exitFailure
    Just p -> do
      stored <- scribeIO bus p
      TIO.putStrLn (frameStored stored)
      closeBus bus

-- ---------------------------------------------------------------------------
-- Watch
-- ---------------------------------------------------------------------------

runWatch :: [String] -> IO ()
runWatch args = do
  let (root, names) = case args of
        [] -> (".", [])
        (r : ns) -> if null ns then (".", [T.pack r]) else (r, map T.pack ns)
  when (null names) $ do
    TIO.putStrLn "🔴 watch requires at least one name"
    exitFailure
  let path = root </> "log.jsonl"
      logName = takeFileName path
      dir = takeDirectory path
  exists <- doesFileExist path
  unless exists $ do
    -- Create an empty log so watchers have something to tail.
    withFile path AppendMode (\_ -> pure ())
  h <- openFile path ReadMode
  hSetBuffering h LineBuffering
  hSeek h AbsoluteSeek 0
  -- Skip existing content; agents start watching from now.
  size <- hFileSize h
  hSeek h AbsoluteSeek size
  withManager $ \mgr -> do
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      drain h names
    -- Block forever; the watch listener runs in the background.
    forever (threadDelay 1000000)
  where
    drain h names' = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ TIO.putStrLn (filterStored names' line)
        drain h names'

-- | Parse a raw log line and return it only if it is addressed to one of the
-- subscribed names.
filterStored :: [Name] -> Text -> Maybe Text
filterStored names line = do
  stored <- parseLine line
  let p = stamped stored
  if deliversTo p names then Just line else Nothing
