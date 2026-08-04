{-# LANGUAGE OverloadedStrings #-}

-- | Out-of-process agent runner for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), invokes an external
-- command for each post, and writes the command's output lines back through
-- the scribe with proper thread edges.
--
-- This is the durable-log counterpart to the in-process 'runSeatBus'. It does
-- not share STM state with the scribe; instead it polls the file directly and
-- writes replies by invoking the scribe executable.
--
-- Usage:
--   free-agent-bus-agent ROOT NAME [NAME...] -- CMD [ARGS...]
--
-- Example:
--   free-agent-bus-agent ./bus echo-bot -- echo "heard:"
module Main (main) where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), deliversTo, sortNub)
import Circuit.Agent.Framing (StoredPost, framePost, parseLine, stampId, stamped)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forever, unless, when)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Host (BodyMode (..), Host (..), hostShard, processHost)
import Free.Agent.Seat (FreeSeat, hostSeat, interpretSeat)
import System.Directory (doesFileExist, findExecutable)
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
--   ROOT NAME [NAME...] -- CMD [ARGS...]
parseArgs :: [String] -> Maybe (FilePath, [Name], FilePath, [String])
parseArgs [] = Nothing
parseArgs (root : rest) = do
  (names, "--" : cmdArgs) <- pure (break (== "--") rest)
  (cmd, cmdArgs') <- uncons cmdArgs
  pure (root, map T.pack names, cmd, cmdArgs')

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

usage :: IO ()
usage = do
  TIO.putStrLn "Usage: free-agent-bus-agent ROOT NAME [NAME...] -- CMD [ARGS...]"
  TIO.putStrLn ""
  TIO.putStrLn "  ROOT            directory containing log.jsonl"
  TIO.putStrLn "  NAME            agent name(s) to subscribe to"
  TIO.putStrLn "  CMD ARGS...     command that receives the post body as one argument"
  TIO.putStrLn "                  and prints reply lines on stdout"

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
    Just (root, names, cmd, cmdArgs) -> do
      when (null names) $ do
        TIO.putStrLn "🔴 at least one NAME is required"
        exitFailure
      let agentName = case names of (n : _) -> n; [] -> "agent"
          host = (processHost agentName cmd cmdArgs) {hostBodyMode = BodyWhole}
          seat = hostSeat host
      scribe <- findScribe
      TIO.putStrLn $ "🟢 bus agent starting: " <> T.intercalate "," names
      TIO.putStrLn $ "   root: " <> T.pack root
      TIO.putStrLn $ "   command: " <> T.pack cmd <> " " <> T.intercalate " " (map T.pack cmdArgs)
      TIO.putStrLn $ "   scribe: " <> T.pack scribe
      let path = root </> "log.jsonl"
      tailLog path names $ \stored -> do
        replies <- runOne seat stored
        traverse_ (scribePost scribe root) replies
