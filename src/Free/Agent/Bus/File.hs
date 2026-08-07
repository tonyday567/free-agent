{-# LANGUAGE OverloadedStrings #-}

-- | Out-of-process bus helpers: file cursors, scribe invocation, and an
-- fsnotify-based tail loop. Used by the long-running agent executables that
-- watch the JSONL log directly instead of sharing STM state with the scribe.
module Free.Agent.Bus.File
  ( -- * Cursor files
    cursorPath,
    readCursor,
    writeCursor,

    -- * Scribe executable
    findScribe,
    scribePost,

    -- * Event-tail loop
    QuiesceConfig (..),
    tailLog,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo)
import Circuit.Agent.Framing (StoredPost, framePost, parseLine, stampId, stamped)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Monad (filterM, forever, guard, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FSNotify (Event (..), watchDir, withManager)
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
    withFile,
  )
import System.Process (readProcess)
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | Quiescence configuration for long-running agents.
data QuiesceConfig = QuiesceConfig
  { -- | Number of empty cycles before taking action.
    qcCycles :: Int,
    -- | Recipient name for the quiescence marker.
    qcPitboss :: Name,
    -- | Length of one cycle in microseconds.
    qcCycleMicros :: Int
  }
  deriving (Show)

-- | Path to the cursor file for an agent.
--
-- The cursor stores the next 'postId' the agent should process, so restarts
-- catch up without re-reading the whole log. A missing cursor defaults to 0,
-- meaning "start from the first post".
cursorPath :: FilePath -> Name -> FilePath
cursorPath root name = root </> (".cursor-" <> T.unpack name)

-- | Read the cursor for an agent. Returns 0 if no cursor exists yet.
readCursor :: FilePath -> Name -> IO PostId
readCursor root name = do
  let path = cursorPath root name
  exists <- doesFileExist path
  if not exists
    then pure 0
    else do
      txt <- TIO.readFile path
      pure $ maybe 0 fromIntegral (readMaybe @Integer (T.unpack (T.strip txt)))

-- | Persist the cursor for an agent. Writes @stampId + 1@ so the next wake
-- starts after the post just processed.
writeCursor :: FilePath -> Name -> PostId -> IO ()
writeCursor root name pid =
  TIO.writeFile (cursorPath root name) (T.pack (show pid))

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
          error "free-agent-bus not found"

-- | Scribe one post by invoking the external scribe executable.
scribePost :: (MonadIO m) => FilePath -> FilePath -> Post Text -> m ()
scribePost scribe root p = liftIO $ do
  _ <- readProcess scribe ["post", "--root", root] (T.unpack (framePost p))
  pure ()

-- | Event-tail a log file and invoke the callback for every new stored post
-- addressed to any of the subscribed names.
--
-- On startup the file is scanned from the beginning; posts with 'stampId'
-- greater than or equal to the supplied cursor and addressed to any subscribed
-- name are delivered. After catch-up the handle is parked at EOF and fsnotify
-- wakes it for new lines.
--
-- When a quiescence config is supplied, the main loop waits on a signal with a
-- timeout instead of blocking forever. Each timeout without a signal increments
-- an empty-cycle counter; a signal resets it. After the configured number of
-- empty cycles, the provided action is run and the loop exits.
tailLog ::
  -- | Path to @log.jsonl@.
  FilePath ->
  -- | Subscribed names.
  [Name] ->
  -- | Starting cursor.
  PostId ->
  -- | Optional quiescence config and action.
  Maybe (QuiesceConfig, IO ()) ->
  -- | Callback for each delivered stored post.
  (StoredPost -> IO ()) ->
  IO ()
tailLog path names startCursor mQuiesce cb = do
  exists <- doesFileExist path
  unless exists $ do
    -- Touch an empty log so the scribe has a file to append to.
    withFile path AppendMode (\_ -> pure ())
  h <- openFile path ReadMode
  hSetBuffering h LineBuffering
  -- Catch up on any posts missed while this agent was offline.
  catchUp h startCursor
  -- Park at EOF and wait for new posts via fsnotify.
  size <- hFileSize h
  hSeek h AbsoluteSeek size
  let logName = takeFileName path
      dir = takeDirectory path
  withManager $ \mgr -> do
    signal <- newEmptyMVar
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      drain h names cb
      _ <- tryPutMVar signal ()
      pure ()
    case mQuiesce of
      Nothing -> forever (threadDelay 1000000)
      Just (qc, onQuiesce) -> quiesceLoop qc signal 0 onQuiesce
  where
    catchUp h cursor = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ cb (filterStoredSince names cursor line)
        catchUp h cursor

    drain h names' cb' = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ cb' (filterStored names' line)
        drain h names' cb'

    filterStoredSince names' cursor line = do
      stored <- parseLine line
      guard (stampId stored >= cursor)
      guard (deliversTo (stamped stored) names')
      pure stored

    filterStored :: [Name] -> Text -> Maybe StoredPost
    filterStored names' line = do
      stored <- parseLine line
      if deliversTo (stamped stored) names' then Just stored else Nothing

    quiesceLoop qc signal count onQuiesce = do
      m <- timeout (qcCycleMicros qc) (takeMVar signal)
      case m of
        Just () -> quiesceLoop qc signal 0 onQuiesce
        Nothing -> do
          let count' = count + 1
          if count' >= qcCycles qc
            then onQuiesce
            else quiesceLoop qc signal count' onQuiesce
