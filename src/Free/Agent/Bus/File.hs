{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Out-of-process bus helpers: file cursors and an fsnotify-based tail
-- loop. Used by the long-running agent executables that watch the JSONL log
-- directly instead of sharing STM state with the scribe.
module Free.Agent.Bus.File
  ( -- * Cursor files
    cursorPath,
    readCursor,
    writeCursor,

    -- * Event-tail loop
    QuiesceConfig (..),
    Flow (..),
    tailLog,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo)
import Circuit.Agent.Framing (Stamped, stamp, stamped, unframeStored)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    check,
    newTMVarIO,
    newTVarIO,
    orElse,
    readTVar,
    readTVarIO,
    takeTMVar,
    tryPutTMVar,
    writeTVar,
  )
import Control.Monad (guard, unless, when)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FSNotify (Event (..), watchDir, withManager)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO
  ( IOMode (AppendMode, ReadMode),
    SeekMode (AbsoluteSeek),
    hSeek,
    withFile,
  )
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

-- | Flow control returned by a 'tailLog' callback: keep listening, or halt
-- the loop after this post. This is decided quiet at the seat level: a
-- callback that returns 'Halt' ends the exchange by content, not by
-- timeout.
data Flow = Continue | Halt
  deriving (Eq, Show)

-- | Path to the cursor file for an agent.
--
-- The cursor stores the next 'postId' the agent should process, so restarts
-- catch up without re-reading the whole log. A missing cursor defaults to 0,
-- meaning "start from the first post".
cursorPath :: FilePath -> Name -> FilePath
cursorPath root name = root </> (".cursor-" <> T.unpack name)

-- | Read the cursor for an agent. If no cursor file exists, defaults to the
-- latest post id + 1 in the bus log so the agent skips history on cold boot.
-- Returns 0 only when the log itself is absent or empty.
readCursor :: FilePath -> Name -> IO PostId
readCursor root name = do
  let path = cursorPath root name
  exists <- doesFileExist path
  if not exists
    then latestPostId root
    else do
      txt <- TIO.readFile path
      pure $ maybe 0 fromIntegral (readMaybe @Integer (T.unpack (T.strip txt)))

-- | Return the id that would follow the last post in the log (i.e. the
-- post count), or 0 if the log doesn't exist or is empty. Used as the
-- default cursor on cold boot so the agent only processes new posts.
latestPostId :: FilePath -> IO PostId
latestPostId root = do
  let logPath = root </> "log.jsonl"
  logExists <- doesFileExist logPath
  if not logExists
    then pure 0
    else do
      txt <- TIO.readFile logPath
      let ls = filter (not . T.null) (T.lines txt)
      case ls of
        [] -> pure 0
        _ -> case unframeStored @Text (last ls) of
          Just stored -> pure (snd (stamp stored) + 1)
          Nothing -> pure (fromIntegral (length ls))

-- | Persist the cursor for an agent. Writes @stamp + 1@ so the next wake
-- starts after the post just processed.
writeCursor :: FilePath -> Name -> PostId -> IO ()
writeCursor root name pid =
  TIO.writeFile (cursorPath root name) (T.pack (show pid))

-- | Event-tail a log file and invoke the callback for every new stored post
-- addressed to any of the subscribed names.
--
-- On startup the file is scanned from the beginning; posts with 'stamp'
-- greater than or equal to the supplied cursor and addressed to any subscribed
-- name are delivered. After catch-up, fsnotify wakes a drain for new lines.
--
-- Reading is offset-based: each drain opens the file, reads the complete
-- lines appended since the last offset, and closes the handle /before/
-- invoking callbacks. A partial trailing line (writer mid-append) is left
-- for the next drain. The handle must be closed before callbacks run
-- because callbacks may append to the log in-process, and GHC locks files
-- per process — a held read handle makes the append fail with
-- "resource busy (file is locked)".
--
-- When a quiescence config is supplied, the main loop waits on a signal with a
-- timeout instead of blocking forever. Each timeout without a signal increments
-- an empty-cycle counter; a signal resets it. After the configured number of
-- empty cycles, the provided action is run and the loop exits.
--
-- The loop also exits when a callback returns 'Halt'.
tailLog ::
  -- | Path to @log.jsonl@.
  FilePath ->
  -- | Subscribed names.
  [Name] ->
  -- | Starting cursor.
  PostId ->
  -- | Optional quiescence config and action.
  Maybe (QuiesceConfig, IO ()) ->
  -- | Callback for each delivered stored post; return 'Halt' to stop.
  (Stamped Text -> IO Flow) ->
  IO ()
tailLog path names startCursor mQuiesce cb = do
  exists <- doesFileExist path
  unless exists $ do
    -- Touch an empty log so the scribe has a file to append to.
    withFile path AppendMode (\_ -> pure ())
  (off0, halted0) <- drainFrom 0 (filterStoredSince startCursor)
  offRef <- newIORef off0
  halted <- newTVarIO halted0
  let logName = takeFileName path
      dir = takeDirectory path
  withManager $ \mgr -> do
    signal <- newTMVarIO ()
    busy <- newTVarIO True -- quiescence gated: don't count empty cycles until first drain completes
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      already <- readTVarIO halted
      unless already $ do
        atomically $ writeTVar busy True
        off <- readIORef offRef
        (off', h) <- drainFrom off filterStored
        writeIORef offRef off'
        atomically $ do
          when h (writeTVar halted True)
          writeTVar busy False
          _ <- tryPutTMVar signal ()
          pure ()
    case mQuiesce of
      Nothing -> pollLoop offRef signal halted
      Just (qc, onQuiesce) -> quiesceLoop qc signal busy halted 0 onQuiesce
  where
    -- \| No-quiesce poll loop: wait on fsnotify with a 1 s timeout, then
    -- manually re-drain.  FSEvents on macOS does not reliably fire on
    -- append, so the timeout fallback ensures delivery within ~1 s.
    pollLoop offRef signal halted = do
      h <- readTVarIO halted
      if h
        then pure ()
        else do
          m <- timeout 1_000_000 (atomically (takeTMVar signal))
          case m of
            Just () -> do
              off <- readIORef offRef
              (off', h') <- drainFrom off filterStored
              writeIORef offRef off'
              when h' (atomically (writeTVar halted True))
            Nothing -> do
              -- timeout: poll the file manually
              off <- readIORef offRef
              (off', h') <- drainFrom off filterStored
              writeIORef offRef off'
              when h' (atomically (writeTVar halted True))
          pollLoop offRef signal halted
    drainFrom off filt = do
      (ls, off') <- readCompleteLines off
      halted <- deliver (mapMaybe filt ls)
      pure (off', halted)

    -- Deliver posts oldest first; stop early on 'Halt'.
    deliver [] = pure False
    deliver (p : ps) = do
      flow <- cb p
      case flow of
        Halt -> pure True
        Continue -> deliver ps

    readCompleteLines off = withFile path ReadMode $ \h -> do
      hSeek h AbsoluteSeek off
      bs <- BS.hGetContents h
      pure (completeLines off bs)

    -- Lines up to the last newline are complete; the byte offset advances
    -- past it. Anything after the last newline is a writer mid-append and
    -- waits for the next drain.
    completeLines off bs =
      case BS.elemIndexEnd 0x0A bs of
        Nothing -> ([], off)
        Just i ->
          ( T.lines (TE.decodeUtf8 (BS.take (i + 1) bs)),
            off + fromIntegral i + 1
          )

    filterStoredSince cursor line = do
      stored <- unframeStored @Text line
      guard (snd (stamp stored) >= cursor)
      guard (deliversTo (stamped stored) names)
      pure stored

    filterStored line = do
      stored <- unframeStored @Text line
      if deliversTo (stamped stored) names then Just stored else Nothing

    -- Wait for a halt or a new-posts signal, whichever lands first.
    awaitEvent signal halted =
      (readTVar halted >>= check >> pure True)
        `orElse` (takeTMVar signal >> pure False)

    quiesceLoop qc signal busy halted count onQuiesce = do
      m <- timeout (qcCycleMicros qc) (atomically (awaitEvent signal halted))
      case m of
        Just True -> pure ()
        Just False -> quiesceLoop qc signal busy halted 0 onQuiesce
        Nothing -> do
          inProgress <- readTVarIO busy
          if inProgress
            then quiesceLoop qc signal busy halted 0 onQuiesce
            else do
              let count' = count + 1
              if count' >= qcCycles qc
                then onQuiesce
                else quiesceLoop qc signal busy halted count' onQuiesce
