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

    -- * Quiescence machine
    QEvent (..),
    Decision (..),
    quiesceAgent,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo)
import Circuit.Agent.Framing (Stamped, stamp, stamped, unframeStored)
import Circuit.Agent.Mark (Mark (Timeout), markGlyph)
import Circuit.Machine (Body (..), Moore)
import Circuit.Tensor (Const (Const), unConst)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
  ( TMVar,
    TVar,
    atomically,
    check,
    modifyTVar',
    newEmptyTMVarIO,
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

-- | Quiescence percepts, classified from drains by the pump in 'tailLog'.
-- 'Tick' ends an empty cycle — a wait that woke with nothing addressed
-- delivered; 'Delivered' is an addressed post drained to the callback;
-- 'Activity' is an append not addressed to this seat; 'Halted' is the end
-- of the exchange — the pump exits rather than feeding it.
data QEvent = Tick | Delivered | Activity | Halted
  deriving (Eq, Show)

-- | The quiescence verdict after the configured number of consecutive
-- empty cycles: the body of the stand-down post. It carries the ⌛
-- 'Timeout' mark — a timeout is an inference from wall-clock silence,
-- not a decided quiet (🔵).
newtype Decision = Decision {decisionBody :: Text}
  deriving (Eq, Show)

-- | The quiescence machine: a pure Moore over 'QEvent'. N consecutive
-- 'Tick's with no 'Delivered' emit the verdict once; 'Delivered' resets
-- the count; 'Activity' — chatter to other seats — does not; 'Halted'
-- is inert. Time is a percept: the pump feeds ticks, no wall clock
-- lives here.
quiesceAgent :: Int -> Moore (,) Int (->) QEvent (Maybe Decision)
quiesceAgent n = Body (\i -> step (0, i)) step (Const . decide)
  where
    step (count, i) = case i of
      Tick -> min n (count + 1)
      Delivered -> 0
      Activity -> count
      Halted -> count
    decide count
      | count >= n =
          Just (Decision (markGlyph Timeout <> " standing down after " <> T.pack (show n) <> " empty cycles"))
      | otherwise = Nothing

-- | One machine tick: the next state.
stepQ :: Moore (,) Int (->) QEvent (Maybe Decision) -> Int -> QEvent -> Int
stepQ (Body _ k _) s i = k (s, i)

-- | The observation at a state.
observeQ :: Moore (,) Int (->) QEvent (Maybe Decision) -> Int -> Maybe Decision
observeQ (Body _ _ o) s = unConst (o s)

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
-- name are delivered. After catch-up, fsnotify wakes a drain for new lines,
-- and one more drain runs after watch registration so a post appended in
-- the registration gap is not stranded.
--
-- Reading is offset-based: each drain opens the file, reads the complete
-- lines appended since the last offset, and closes the handle /before/
-- invoking callbacks. A partial trailing line (writer mid-append) is left
-- for the next drain. The handle must be closed before callbacks run
-- because callbacks may append to the log in-process, and GHC locks files
-- per process — a held read handle makes the append fail with
-- "resource busy (file is locked)".
--
-- When a quiescence config is supplied, a pump interprets 'quiesceAgent'
-- at the wait boundary: each wake re-drains (a missed FSEvent must not
-- strand an addressed post), a wake that delivered nothing addressed feeds
-- one 'Tick', unaddressed appends feed 'Activity' (which never resets the
-- count), and deliveries feed 'Delivered'. After the configured number of
-- consecutive empty cycles the pump emits the machine's 'Decision' to the
-- action and the loop exits — so a silent bus stands down while a chatty
-- one does not star.
--
-- The loop also exits when a callback returns 'Halt'.
tailLog ::
  -- | Path to @log.jsonl@.
  FilePath ->
  -- | Subscribed names.
  [Name] ->
  -- | Starting cursor.
  PostId ->
  -- | Optional quiescence config and verdict action.
  Maybe (QuiesceConfig, Decision -> IO ()) ->
  -- | Callback for each delivered stored post; return 'Halt' to stop.
  (Stamped Text -> IO Flow) ->
  IO ()
tailLog path names startCursor mQuiesce cb = do
  exists <- doesFileExist path
  unless exists $ do
    -- Touch an empty log so the scribe has a file to append to.
    withFile path AppendMode (\_ -> pure ())
  (off0, halted0, _, _) <- drainFrom 0 (filterStoredSince startCursor)
  offRef <- newIORef off0
  halted <- newTVarIO halted0
  lock <- newMVar ()
  let logName = takeFileName path
      dir = takeDirectory path
  withManager $ \mgr -> do
    signal <- newEmptyTMVarIO
    -- Addressed posts the watcher drained since the pump last classified
    -- a wake; only deliveries reset the quiescence count.
    delivered <- newTVarIO 0 :: IO (TVar Int)
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      already <- readTVarIO halted
      unless already $ do
        (h, _, nDeliv) <- drainLocked lock offRef filterStored
        atomically $ do
          modifyTVar' delivered (+ nDeliv)
          when h (writeTVar halted True)
          _ <- tryPutTMVar signal ()
          pure ()
    -- Registration gap: a post appended before the watch was registered
    -- fired no event; read it by offset.
    (h0, _, _) <- drainLocked lock offRef filterStored
    atomically $ when h0 (writeTVar halted True)
    case mQuiesce of
      Nothing -> pollLoop lock offRef signal halted
      Just (qc, onQuiesce) -> quiescePump qc signal delivered halted lock offRef onQuiesce
  where
    -- \| No-quiesce poll loop: wait on fsnotify with a 1 s timeout, then
    -- manually re-drain.  FSEvents on macOS does not reliably fire on
    -- append, so the timeout fallback ensures delivery within ~1 s.
    pollLoop lock offRef signal halted = do
      h <- readTVarIO halted
      if h
        then pure ()
        else do
          m <- timeout 1_000_000 (atomically (takeTMVar signal))
          case m of
            Just () -> do
              (h', _, _) <- drainLocked lock offRef filterStored
              when h' (atomically (writeTVar halted True))
            Nothing -> do
              -- timeout: poll the file manually
              (h', _, _) <- drainLocked lock offRef filterStored
              when h' (atomically (writeTVar halted True))
          pollLoop lock offRef signal halted
    -- Deliveries are serialized: the offset is read, the log drained,
    -- and the offset advanced under one lock, so the fsnotify watcher
    -- and the poll fallback cannot both deliver a post when a callback
    -- runs longer than the poll interval. The drain also reports how many
    -- complete lines it saw and how many addressed posts it delivered, so
    -- the quiescence pump can tell delivery from chatter.
    drainLocked lock offRef filt = withMVar lock $ \_ -> do
      off <- readIORef offRef
      (off', h, nAll, nDeliv) <- drainFrom off filt
      writeIORef offRef off'
      pure (h, nAll, nDeliv)
    drainFrom off filt = do
      (ls, off') <- readCompleteLines off
      (nDeliv, halted) <- deliver (mapMaybe filt ls)
      pure (off', halted, length ls, nDeliv)

    -- Deliver posts oldest first; stop early on 'Halt'.
    deliver [] = pure (0, False)
    deliver (p : ps) = do
      flow <- cb p
      case flow of
        Halt -> pure (1, True)
        Continue -> do
          (n, h) <- deliver ps
          pure (n + 1, h)

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

    -- The quiescence pump: the IO interpretation of 'quiesceAgent' at the
    -- wait boundary. Every wake re-drains, then classifies the wake for
    -- the machine: deliveries feed 'Delivered', unaddressed appends feed
    -- 'Activity' before the tick, and a wake that delivered nothing
    -- addressed feeds one 'Tick'. The verdict runs the action once and
    -- the pump exits.
    quiescePump qc signal delivered halted lock offRef onQuiesce = go 0
      where
        sys = quiesceAgent (qcCycles qc)
        go count = do
          h <- readTVarIO halted
          if h
            then pure ()
            else do
              m <- timeout (qcCycleMicros qc) (atomically (awaitEvent signal halted))
              case m of
                Just True -> pure ()
                _ -> do
                  (h', nAll, nDelivLocal) <- drainLocked lock offRef filterStored
                  if h'
                    then atomically (writeTVar halted True)
                    else do
                      nDeliv <- atomically $ do
                        n <- readTVar delivered
                        writeTVar delivered 0
                        pure (n + nDelivLocal)
                      let count'
                            | nDeliv > 0 = stepQ sys count Delivered
                            | nAll > 0 = stepQ sys (stepQ sys count Activity) Tick
                            | otherwise = stepQ sys count Tick
                      case observeQ sys count' of
                        Just d -> onQuiesce d
                        Nothing -> go count'
