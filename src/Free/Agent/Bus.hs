{-# LANGUAGE OverloadedStrings #-}

-- | STM callback bus over a global JSONL log.
--
-- The live log image is held in a 'TVar'; subscribers block via STM 'retry'
-- until posts matching their names appear. A background thread persists new
-- posts to a single @log.jsonl@ file under a file lock, so the lock never
-- appears in the agent path.
--
-- This is the in-process / single-runtime form of the bus. An out-of-process
-- form can replace the 'TVar' with file-change events without changing the
-- 'Post'/'Stamped'/'Log' image.
--
-- Polymorphic in the body type @a@. File persistence uses 'PostBody' for
-- JSON encoding/decoding at the storage boundary.
module Free.Agent.Bus
  ( -- * Bus handle
    Bus,
    busLogPath,
    openBus,
    closeBus,
    withBus,

    -- * Scribe
    scribe,
    scribeIO,
    postLocal,

    -- * Durable append
    appendStoredPosts,
    appendStoredPostsUnlocked,

    -- * Subscription
    readSince,
    awaitSince,

    -- * Agent runtime
    runSeatBus,
  )
where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost, sortNub)
import Circuit.Agent.Framing
  ( Log (..),
    PostBody,
    Snoc (..),
    Stamped (..),
    Uncons (..),
    encodeLog,
    frameStored,
    readLogFile,
  )
import Circuit.Agent.Mark (Mark (..), isEscalate, isHalt, markGlyph, markOf)
import Control.Arrow (runKleisli)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    isEmptyTQueue,
    newEmptyTMVar,
    newTQueueIO,
    newTVarIO,
    putTMVar,
    readTQueue,
    readTVar,
    retry,
    takeTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (forever, unless)
import Control.Monad.State (runStateT)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.List (maximum)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Free.Agent.Seat (FreeSeat, interpretSeat)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (IOMode (AppendMode), withFile)
import Text.Printf (printf)

-- | Live bus handle, polymorphic in the post body type.
data Bus a = Bus
  { -- | In-memory log image, oldest first.
    busLog :: TVar (Log a),
    -- | Posts waiting to be persisted, each paired with an acknowledgement
    -- 'TMVar' that is filled once the post is on disk.
    busPending :: TQueue (Stamped a, TMVar ()),
    -- | Path to @log.jsonl@.
    busPath :: FilePath,
    -- | Background persistence thread.
    busThread :: ThreadId
  }

-- | Path to the underlying @log.jsonl@.
busLogPath :: Bus a -> FilePath
busLogPath = busPath

-- | Open or create a bus at the given root directory.
--
-- Loads any existing @log.jsonl@ into memory and starts the persistence
-- thread. The lock file lives at @root/log.jsonl.lock@.
openBus :: (PostBody a) => FilePath -> IO (Bus a)
openBus root = do
  createDirectoryIfMissing True root
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ do
    -- Create an empty log file so out-of-process tailers have something to
    -- watch before the first post arrives.
    withFile path AppendMode (\_ -> pure ())
  initial <- if exists then readLogFile path else pure (Log [])
  tv <- newTVarIO initial
  q <- newTQueueIO
  tid <- forkIO (persistLoop path q)
  pure (Bus tv q path tid)

-- | Stop the persistence thread.
--
-- Does not flush pending posts; call this only when durability is not
-- required or after ensuring the log is quiescent.
closeBus :: Bus a -> IO ()
closeBus = killThread . busThread

-- | Bracketed 'openBus'/'closeBus': open a bus, run the action, kill the
-- persistence thread on exit. The seat loop holds one bus for its whole
-- lifetime and scribes replies in-process — no external scribe executable.
withBus :: (PostBody a) => FilePath -> (Bus a -> IO b) -> IO b
withBus root = bracket (openBus root) closeBus

-- | Append a bare post to the live log inside one STM transaction.
--
-- The returned 'Stamped a' carries the absolute line id assigned by the
-- scribe. The caller must supply the timestamp. The returned 'TMVar' is
-- filled once the post has been persisted to disk.
scribe :: Bus a -> UTCTime -> Post a -> STM (Stamped a, TMVar ())
scribe bus ts p = do
  log0 <- readTVar (busLog bus)
  let pid = fromIntegral (length (unLog log0))
      stored = Stamped ts pid p
  ack <- newEmptyTMVar
  writeTVar (busLog bus) (snoc log0 stored)
  writeTQueue (busPending bus) (stored, ack)
  pure (stored, ack)

-- | Synchronous scribe: assign the current timestamp, append, and wait for
-- the post to be persisted.
scribeIO :: Bus a -> Post a -> IO (Stamped a)
scribeIO bus p = do
  ts <- getCurrentTime
  (stored, ack) <- atomically (scribe bus ts p)
  atomically (takeTMVar ack)
  pure stored

-- | File-truth scribe: assign the id from the file itself, under the lock.
--
-- The id is the current line count, read and appended under the exclusive
-- file lock, so concurrent processes can never assign the same id twice.
-- This is the posting path for anything that shares the log with other
-- processes (CLI posts, seat replies). The 'TVar' bus ('scribeIO') is for
-- a single runtime that owns all writes; a long-lived seat's in-memory
-- image goes stale the moment another process posts, and stale images
-- assign colliding ids.
postLocal :: (PostBody a) => FilePath -> Post a -> IO (Stamped a)
postLocal root p = do
  createDirectoryIfMissing True root
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ withFile path AppendMode (\_ -> pure ())
  ts <- getCurrentTime
  stored <- withFileLock (path <.> "lock") Exclusive $ \_lock -> do
    n <- BS.count 0x0A <$> BS.readFile path
    let stored = Stamped ts (fromIntegral n) p
    appendStoredPostsUnlocked path [stored]
    pure stored
  writePings path [stored]
  pure stored

-- | Append stamped posts under the exclusive file lock.
--
-- Shared durable image primitive for the live bus persistence loop. Does not
-- assign ids or timestamps.
appendStoredPosts :: (PostBody a) => FilePath -> [Stamped a] -> IO ()
appendStoredPosts path posts =
  withFileLock (path <.> "lock") Exclusive $ \_lock ->
    appendStoredPostsUnlocked path posts

-- | Append stamped posts without taking the lock. Caller must already hold
-- @path.lock@ (or otherwise guarantee exclusive writers).
appendStoredPostsUnlocked :: (PostBody a) => FilePath -> [Stamped a] -> IO ()
appendStoredPostsUnlocked path posts =
  withFile path AppendMode $ \h ->
    traverse_ (TIO.hPutStrLn h . frameStored) posts

-- | Read all posts matching any of the names with id at or after the cursor.
--
-- The cursor is the next unprocessed id, matching the file-cursor
-- convention. Does not retry; returns an empty list if nothing matches.
readSince :: Bus a -> [Name] -> PostId -> STM [Stamped a]
readSince bus names since = do
  log0 <- readTVar (busLog bus)
  let posts = unLog log0
  pure [s | s <- posts, stamp s >= since, deliversTo (stamped s) names]

-- | Wait until at least one matching post exists after the cursor.
awaitSince :: Bus a -> [Name] -> PostId -> STM [Stamped a]
awaitSince bus names since = do
  found <- readSince bus names since
  if null found then retry else pure found

-- | Run a 'FreeSeat' as a bus agent.
--
-- Blocks via 'awaitSince' (STM retry) for posts addressed to any of the
-- names, feeds the batch into the seat, and scribes any emitted replies.
-- This is the callback loop: no polling, no file locks in the agent path.
--
-- Replies carry thread edges citing the parent 'stamp'. To preserve the
-- input-to-output mapping we process one 'Stamped Text' at a time; the seat
-- still sees a singleton batch, and the parent id is prepended to each
-- emitted post's 'thread'.
--
-- Decided quiet: a delivered post carrying a halt (🟢 / 🔵) or escalation
-- (🔴) mark stops the loop. Marks are control, not content: they are not
-- handed to the seat.
--
-- Self-halt: a 🔵 reply is the seat deciding its own quiet — scribe it and
-- stop, mid-batch if need be; later posts go unanswered (the seat is gone).
-- 🟢 stays exchange-level: a seat may land one exchange and host more.
runSeatBus :: Bus Text -> Name -> [Name] -> FreeSeat -> IO ()
runSeatBus bus agentName names seat = loop 0
  where
    sh = interpretSeat seat
    loop lastId = do
      posts <- atomically (awaitSince bus names lastId)
      let marked = any (halts . stamped) posts
          work = filter (not . halts . stamped) posts
      selfHalt <- go work
      unless (marked || selfHalt) $
        loop (maximum (map stamp posts) + 1)
    go [] = pure False
    go (stored : rest) = do
      outs <- processOne stored
      let keep p = not (T.null (T.strip (body p)))
          nonEmpty = filter keep outs
      traverse_ (scribeIO bus) nonEmpty
      if any ((== Just StandDown) . markOf) outs
        then pure True
        else go rest
    halts p = maybe False (\m -> isHalt m || isEscalate m) (markOf p)
    processOne stored = do
      er <-
        try @SomeException $ do
          let p = stamped stored
              parentId = stamp stored
          (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
          pure [out {thread = sortNub (parentId : thread out)} | out <- outs]
      case er of
        Left e -> do
          let p = stamped stored
              exc = T.pack (displayException e)
              msg = markGlyph Escalate <> " handler failed: " <> exc
          _ <- scribeIO bus (mkPost agentName [from p] msg)
          pure []
        Right outs -> pure outs

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Background persistence loop.
--
-- Batches all posts currently in the queue, appends them under a file lock,
-- writes per-agent ping files, then repeats. Empty queue blocks via
-- 'readTQueue'.
persistLoop :: (PostBody a) => FilePath -> TQueue (Stamped a, TMVar ()) -> IO ()
persistLoop path q = forever $ do
  pairs <- atomically $ do
    first <- readTQueue q
    rest <- drainQueue
    pure (first : rest)
  let posts = map fst pairs
  appendStoredPosts path posts
  writePings path posts
  atomically $ traverse_ (\(_, ack) -> putTMVar ack ()) pairs
  where
    drainQueue = do
      empty <- isEmptyTQueue q
      if empty
        then pure []
        else do
          x <- readTQueue q
          xs <- drainQueue
          pure (x : xs)

-- | Write a @.ping-NAME@ file for each named recipient of each post.
--
-- The file contains the latest post id addressed to that recipient. Agents can
-- watch this file cheaply instead of re-parsing the log on every wake.
writePings :: FilePath -> [Stamped a] -> IO ()
writePings path posts = traverse_ writeOne recipients
  where
    root = takeDirectory path
    pingFile name = root </> printf ".ping-%s" (T.unpack name)
    recipients = concatMap postRecipients posts
    postRecipients stored =
      case to (stamped stored) of
        [] -> []
        [""] -> []
        ts -> [(name, stamp stored) | name <- ts]
    writeOne (name, pid) =
      TIO.writeFile (pingFile name) (T.pack (show pid))
