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
-- 'Post'/'Stamped'/'Jsonl' image.
module Free.Agent.Bus
  ( -- * Bus handle
    Bus,
    busLogPath,
    openBus,
    closeBus,

    -- * Scribe
    scribe,
    scribeIO,

    -- * Durable append (shared with card-archive)
    appendStoredPosts,
    appendStoredPostsUnlocked,
    scribeCard,

    -- * Subscription
    busDeliversTo,
    readSince,
    awaitSince,

    -- * Agent runtime
    runSeatBus,
  )
where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), PostId, sortNub)
import Circuit.Agent.Framing (Jsonl (..), Snoc (..), StoredPost, Stamped (..), These (..), Uncons (..), frameStored, formatNow)
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
import Control.Monad (forever, unless)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.List (maximum)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Seat (FreeSeat, interpretSeat)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>), (<.>))
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.IO (IOMode (AppendMode), withFile)
import Text.Printf (printf)

-- | Live bus handle.
data Bus = Bus
  { -- | In-memory log image, oldest first.
    busLog :: TVar Jsonl,
    -- | Posts waiting to be persisted, each paired with an acknowledgement
    -- 'TMVar' that is filled once the post is on disk.
    busPending :: TQueue (StoredPost, TMVar ()),
    -- | Path to @log.jsonl@.
    busPath :: FilePath,
    -- | Background persistence thread.
    busThread :: ThreadId
  }

-- | Path to the underlying @log.jsonl@.
busLogPath :: Bus -> FilePath
busLogPath = busPath

-- | Open or create a bus at the given root directory.
--
-- Loads any existing @log.jsonl@ into memory and starts the persistence
-- thread. The lock file lives at @root/log.jsonl.lock@.
openBus :: FilePath -> IO Bus
openBus root = do
  createDirectoryIfMissing True root
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ do
    -- Create an empty log file so out-of-process tailers have something to
    -- watch before the first post arrives.
    withFile path AppendMode (\_ -> pure ())
  initial <- Jsonl . T.lines <$> TIO.readFile path
  tv <- newTVarIO initial
  q <- newTQueueIO
  tid <- forkIO (persistLoop path q)
  pure (Bus tv q path tid)

-- | Stop the persistence thread.
--
-- Does not flush pending posts; call this only when durability is not
-- required or after ensuring the log is quiescent.
closeBus :: Bus -> IO ()
closeBus = killThread . busThread

-- | Append a bare post to the live log inside one STM transaction.
--
-- The returned 'StoredPost' carries the absolute line id assigned by the
-- scribe. The caller must supply the timestamp. The returned 'TMVar' is
-- filled once the post has been persisted to disk.
scribe :: Bus -> Text -> Post Text -> STM (StoredPost, TMVar ())
scribe bus ts p = do
  log0 <- readTVar (busLog bus)
  let pid = fromIntegral (length (unJsonl log0))
      stored = Stamped pid ts p
  ack <- newEmptyTMVar
  writeTVar (busLog bus) (snoc log0 stored)
  writeTQueue (busPending bus) (stored, ack)
  pure (stored, ack)

-- | Synchronous scribe: assign the current timestamp, append, and wait for
-- the post to be persisted.
scribeIO :: Bus -> Post Text -> IO StoredPost
scribeIO bus p = do
  ts <- formatNow
  (stored, ack) <- atomically (scribe bus ts p)
  atomically (takeTMVar ack)
  pure stored

-- | Append stamped posts under the exclusive file lock.
--
-- Shared durable image primitive for the live bus persistence loop and the
-- single-writer card archive. Does not assign ids or timestamps.
appendStoredPosts :: FilePath -> [StoredPost] -> IO ()
appendStoredPosts path posts =
  withFileLock (path <.> "lock") Exclusive $ \_lock ->
    appendStoredPostsUnlocked path posts

-- | Append stamped posts without taking the lock. Caller must already hold
-- @path.lock@ (or otherwise guarantee exclusive writers).
appendStoredPostsUnlocked :: FilePath -> [StoredPost] -> IO ()
appendStoredPostsUnlocked path posts =
  withFile path AppendMode $ \h ->
    traverse_ (TIO.hPutStrLn h . frameStored) posts

-- | Single-writer card scribe: assign @postId@ = current line count, stamp
-- @ts@, and append one 'StoredPost' under the file lock. No STM bus image.
--
-- Creates an empty log file when missing. Suitable for batch migration and
-- one-shot archivist appends — not for the multi-consumer live bus.
scribeCard :: FilePath -> Post Text -> IO StoredPost
scribeCard path p = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  unless exists $ withFile path AppendMode (\_ -> pure ())
  withFileLock (path <.> "lock") Exclusive $ \_lock -> do
    pid <- fromIntegral . length . T.lines <$> TIO.readFile path
    ts <- formatNow
    let stored = Stamped pid ts p
    appendStoredPostsUnlocked path [stored]
    pure stored

-- | Free-agent-bus delivery predicate.
--
-- * @to = ["all"]@ broadcasts to every subscriber.
-- * @to = []@ and @to = [""]@ are discard (deliver to no one).
-- * Named recipients deliver to subscribers whose name appears in @to@.
busDeliversTo :: Post a -> [Name] -> Bool
busDeliversTo p subs =
  case to p of
    [] -> False
    [t] | t == T.empty -> False
    ts -> ("all" :: Text) `elem` ts || any (`elem` ts) subs

-- | Read all posts matching any of the names with id greater than the cursor.
--
-- Does not retry; returns an empty list if nothing matches.
readSince :: Bus -> [Name] -> PostId -> STM [StoredPost]
readSince bus names since = do
  log0 <- readTVar (busLog bus)
  let posts = jsonlToList log0
  pure [s | s <- posts, stampId s > since, busDeliversTo (stamped s) names]

-- | Wait until at least one matching post exists after the cursor.
awaitSince :: Bus -> [Name] -> PostId -> STM [StoredPost]
awaitSince bus names since = do
  found <- readSince bus names since
  if null found then retry else pure found

-- | Run a 'FreeSeat' as a bus agent.
--
-- Blocks via 'awaitSince' (STM retry) for posts addressed to any of the
-- names, feeds the batch into the seat, and scribes any emitted replies.
-- This is the callback loop: no polling, no file locks in the agent path.
--
-- Replies carry thread edges citing the parent 'stampId'. To preserve the
-- input-to-output mapping we process one 'StoredPost' at a time; the seat
-- still sees a singleton batch, and the parent id is prepended to each
-- emitted post's 'thread'.
runSeatBus :: Bus -> [Name] -> FreeSeat -> IO ()
runSeatBus bus names seat = loop 0
  where
    sh = interpretSeat seat
    loop lastId = do
      posts <- atomically (awaitSince bus names lastId)
      outs <- concat <$> traverse processOne posts
      traverse_ (scribeIO bus) outs
      loop (maximum (map stampId posts))
    processOne stored = do
      let p = stamped stored
          parentId = stampId stored
      (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
      pure [out {thread = sortNub (parentId : thread out)} | out <- outs]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Drain a 'Jsonl' into a list of parsed posts.
jsonlToList :: Jsonl -> [StoredPost]
jsonlToList = go
  where
    go jl = case uncons jl of
      That _ -> []
      This p -> [p]
      These p rest -> p : go rest

-- | Background persistence loop.
--
-- Batches all posts currently in the queue, appends them under a file lock,
-- writes per-agent ping files, then repeats. Empty queue blocks via
-- 'readTQueue'.
persistLoop :: FilePath -> TQueue (StoredPost, TMVar ()) -> IO ()
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
writePings :: FilePath -> [StoredPost] -> IO ()
writePings path posts = traverse_ writeOne recipients
  where
    root = takeDirectory path
    pingFile name = root </> printf ".ping-%s" (T.unpack name)
    recipients = concatMap postRecipients posts
    postRecipients stored =
      case to (stamped stored) of
        [] -> []
        [""] -> []
        ts -> [(name, stampId stored) | name <- ts]
    writeOne (name, pid) =
      TIO.writeFile (pingFile name) (T.pack (show pid))
