{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | STM callback bus over a global JSONL log.
--
-- The live log image is held in a 'TVar'; subscribers block via STM 'retry'
-- until posts matching their names appear /in the persisted prefix/ — a
-- seat can never react to a post that is not on disk. A background thread
-- (the persister, itself a seat) watches the watermark 'busDurable': ids
-- below it are on disk. It appends the not-yet-durable delta to
-- @log.jsonl@ under the file lock, so the lock never appears in the agent
-- path, and pulls external growth (another process posting to the same
-- root) into the image so ids stay file-positional.
--
-- Waiting is a predicate over monotone facts, never a handoff: durability
-- is observed via 'awaitDurable' (durable or 'Failed', composable with
-- 'orElse'), and persister death is content — any exit of the persist
-- thread writes 'Failed' to 'busPersist'.
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
    busImage,
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
    writePings,

    -- * Durability
    Persist (..),
    awaitDurable,
    awaitQuiescent,

    -- * Subscription
    readSince,
    awaitSince,

    -- * Agent runtime
    runSeatBus,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost, sortNub)
import Circuit.Agent.Framing
  ( Log (..),
    PostBody,
    Snoc (..),
    Stamped,
    Uncons (..),
    encodeLog,
    frameStored,
    readLogFile,
    stamp,
    stamped,
    pattern Stamped,
  )
import Circuit.Agent.Mark (Mark (..), isEscalate, isHalt, markGlyph, markOf)
import Circuit.Agent.Tensor (closeShardIO)
import Control.Concurrent (ThreadId, forkFinally, killThread)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, bracket, displayException, toException, try)
import Control.Monad (unless)
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
  { -- | In-memory log image, oldest first. Includes the not-yet-durable
    -- tail — subscribers never see it, the persister works from it.
    busLog :: TVar (Log a),
    -- | Durable watermark: posts with id below it are on disk.
    busDurable :: TVar PostId,
    -- | Persister health: 'Persisting', or 'Failed' with the exit reason.
    busPersist :: TVar Persist,
    -- | Path to @log.jsonl@.
    busPath :: FilePath,
    -- | Background persistence thread.
    busThread :: ThreadId
  }

-- | Persister health. Death is content: any exit of the persist thread
-- writes 'Failed'.
data Persist = Persisting | Failed SomeException

-- | Path to the underlying @log.jsonl@.
busLogPath :: Bus a -> FilePath
busLogPath = busPath

-- | The in-memory image, oldest first — including posts not yet durable.
busImage :: Bus a -> STM (Log a)
busImage = readTVar . busLog

-- | Open or create a bus at the given root directory.
--
-- Loads any existing @log.jsonl@ into memory and starts the persistence
-- thread. The watermark starts past the loaded image: everything seen at
-- open is already on disk. The lock file lives at @root/log.jsonl.lock@.
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
  dur <- newTVarIO (fromIntegral (length (unLog initial)))
  st <- newTVarIO Persisting
  tid <- forkFinally (persistLoop path tv dur) $ \r ->
    atomically $ writeTVar st $ case r of
      Left e -> Failed e
      Right () -> Failed (toException (userError "Free.Agent.Bus: persist loop exited"))
  pure (Bus tv dur st path tid)

-- | Stop the persistence thread.
--
-- Does not flush pending posts; call this only when durability is not
-- required or after ensuring the log is quiescent ('awaitQuiescent').
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
-- scribe. The caller must supply the timestamp. There is no ack: the post
-- is durable when the watermark passes it — observe that with
-- 'awaitDurable', a predicate, not a handoff.
scribe :: Bus a -> UTCTime -> Post a -> STM (Stamped a)
scribe bus ts p = do
  log0 <- readTVar (busLog bus)
  let pid = fromIntegral (length (unLog log0))
      stored = Stamped (ts, pid) p
  writeTVar (busLog bus) (snoc log0 stored)
  pure stored

-- | Synchronous scribe: assign the current timestamp and append to the
-- live image. Returns immediately — persistence runs in the background;
-- callers that need the post on disk await 'awaitDurable'.
scribeIO :: Bus a -> Post a -> IO (Stamped a)
scribeIO bus p = do
  ts <- getCurrentTime
  atomically (scribe bus ts p)

-- | Wait until the post id is durable, or the persister has failed.
--
-- Ends by content — durable or 'Failed', whichever the monotone facts
-- reach first — so it composes with 'orElse' and can never hang: a dead
-- persister answers 'Left'.
awaitDurable :: Bus a -> PostId -> STM (Either SomeException ())
awaitDurable bus pid = do
  st <- readTVar (busPersist bus)
  case st of
    Failed e -> pure (Left e)
    Persisting -> do
      d <- readTVar (busDurable bus)
      if d > pid then pure (Right ()) else retry

-- | Wait until every post in the image is durable, or the persister has
-- failed. The quiescence check behind a clean shutdown.
awaitQuiescent :: Bus a -> STM (Either SomeException ())
awaitQuiescent bus = do
  img <- readTVar (busLog bus)
  d <- readTVar (busDurable bus)
  st <- readTVar (busPersist bus)
  case st of
    Failed e -> pure (Left e)
    Persisting
      | d >= fromIntegral (length (unLog img)) -> pure (Right ())
      | otherwise -> retry

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
    let stored = Stamped (ts, fromIntegral n) p
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

-- | Read all posts matching any of the names with id at or after the
-- cursor, /in the persisted prefix/.
--
-- The cursor is the next unprocessed id, matching the file-cursor
-- convention. Does not retry; returns an empty list if nothing matches.
-- Durable visibility: a subscriber can never observe a post that is not
-- on disk — the in-process bus is the same log as the file bus.
readSince :: Bus a -> [Name] -> PostId -> STM [Stamped a]
readSince bus names since = do
  log0 <- readTVar (busLog bus)
  d <- readTVar (busDurable bus)
  let posts = unLog log0
  pure
    [ s
    | s <- posts,
      let i = snd (stamp s),
      i >= since && i < d,
      deliversTo (stamped s) names
    ]

-- | Wait until at least one matching post exists in the persisted prefix
-- after the cursor.
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
        loop (maximum (map (snd . stamp) posts) + 1)
    go [] = pure False
    go (stored : rest) = do
      outs <- processOne stored
      let keep p =
            let b = T.strip (body p)
             in not (T.null b) && b /= "(empty)"
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
              parentId = snd (stamp stored)
          (outs, _st) <- closeShardIO sh [p] []
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

-- | Background persistence: the persister is a seat and the watermark is
-- its cursor.
--
-- Blocks in STM on @length log > durable@, then appends the not-yet-durable
-- delta under the file lock and advances the watermark. Batches for free:
-- every post scribed while it works lands in the next delta.
--
-- External growth — the file longer than the watermark from another
-- process on the same root — is reconciled, not collided with: the
-- external posts are pulled into the image, and the not-yet-durable tail
-- is renumbered past them (safe: readers only see the durable prefix, so
-- the tail's ids are invisible until reassigned). Ids stay file-positional
-- on every interleaving of 'scribeIO' and 'postLocal'. A file that shrank
-- below the watermark is corruption: fail loudly with a named error.
--
-- Never returns; any exit (via 'forkFinally' in 'openBus') writes
-- 'Failed' — death is content.
persistLoop :: (PostBody a) => FilePath -> TVar (Log a) -> TVar PostId -> IO ()
persistLoop path logVar durVar = loop
  where
    lockPath = path <.> "lock"
    loop = do
      (d, image) <- atomically $ do
        d <- readTVar durVar
        image <- readTVar logVar
        check (fromIntegral (length (unLog image)) > d)
        pure (d, image)
      withFileLock lockPath Exclusive $ \_lock -> do
        m <- countLines path
        case compare m d of
          LT -> do
            ioError
              ( userError
                  "Free.Agent.Bus: log.jsonl shrank below the durable watermark — external truncation?"
              )
          EQ -> do
            let delta = drop (fromIntegral d) (unLog image)
            appendStoredPostsUnlocked path delta
            writePings path delta
            atomically (writeTVar durVar (d + fromIntegral (length delta)))
          GT -> do
            full <- readLogFile path
            let ext = drop (fromIntegral d) (unLog full)
            renumbered <- atomically $ do
              img <- unLog <$> readTVar logVar
              let (kept, tailOld) = splitAt (fromIntegral d) img
                  renum =
                    [ Stamped (ts, m + fromIntegral i) p
                    | (i, Stamped (ts, _) p) <- zip [0 :: Int ..] tailOld
                    ]
              writeTVar logVar (Log (kept ++ ext ++ renum))
              writeTVar durVar m
              pure renum
            appendStoredPostsUnlocked path renumbered
            writePings path renumbered
            atomically (writeTVar durVar (m + fromIntegral (length renumbered)))
      loop

-- | Number of complete lines in the log file.
countLines :: FilePath -> IO PostId
countLines path = fromIntegral . BS.count 0x0A <$> BS.readFile path

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
        ts -> [(name, snd (stamp stored)) | name <- ts]
    writeOne (name, pid) =
      TIO.writeFile (pingFile name) (T.pack (show pid))
