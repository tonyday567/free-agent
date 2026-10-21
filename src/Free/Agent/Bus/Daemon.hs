{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The stamping office: a single-writer daemon for the bus.
--
-- The bus has two stamping regimes:
--
--   * Office off: writers self-stamp with 'postLocal' — the entangled case,
--     where concurrent writers may race for ids.
--   * Office on: writers send bare 'Post' lines to the daemon over a named
--     pipe (@root/bus.fifo@); the daemon stamps them in arrival order and
--     appends to @log.jsonl@. This serialisation is the bus's ⅋ discipline
--     made into a process.
--
-- The daemon is intentionally stateless: it assigns ids by counting lines in
-- the log under the same exclusive lock that 'postLocal' uses, so the two
-- regimes can coexist during a transition without colliding.
module Free.Agent.Bus.Daemon
  ( -- * Daemon
    runDaemon,
    ensureFifo,

    -- * Client
    postViaDaemon,

    -- * Paths
    fifoPath,
    receiptPath,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost, sortNub)
import Circuit.Agent.Framing
  ( PostBody,
    Stamped,
    framePost,
    frameStored,
    parsePost,
    stamp,
    stamped,
    unframeStored,
    pattern Stamped,
  )
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, catch, displayException, try)
import Control.Monad (forever, unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Free.Agent.Bus (appendStoredPostsUnlocked, readFileStrict, touchLog, withBusFiles, withLogLock, writePings)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (AppendMode, ReadWriteMode),
    hFlush,
    hGetLine,
    hSetBuffering,
    stderr,
    withFile,
  )
import System.IO.Error (isAlreadyExistsError, isDoesNotExistError)
import System.Posix.Files (createNamedPipe, getFileStatus, isNamedPipe)
import Text.Printf (printf)

-- | Path to the named pipe used to submit bare posts to the daemon.
fifoPath :: FilePath -> FilePath
fifoPath root = root </> "bus.fifo"

-- | Path to the per-sender receipt file. The daemon overwrites this with the
-- latest stamped post from that sender; the client waits for a receipt that
-- matches its submission.
receiptPath :: FilePath -> Name -> FilePath
receiptPath root name = root </> printf ".receipt-%s" (T.unpack name)

-- | Run the stamping office forever.
--
-- Creates the bus directory, touches @log.jsonl@, ensures the FIFO exists
-- ('ensureFifo'), then reads one bare 'Post' line at a time. Malformed lines
-- are reported on stderr and skipped; valid lines are stamped and appended.
runDaemon :: forall a. (PostBody a) => FilePath -> IO ()
runDaemon root = do
  createDirectoryIfMissing True root
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ touchLog path
  ensureFifo (fifoPath root)
  withFile (fifoPath root) ReadWriteMode $ \h -> do
    hSetBuffering h LineBuffering
    forever $ do
      line <- T.pack <$> hGetLine h
      case parsePost @a line of
        Nothing -> TIO.hPutStrLn stderr ("🔴 daemon: invalid post JSON: " <> line)
        Just p -> do
          stored <- stampOne path p
          writeReceipt root (from p) (frameStored stored)
          writePings path [stored]

-- | Create the submission FIFO if absent. Idempotent: if the path already
-- exists it must actually be a named pipe — a regular file there is a
-- loud failure (wave H: a stale or hijacked path must never be read as a
-- queue).
ensureFifo :: FilePath -> IO ()
ensureFifo fifo = do
  r <- try @IOException (createNamedPipe fifo 0o600)
  case r of
    Right () -> pure ()
    Left e
      | isAlreadyExistsError e -> do
          st <- getFileStatus fifo
          unless (isNamedPipe st) $
            ioError
              ( userError
                  ("🔴 " <> fifo <> " exists and is not a named pipe; refusing to run the daemon on it")
              )
      | otherwise -> ioError e

-- | Stamp a single post and append it to the log.
--
-- This is the same protocol as 'postLocal' in "Free.Agent.Bus": ids are the
-- current line count read under the exclusive file lock ('withLogLock' —
-- single-sourced there).
stampOne :: (PostBody a) => FilePath -> Post a -> IO (Stamped a)
stampOne path p = do
  ts <- getCurrentTime
  withLogLock (path <.> "lock") $ do
    n <- BS.count 0x0A <$> BS.readFile path
    let stored = Stamped (ts, fromIntegral n) p
    appendStoredPostsUnlocked path [stored]
    pure stored

-- | Write the per-sender receipt, but never let it kill the daemon.
--
-- The receipt is a courtesy for the client's wait; the log is the truth.
-- GHC locks files in-process per (dev,ino), so a client mid-read can make
-- the write fail transiently with "resource busy"; the write rides the
-- process-wide bus-file mutex (the client reads through it too), retries
-- a few times, then reports on stderr and moves on (wave H).
writeReceipt :: FilePath -> Name -> Text -> IO ()
writeReceipt root sender msg = go (5 :: Int)
  where
    path = receiptPath root sender
    go 0 =
      TIO.hPutStrLn stderr $
        "🔴 daemon: giving up writing receipt "
          <> T.pack path
          <> " (clients may time out; the log is unaffected)"
    go n =
      catch
        (withBusFiles (TIO.writeFile path msg))
        ( \(e :: SomeException) -> do
            TIO.hPutStrLn stderr $
              "🟡 daemon: receipt write failed (retrying): " <> T.pack (displayException e)
            threadDelay 20_000
            go (n - 1)
        )

-- | Submit a bare post to the daemon and wait for the receipt.
--
-- Fails fast and honestly when no live daemon is reading the FIFO: the
-- fifo exists (so the early guard passed) but opening it for append throws
-- ENXIO — "does not exist (Device not configured)" — because GHC opens
-- with @O_NONBLOCK@ and a FIFO with no reader refuses writers (wave H).
postViaDaemon :: FilePath -> Post Text -> IO (Stamped Text)
postViaDaemon root p = do
  let fifo = fifoPath root
  fifoExists <- doesFileExist fifo
  unless fifoExists $
    fail ("🔴 no bus.fifo at " <> fifo <> "; is the daemon running?")
  r <- try @IOException $ do
    withFile fifo AppendMode $ \h -> do
      hSetBuffering h LineBuffering
      TIO.hPutStrLn h (framePost p)
      hFlush h
  case r of
    Right () -> waitReceipt root p
    Left e
      | isDoesNotExistError e ->
          fail
            ( "🔴 no live daemon reading "
                <> fifo
                <> " under "
                <> root
                <> " (the stamping office is not running; start it or check its log)"
            )
      | otherwise -> ioError e

-- | Poll the receipt file until it contains a stamped post matching the
-- submitted post, or a timeout expires.
--
-- The read is strict and bracketed ('readFileStrict'): a lazy read keeps
-- its handle open past the read, and GHC locks files in-process per
-- (dev,ino) — the daemon's next receipt write to the same path would then
-- fail with "resource busy (file is locked)" and die (wave H, the fifo
-- ioException).
waitReceipt :: FilePath -> Post Text -> IO (Stamped Text)
waitReceipt root p = go (200 :: Int)
  where
    go 0 = fail "🔴 daemon receipt timeout"
    go n = do
      let path = receiptPath root (from p)
      exists <- doesFileExist path
      if not exists
        then delay >> go (n - 1)
        else do
          line <- readFileStrict path
          case unframeStored @Text line of
            Just stored
              | matches stored -> pure stored
              | otherwise -> delay >> go (n - 1)
            Nothing -> delay >> go (n - 1)
    delay = threadDelay 50_000
    matches stored =
      let q = stamped stored
       in from q == from p
            && to q == to p
            && thread q == thread p
            && body q == body p

-- | Write a @.ping-NAME@ file for each named recipient of each post.
--
-- 'writePings' lives in "Free.Agent.Bus" (the canonical spelling); this
-- module re-exports it via the import list above.
