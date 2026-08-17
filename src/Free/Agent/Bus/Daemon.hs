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
import Control.Monad (forever, unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Free.Agent.Bus (appendStoredPostsUnlocked)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
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
import System.Posix.Files (createNamedPipe)
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
-- Creates the bus directory, touches @log.jsonl@, creates the FIFO if absent,
-- then reads one bare 'Post' line at a time. Malformed lines are reported on
-- stderr and skipped; valid lines are stamped and appended.
runDaemon :: forall a. (PostBody a) => FilePath -> IO ()
runDaemon root = do
  createDirectoryIfMissing True root
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ withFile path AppendMode (\_ -> pure ())
  let fifo = fifoPath root
  fifoExists <- doesFileExist fifo
  unless fifoExists $ createNamedPipe fifo 0o600
  withFile fifo ReadWriteMode $ \h -> do
    hSetBuffering h LineBuffering
    forever $ do
      line <- T.pack <$> hGetLine h
      case parsePost @a line of
        Nothing -> TIO.hPutStrLn stderr ("🔴 daemon: invalid post JSON: " <> line)
        Just p -> do
          stored <- stampOne path p
          TIO.writeFile (receiptPath root (from p)) (frameStored stored)
          writePings path [stored]

-- | Stamp a single post and append it to the log.
--
-- This is the same protocol as 'postLocal' in "Free.Agent.Bus": ids are the
-- current line count read under the exclusive file lock.
stampOne :: (PostBody a) => FilePath -> Post a -> IO (Stamped a)
stampOne path p = do
  ts <- getCurrentTime
  withFileLock (path <.> "lock") Exclusive $ \_lock -> do
    n <- BS.count 0x0A <$> BS.readFile path
    let stored = Stamped (ts, fromIntegral n) p
    appendStoredPostsUnlocked path [stored]
    pure stored

-- | Submit a bare post to the daemon and wait for the receipt.
postViaDaemon :: FilePath -> Post Text -> IO (Stamped Text)
postViaDaemon root p = do
  let fifo = fifoPath root
  fifoExists <- doesFileExist fifo
  unless fifoExists $
    fail ("🔴 no bus.fifo at " <> fifo <> "; is the daemon running?")
  withFile fifo AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    TIO.hPutStrLn h (framePost p)
    hFlush h
  waitReceipt root p

-- | Poll the receipt file until it contains a stamped post matching the
-- submitted post, or a timeout expires.
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
          line <- TIO.readFile path
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
-- Copied from "Free.Agent.Bus" because the original helper is internal to
-- that module.
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
