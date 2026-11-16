{-# LANGUAGE OverloadedStrings #-}

-- | A small JSONL file log with locked, file-truth appends.
--
-- 'postLocal' assigns the timestamp and file-positional id while holding the
-- cross-process lock, then appends and closes the log. Closing the handle is
-- not an fsync or durability claim. Reads are strict and decoding reports
-- malformed rows instead of dropping them. Selection is pure; waiting lives
-- in the file watcher in "Free.Agent.Bus.File".
module Free.Agent.Bus
  ( -- * Append
    postLocal,
    postLocalWith,

    -- * Decode and selection
    readLogStrict,
    decodeLogStrict,
    readSince,

    -- * Same-process file-handle exclusion
    readFileStrict,
    withBusFiles,
    withLogRead,
    touchLog,
  )
where

import Circuit.Agent (Name)
import Circuit.Parser.Framing (RowError (..), Stored, decodeLog, encodeRow)
import Circuit.Post (Post, PostId, deliversTo)
import Circuit.Shared (Stamped (..), stamp, stamped)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FileLock (SharedExclusive (Exclusive), withTryFileLock)
import System.FilePath ((<.>), (</>))
import System.IO (Handle, IOMode (AppendMode, ReadMode), withFile)
import System.IO.Unsafe (unsafePerformIO)

-- | Append one post to @root/log.jsonl@.
--
-- Timestamp and id are assigned from file truth under the exclusive lock.
-- The id is the current count of complete lines. The append handle is closed
-- before the stamped receipt is returned; no recipient side files are written.
postLocal :: FilePath -> Post Text Text -> IO Stored
postLocal root p = do
  (stored, ()) <- postLocalWith root (\_ _ _ -> ([p], ()))
  case stored of
    [receipt] -> pure receipt
    _ -> ioError (userError "Free.Agent.Bus: single-post append did not produce one receipt")

-- | Decide and append one locked batch from current file truth.
--
-- The decision receives the strictly decoded prior posts, the next
-- file-positional id and one timestamp. It returns the posts to append and a
-- result computed from the same file truth. The posts receive consecutive ids
-- and are appended in one open/close window. There is no public unlocked append.
postLocalWith :: FilePath -> ([Stored] -> PostId -> UTCTime -> ([Post Text Text], b)) -> IO ([Stored], b)
postLocalWith root decide = do
  createDirectoryIfMissing True root
  let path = root </> "log.jsonl"
  withLogLock (path <.> "lock") $ do
    exists <- doesFileExist path
    unless exists $ withFile path AppendMode (\_ -> pure ())
    bytes <- BS.readFile path
    prior <-
      either
        (ioError . userError . formatDecodeErrors path)
        pure
        (decodeLogStrict (TE.decodeUtf8 bytes))
    ts <- getCurrentTime
    let nextId = fromIntegral (BS.count 0x0A bytes)
        (posts, result) = decide prior nextId ts
        stored = zipWith (\pid p -> Stamped (ts, pid) p) [nextId ..] posts
    withFile path AppendMode $ \h ->
      TIO.hPutStr h (T.concat (map ((<> "\n") . encodeRow) stored))
    pure (stored, result)

-- | Acquire the log's exclusive lock without blocking the runtime in flock.
--
-- Same-process handle exclusion is taken first, so lock order is always
-- mutex-then-flock. A busy lock is retried with a short sleep; failure after
-- about ten seconds is explicit rather than an unbounded wait.
withLogLock :: FilePath -> IO a -> IO a
withLogLock lockPath act = withBusFiles (go (20_000 :: Int))
  where
    go 0 =
      ioError
        ( userError
            ("Free.Agent.Bus: could not acquire " <> lockPath <> " within 10s; lock holder stuck?")
        )
    go n = do
      m <- withTryFileLock lockPath Exclusive (\_lock -> act)
      case m of
        Just r -> pure r
        Nothing -> threadDelay 500 >> go (n - 1)

-- | Process-wide mutex over bus-path handle lifetimes.
--
-- GHC's RTS locks files in-process per (dev,ino), and conflicting read/append
-- opens fail with @resource busy (file is locked)@. The cross-process flock
-- does not own that hazard; this mutex does.
logWriteMutex :: MVar ()
logWriteMutex = unsafePerformIO (newMVar ())
{-# NOINLINE logWriteMutex #-}

-- | Serialize a bus-path open/act/close window against other bus-path file
-- operations in this process. Callers must not take this mutex recursively.
withBusFiles :: IO a -> IO a
withBusFiles act = withMVar logWriteMutex (\_ -> act)

-- | Open a bus path for reading under same-process handle exclusion.
withLogRead :: FilePath -> (Handle -> IO a) -> IO a
withLogRead path act = withBusFiles $ withFile path ReadMode act

-- | Create a log if absent, closing the append handle before returning.
touchLog :: FilePath -> IO ()
touchLog path = withBusFiles $ withFile path AppendMode (\_ -> pure ())

-- | Strict, bracketed whole-file read under same-process handle exclusion.
readFileStrict :: FilePath -> IO Text
readFileStrict path = withBusFiles (TE.decodeUtf8 <$> BS.readFile path)

-- | Strictly read and decode a stored log. Any malformed nonblank row fails
-- the whole read and names its row, character offset and parser reason.
readLogStrict :: FilePath -> IO [Stored]
readLogStrict path = do
  content <- readFileStrict path
  either (ioError . userError . formatDecodeErrors path) pure (decodeLogStrict content)

-- | Decode a complete JSONL log through the canonical row codec.
decodeLogStrict :: Text -> Either RowError [Stored]
decodeLogStrict = decodeLog

formatDecodeErrors :: FilePath -> RowError -> String
formatDecodeErrors path (RowError row offset reason) =
  "Free.Agent.Bus: "
    <> path
    <> ": malformed JSONL row; decode failed at row "
    <> show row
    <> ", character offset "
    <> show offset
    <> ": "
    <> T.unpack reason

-- | Select addressed posts with ids at or after the cursor. The cursor is the
-- next unprocessed id. This is pure; file waiting and incremental reads belong
-- to the watcher.
readSince :: [Stored] -> [Name] -> PostId -> [Stored]
readSince posts names since =
  [ stored
  | stored <- posts,
    snd (stamp stored) >= since,
    deliversTo (stamped stored) names
  ]
