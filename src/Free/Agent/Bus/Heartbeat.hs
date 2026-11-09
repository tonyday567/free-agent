{-# LANGUAGE OverloadedStrings #-}

-- | Seat heartbeats: file-truth liveness for the bus.
--
-- A seat writes @.hb-NAME@ in the bus root on every transition — wake,
-- working, idle — and a final @dead@ when it exits. The file is one line:
--
-- > <state> <utc-timestamp> <pid>
--
-- Legacy files without a pid still parse, but a stale heartbeat without
-- verifiable process evidence reads non-fab. A current file lets the status
-- reader probe the pid without polling or invoking the seat, separating a
-- crash from a live but stale seat. A torn read parses to 'Nothing'.
--
-- The seat manager writes the same file with @dead@ when it retires a seat
-- it launched; the seat writes @dead@ itself on a clean exit.
module Free.Agent.Bus.Heartbeat
  ( -- * States
    HbState (..),
    Heartbeat,
    hbGlyph,
    hbText,

    -- * File
    hbPath,
    writeHeartbeat,
    readHeartbeat,
    parseHb,
  )
where

import Circuit.Agent (Name)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid)
import Text.Read (readMaybe)

-- | The three live states plus the retire marker.
--
-- 'Wake' is the seat starting up, 'Working' is a delivered post being
-- handled, 'Idle' is the seat blocked on its ping latch — ready and
-- waiting for input. 'Dead' is written by the seat on clean exit and by
-- the seat manager when it retires a seat.
data HbState
  = Wake
  | Working
  | Idle
  | Dead
  deriving (Eq, Ord, Show, Enum, Bounded)

type Heartbeat = (HbState, UTCTime, Maybe CPid)

-- | The glyph a state renders as in a dot column: ready and starting are
-- both 🟢 (there is a process and it is not busy), working is 🟣 busy,
-- retired is ⚫. Age enters the glyph only in the status circle classifier:
-- a parked idle renders 🟡 there once it passes the hour.
hbGlyph :: HbState -> Text
hbGlyph = \case
  Wake -> "\x1F7E2" -- 🟢
  Idle -> "\x1F7E2" -- 🟢
  Working -> "\x1F7E3" -- 🟣
  Dead -> "\x26AB" -- ⚫

-- | The wire token for a state, as written in the heartbeat file.
hbText :: HbState -> Text
hbText = \case
  Wake -> "wake"
  Working -> "working"
  Idle -> "idle"
  Dead -> "dead"

-- | Path to a seat's heartbeat file.
hbPath :: FilePath -> Name -> FilePath
hbPath root name = root </> (".hb-" <> T.unpack name)

-- | Write the heartbeat for a seat. Truncating write: the reader tolerates
-- a torn line as 'Nothing'.
writeHeartbeat :: FilePath -> Name -> HbState -> IO ()
writeHeartbeat root name st = do
  ts <- getCurrentTime
  pid <- getProcessID
  TIO.writeFile (hbPath root name) (hbText st <> " " <> T.pack (iso8601Show ts) <> " " <> T.pack (show pid) <> "\n")

-- | Read a seat's heartbeat, if the file exists and parses.
readHeartbeat :: FilePath -> Name -> IO (Maybe Heartbeat)
readHeartbeat root name = do
  let path = hbPath root name
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else parseHb <$> TIO.readFile path

-- | Parse current @state timestamp pid@ and legacy @state timestamp@ files.
parseHb :: Text -> Maybe Heartbeat
parseHb t = case T.words (T.strip t) of
  [s, ts] -> (,,) <$> stateOf s <*> iso8601ParseM (T.unpack ts) <*> pure Nothing
  [s, ts, pid] -> (,,) <$> stateOf s <*> iso8601ParseM (T.unpack ts) <*> (Just <$> readMaybe (T.unpack pid))
  _ -> Nothing
  where
    stateOf s = lookup s [(hbText st, st) | st <- [minBound .. maxBound]]
