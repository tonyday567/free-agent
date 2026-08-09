{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | free-agent bus CLI.
--
-- Subcommands:
--
--   post [ROOT] [--from NAME] [--to NAME]... [--body TEXT]
--   watch [ROOT] NAME [NAME...]
--   read [ROOT] NAME [NAME...] [--since ID | --cursor NAME]
--   cursor [ROOT] get NAME
--   cursor [ROOT] set NAME ID
--   ping-watch [ROOT] NAME
--   status [ROOT] [--threshold SECS]
module Free.Agent.Bus.Cli
  ( BusCommand (..),
    busParser,
    runBusCommand,
    runStatus,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost)
import Circuit.Agent.Framing (Stamped (..), frameStored, parseLine, parsePost, stamp, timeStamp, stamped)
import Circuit.Agent.Mark (isHalt, markOf)
import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forever, guard, unless, when)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, sort, sortOn)
import Data.Maybe (listToMaybe, mapMaybe, maybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (LocalTime, NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, localTimeToUTC, utc)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File (readCursor, writeCursor)
import Options.Applicative
import System.Directory (doesFileExist, listDirectory)
import System.Exit (exitFailure)
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
    stdout,
    withFile,
  )
import Text.Read (readMaybe)

data Since = SinceId PostId | SinceCursor Text
  deriving (Show)

data BusCommand
  = PostCommand FilePath (Maybe Text) [Text] (Maybe Text)
  | WatchCommand FilePath [Text]
  | ReadCommand FilePath [Text] (Maybe Since)
  | CursorGetCommand FilePath Text
  | CursorSetCommand FilePath Text PostId
  | PingWatchCommand FilePath Text
  | StatusCommand FilePath NominalDiffTime
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

rootOpt :: Parser FilePath
rootOpt =
  option
    str
    ( long "root"
        <> short 'r'
        <> metavar "ROOT"
        <> value "."
        <> showDefault
        <> help "Bus root directory"
    )

nameArg :: Parser Text
nameArg = argument (T.pack <$> str) (metavar "NAME" <> help "Agent name")

namesArg :: Parser [Text]
namesArg =
  some
    ( argument
        (T.pack <$> str)
        (metavar "NAME..." <> help "Subscriber name(s)")
    )

postCmd :: Parser BusCommand
postCmd = do
  root <- rootOpt
  fromName <-
    optional
      ( option
          (T.pack <$> str)
          (long "from" <> metavar "NAME" <> help "Sender name")
      )
  toNames <-
    many
      ( option
          (T.pack <$> str)
          (long "to" <> metavar "NAME" <> help "Recipient name (repeatable)")
      )
  bodyText <-
    optional
      ( option
          (T.pack <$> str)
          (long "body" <> metavar "TEXT" <> help "Post body")
      )
  pure (PostCommand root fromName toNames bodyText)

watchCmd :: Parser BusCommand
watchCmd = WatchCommand <$> rootOpt <*> namesArg

readCmd :: Parser BusCommand
readCmd = do
  root <- rootOpt
  names <- namesArg
  since <- optional sinceParser
  pure (ReadCommand root names since)

sinceParser :: Parser Since
sinceParser =
  (SinceId <$> option auto (long "since" <> metavar "ID" <> help "Only posts with id >= ID"))
    <|> (SinceCursor . T.pack <$> strOption (long "cursor" <> metavar "NAME" <> help "Only posts at or after .cursor-NAME"))

cursorGetCmd :: Parser BusCommand
cursorGetCmd = CursorGetCommand <$> rootOpt <*> nameArg

cursorSetCmd :: Parser BusCommand
cursorSetCmd = CursorSetCommand <$> rootOpt <*> nameArg <*> idArg
  where
    idArg = argument auto (metavar "ID" <> help "Cursor value (next post id)")

cursorCmd :: Parser BusCommand
cursorCmd =
  subparser
    ( command "get" (info (cursorGetCmd <**> helper) (progDesc "Print current cursor"))
        <> command "set" (info (cursorSetCmd <**> helper) (progDesc "Set cursor"))
    )

pingWatchCmd :: Parser BusCommand
pingWatchCmd = PingWatchCommand <$> rootOpt <*> nameArg

thresholdOpt :: Parser NominalDiffTime
thresholdOpt =
  option
    (eitherReader readSeconds)
    ( long "threshold"
        <> short 't'
        <> metavar "SECS"
        <> value 900
        <> showDefault
        <> help "Seconds since last post for the bus to be considered live"
    )
  where
    readSeconds s =
      case reads s of
        [(n, "")] -> Right (fromInteger n :: NominalDiffTime)
        _ -> Left ("expected a whole number of seconds, got: " ++ s)

statusCmd :: Parser BusCommand
statusCmd = StatusCommand <$> rootOpt <*> thresholdOpt

busParser :: Parser BusCommand
busParser =
  subparser
    ( command "post" (info (postCmd <**> helper) (progDesc "Post a message to the bus"))
        <> command "watch" (info (watchCmd <**> helper) (progDesc "Watch the log for posts addressed to NAMEs"))
        <> command "read" (info (readCmd <**> helper) (progDesc "Read posts addressed to NAMEs"))
        <> command "cursor" (info (cursorCmd <**> helper) (progDesc "Read or write agent cursor"))
        <> command "ping-watch" (info (pingWatchCmd <**> helper) (progDesc "Watch the ping file for NAME and exit on change"))
        <> command "status" (info (statusCmd <**> helper) (progDesc "Bus health: post count, last post, seats"))
    )

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

runBusCommand :: BusCommand -> IO ()
runBusCommand (PostCommand root mfrom mto mbody) = runPost root mfrom mto mbody
runBusCommand (WatchCommand root names) = runWatch root names
runBusCommand (ReadCommand root names since) = runRead root names since
runBusCommand (CursorGetCommand root name) = runCursorGet root name
runBusCommand (CursorSetCommand root name pid) = runCursorSet root name pid
runBusCommand (PingWatchCommand root name) = runPingWatch root name
runBusCommand (StatusCommand root threshold) = runStatus root threshold

-- ---------------------------------------------------------------------------
-- post
-- ---------------------------------------------------------------------------

runPost :: FilePath -> Maybe Text -> [Text] -> Maybe Text -> IO ()
runPost root mfrom mto mbody = do
  p <- case (mfrom, mbody) of
    (Just fromName, Just bodyText) -> pure (mkPost fromName mto bodyText)
    (Nothing, Nothing) -> do
      line <- TIO.getLine
      case parsePost line of
        Nothing -> do
          TIO.putStrLn "🔴 invalid post JSON"
          exitFailure
        Just p -> pure p
    _ -> do
      TIO.putStrLn "🔴 post requires either --from and --body flags or JSON on stdin"
      exitFailure
  stored <- postLocal root p
  TIO.putStrLn (frameStored stored)

-- ---------------------------------------------------------------------------
-- watch
-- ---------------------------------------------------------------------------

runWatch :: FilePath -> [Text] -> IO ()
runWatch root names = do
  when (null names) $ do
    TIO.putStrLn "🔴 watch requires at least one name"
    exitFailure
  let path = root </> "log.jsonl"
      logName = takeFileName path
      dir = takeDirectory path
  exists <- doesFileExist path
  unless exists $ withFile path AppendMode (\_ -> pure ())
  h <- openFile path ReadMode
  hSetBuffering h LineBuffering
  hSeek h AbsoluteSeek 0
  -- Skip existing content; agents start watching from now.
  size <- hFileSize h
  hSeek h AbsoluteSeek size
  withManager $ \mgr -> do
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      drain h names
    -- Block forever; the watch listener runs in the background.
    forever (threadDelay 1000000)
  where
    drain :: Handle -> [Text] -> IO ()
    drain h names' = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ TIO.putStrLn (filterStored names' line)
        drain h names'

filterStored :: [Name] -> Text -> Maybe Text
filterStored names line = do
  stored <- parseLine line
  let p = stamped stored
  if deliversTo p names then Just line else Nothing

-- ---------------------------------------------------------------------------
-- read
-- ---------------------------------------------------------------------------

runRead :: FilePath -> [Text] -> Maybe Since -> IO ()
runRead root names since = do
  sinceId <- case since of
    Nothing -> pure 0
    Just (SinceId pid) -> pure pid
    Just (SinceCursor name) -> readCursor root name
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ pure ()
  when exists $ do
    content <- TIO.readFile path
    let lines' = T.lines content
    traverse_ TIO.putStrLn (mapMaybe (filterStoredSince names sinceId) lines')

filterStoredSince :: [Name] -> PostId -> Text -> Maybe Text
filterStoredSince names sinceId line = do
  stored <- parseLine line
  let p = stamped stored
  guard (stamp stored >= sinceId)
  guard (deliversTo p names)
  pure line

-- ---------------------------------------------------------------------------
-- cursor
-- ---------------------------------------------------------------------------

runCursorGet :: FilePath -> Text -> IO ()
runCursorGet root name = do
  pid <- readCursor root name
  TIO.putStrLn (T.pack (show pid))

runCursorSet :: FilePath -> Text -> PostId -> IO ()
runCursorSet root name pid = writeCursor root name pid

-- ---------------------------------------------------------------------------
-- ping-watch
-- ---------------------------------------------------------------------------

runPingWatch :: FilePath -> Text -> IO ()
runPingWatch root name = do
  let pingPath = root </> (".ping-" <> T.unpack name)
      pingName = takeFileName pingPath
  exists <- doesFileExist pingPath
  unless exists $ withFile pingPath AppendMode (\_ -> pure ())
  withManager $ \mgr -> do
    done <- newEmptyMVar
    _ <- watchDir mgr root (\ev -> takeFileName (eventPath ev) == pingName) $ \_ev -> do
      txt <- TIO.readFile pingPath
      TIO.putStrLn txt
      putMVar done ()
    takeMVar done

-- ---------------------------------------------------------------------------
-- status
-- ---------------------------------------------------------------------------

-- | Seat status classification for the cursor report.
data SeatStatus = CaughtUp | Done | Behind Integer

-- | Print a bus status report.
--
-- A seat whose latest own post is a halt mark (🟢/🔵) is reported as @done@
-- rather than @behind@: decided quiet is a read, not an inference.
runStatus :: FilePath -> NominalDiffTime -> IO ()
runStatus root threshold = do
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ do
    TIO.putStrLn ("🔴 " <> T.pack root <> " — no log.jsonl; not a bus")
    exitFailure
  content <- TIO.readFile path
  let ls = filter (not . T.null) (T.lines content)
      posts = mapMaybe parseLine ls
      idless = length ls - length posts
      maxId = maximum (0 : map stamp posts)
  now <- getCurrentTime
  case posts of
    [] ->
      TIO.putStrLn ("🟡 " <> T.pack root <> " — empty bus (0 posts)")
    _ -> do
      let lastPost = last posts
          age = diffUTCTime now (timeStamp lastPost)
          live = age < threshold
      TIO.putStrLn
        ( (if live then "🟢 " else "🟡 ")
            <> T.pack root
            <> (if live then " — live; " else " — quiet; ")
            <> T.pack (show (length posts))
            <> " posts"
            <> (if idless > 0 then " (" <> T.pack (show idless) <> " id-less)" else "")
            <> "; last id="
            <> T.pack (show (stamp lastPost))
            <> " from="
            <> from (stamped lastPost)
            <> " at "
            <> T.pack (show (timeStamp lastPost))
            <> " (" <> fmtAge age <> " ago)"
        )
  entries <- listDirectory root
  let names = sort [n | e <- entries, Just n <- [T.stripPrefix ".cursor-" (T.pack e)]]
  cursors <- traverse (\n -> (n,) <$> readCursor root n) names
  let statuses = map (seatStatus posts maxId) cursors
      behinds = [(n, c, u) | (n, c, Behind u) <- statuses]
      dones = [(n, c) | (n, c, Done) <- statuses]
      caughtUps = [(n, c) | (n, c, CaughtUp) <- statuses]
  if null names
    then TIO.putStrLn "seats: no cursors — nothing has ever read this bus"
    else do
      TIO.putStrLn
        ( "seats: "
            <> T.pack (show (length names))
            <> " cursors, "
            <> T.pack (show (length behinds))
            <> " behind, "
            <> T.pack (show (length dones))
            <> " done, "
            <> T.pack (show (length caughtUps))
            <> " caught up"
        )
      traverse_
        (\(n, c, u) -> TIO.putStrLn ("  " <> n <> " @" <> T.pack (show c) <> " (" <> T.pack (show u) <> " unread)"))
        behinds
      traverse_ (\(n, c) -> TIO.putStrLn ("  " <> n <> " @" <> T.pack (show c) <> " done")) dones
      traverse_ (\(n, c) -> TIO.putStrLn ("  " <> n <> " @" <> T.pack (show c) <> " caught up")) caughtUps
  where
    seatStatus :: [Stamped Text] -> PostId -> (Name, PostId) -> (Name, PostId, SeatStatus)
    seatStatus posts maxId (n, c)
      | isDone = (n, c, Done)
      | c > maxId = (n, c, CaughtUp)
      | otherwise = (n, c, Behind (fromIntegral maxId - fromIntegral c + 1 :: Integer))
      where
        isDone =
          case latestBy n posts of
            Nothing -> False
            Just stored -> maybe False isHalt (markOf (stamped stored))

    latestBy :: Name -> [Stamped Text] -> Maybe (Stamped Text)
    latestBy n = listToMaybe . sortOn (Down . stamp) . filter ((== n) . from . stamped)

-- | Scribe stamps naive UTC; raw appends may carry an offset. Take both.
parseTs :: Text -> Maybe UTCTime
parseTs t =
  iso8601ParseM s
    <|> (localTimeToUTC utc <$> (iso8601ParseM s :: Maybe LocalTime))
  where
    s = T.unpack t

fmtAge :: NominalDiffTime -> Text
fmtAge s
  | s < 120 = T.pack (show (round s :: Int)) <> "s"
  | s < 7200 = T.pack (show (round (s / 60) :: Int)) <> "m"
  | s < 172800 = T.pack (show (round (s / 3600) :: Int)) <> "h"
  | otherwise = T.pack (show (round (s / 86400) :: Int)) <> "d"
