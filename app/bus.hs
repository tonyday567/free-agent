{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | free-agent-bus CLI.
--
-- Subcommands:
--
--   post [ROOT] [--from NAME] [--to NAME]... [--body TEXT]
--   watch [ROOT] NAME [NAME...]
--   read [ROOT] NAME [NAME...] [--since ID | --cursor NAME]
--   cursor [ROOT] get NAME
--   cursor [ROOT] set NAME ID
--   ping-watch [ROOT] NAME
module Main (main) where

import Circuit.Agent (Name, Post (..), PostId, mkPost)
import Circuit.Agent.Framing (StoredPost, frameStored, parseLine, parsePost, stampId, stamped)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forever, guard, unless, when)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (busDeliversTo, closeBus, openBus, scribeIO)
import Options.Applicative
import System.Directory (doesFileExist)
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

data Command
  = PostCommand FilePath (Maybe Text) [Text] (Maybe Text)
  | WatchCommand FilePath [Text]
  | ReadCommand FilePath [Text] (Maybe Since)
  | CursorGetCommand FilePath Text
  | CursorSetCommand FilePath Text PostId
  | PingWatchCommand FilePath Text

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

postCmd :: Parser Command
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

watchCmd :: Parser Command
watchCmd = WatchCommand <$> rootOpt <*> namesArg

readCmd :: Parser Command
readCmd = do
  root <- rootOpt
  names <- namesArg
  since <- optional sinceParser
  pure (ReadCommand root names since)

sinceParser :: Parser Since
sinceParser =
  (SinceId <$> option auto (long "since" <> metavar "ID" <> help "Only posts with id >= ID"))
    <|> (SinceCursor . T.pack <$> strOption (long "cursor" <> metavar "NAME" <> help "Only posts at or after .cursor-NAME"))

cursorGetCmd :: Parser Command
cursorGetCmd = CursorGetCommand <$> rootOpt <*> nameArg

cursorSetCmd :: Parser Command
cursorSetCmd = CursorSetCommand <$> rootOpt <*> nameArg <*> idArg
  where
    idArg = argument auto (metavar "ID" <> help "Cursor value (next post id)")

cursorCmd :: Parser Command
cursorCmd =
  subparser
    ( command "get" (info (cursorGetCmd <**> helper) (progDesc "Print current cursor"))
        <> command "set" (info (cursorSetCmd <**> helper) (progDesc "Set cursor"))
    )

pingWatchCmd :: Parser Command
pingWatchCmd = PingWatchCommand <$> rootOpt <*> nameArg

commandParser :: Parser Command
commandParser =
  subparser
    ( command "post" (info (postCmd <**> helper) (progDesc "Post a message to the bus"))
        <> command "watch" (info (watchCmd <**> helper) (progDesc "Watch the log for posts addressed to NAMEs"))
        <> command "read" (info (readCmd <**> helper) (progDesc "Read posts addressed to NAMEs"))
        <> command "cursor" (info (cursorCmd <**> helper) (progDesc "Read or write agent cursor"))
        <> command "ping-watch" (info (pingWatchCmd <**> helper) (progDesc "Watch the ping file for NAME and exit on change"))
    )

opts :: ParserInfo Command
opts = info (commandParser <**> helper) (fullDesc <> progDesc "free-agent bus CLI" <> header "free-agent-bus - append-only JSONL message bus")

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cmd <- execParser opts
  runCommand cmd

runCommand :: Command -> IO ()
runCommand (PostCommand root mfrom mto mbody) = runPost root mfrom mto mbody
runCommand (WatchCommand root names) = runWatch root names
runCommand (ReadCommand root names since) = runRead root names since
runCommand (CursorGetCommand root name) = runCursorGet root name
runCommand (CursorSetCommand root name pid) = runCursorSet root name pid
runCommand (PingWatchCommand root name) = runPingWatch root name

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
  bus <- openBus root
  stored <- scribeIO bus p
  TIO.putStrLn (frameStored stored)
  closeBus bus

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
  if busDeliversTo p names then Just line else Nothing

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
  guard (stampId stored >= sinceId)
  guard (busDeliversTo p names)
  pure line

-- ---------------------------------------------------------------------------
-- cursor
-- ---------------------------------------------------------------------------

cursorFilePath :: FilePath -> Text -> FilePath
cursorFilePath root name = root </> (".cursor-" <> T.unpack name)

readCursor :: FilePath -> Text -> IO PostId
readCursor root name = do
  let path = cursorFilePath root name
  exists <- doesFileExist path
  if not exists
    then pure 0
    else do
      txt <- TIO.readFile path
      pure $ maybe 0 fromIntegral (readMaybe @Integer (T.unpack (T.strip txt)))

runCursorGet :: FilePath -> Text -> IO ()
runCursorGet root name = do
  pid <- readCursor root name
  TIO.putStrLn (T.pack (show pid))

runCursorSet :: FilePath -> Text -> PostId -> IO ()
runCursorSet root name pid = do
  let path = cursorFilePath root name
  TIO.writeFile path (T.pack (show pid))

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
