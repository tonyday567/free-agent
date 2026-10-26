{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | free-agent bus CLI.
--
-- Subcommands:
--
--   say [ROOT] BODY... [--to NAME]... [--from NAME] [--thread ID]
--   post [ROOT] [--from NAME] [--to NAME]... [--body TEXT]
--   watch [ROOT] NAME [NAME...]
--   read [ROOT] [NAME...] [--since ID | --cursor NAME | --thread ID] [--depth 0|1|2]
--   cursor [ROOT] get NAME
--   cursor [ROOT] set NAME ID
--   ping-watch [ROOT] NAME
--   status [ROOT] [--threshold SECS] [--live] [--depth 0|1|2]
--
-- @say@ is the act path for the deck: one short posting command, sender
-- defaulting to @tony@, body as positional words or on stdin. @post@ stays
-- the explicit form (JSON on stdin, or flags).
--
-- @--depth@ selects the cascade: 0 is a dot, 1 is one line, 2 is the
-- verbatim evidence. The depth is always an explicit flag; the api never
-- guesses the consumer. @status --live@ derives its dots from the seat
-- heartbeats (@.hb-NAME@, see "Free.Agent.Bus.Heartbeat"), so a stale
-- heartbeat reads as crashed instead of as live.
module Free.Agent.Bus.Cli
  ( BusCommand (..),
    Depth (..),
    busParser,
    depthOpt,
    liveOpt,
    runBusCommand,
    runStatus,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost, sortNub)
import Circuit.Agent.Framing (Stamped, frameStored, parsePost, stamp, stamped, unframeStored)
import Circuit.Agent.Mark (isHalt, markOf)
import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forM_, forever, guard, unless, when)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, sort, sortOn)
import Data.Maybe (listToMaybe, mapMaybe, maybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (LocalTime, NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, localTimeToUTC, utc)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Free.Agent.Bus (postLocal, readFileStrict)
import Free.Agent.Bus.Daemon (fifoPath, postViaDaemon, runDaemon)
import Free.Agent.Bus.File (readCursor, writeCursor)
import Free.Agent.Bus.Heartbeat (HbState (..), hbGlyph, hbText, readHeartbeat)
import Free.Agent.BusStats (SliceMode (..), slicePosts)
import Free.Agent.Cli.Config (rootOpt)
import Options.Applicative
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FSNotify (Event (..), watchDir, withManager)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (AppendMode, ReadMode),
    SeekMode (AbsoluteSeek),
    hFileSize,
    hIsEOF,
    hIsTerminalDevice,
    hSeek,
    hSetBuffering,
    openFile,
    stderr,
    stdin,
    stdout,
    withFile,
  )
import Text.Read (readMaybe)

-- | Cascade depth: 0 is a dot, 1 is a line, 2 is the verbatim evidence.
newtype Depth = Depth Int
  deriving (Eq, Show)

data Since = SinceId PostId | SinceCursor Text
  deriving (Show)

data BusCommand
  = SayCommand FilePath Text [Text] (Maybe PostId) [Text]
  | PostCommand FilePath (Maybe Text) [Text] (Maybe Text) Bool
  | WatchCommand FilePath [Text]
  | ReadCommand FilePath [Text] (Maybe Since) (Maybe PostId) Depth
  | CursorGetCommand FilePath Text
  | CursorSetCommand FilePath Text PostId
  | PingWatchCommand FilePath Text
  | StatusCommand FilePath NominalDiffTime Bool Depth
  | DaemonCommand FilePath
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

nameArg :: Parser Text
nameArg = argument (T.pack <$> str) (metavar "NAME" <> help "Agent name")

namesArg :: Parser [Text]
namesArg =
  some
    ( argument
        (T.pack <$> str)
        (metavar "NAME..." <> help "Subscriber name(s)")
    )

depthOpt :: Depth -> Parser Depth
depthOpt dflt =
  option
    (eitherReader readDepth)
    ( long "depth"
        <> metavar "0|1|2"
        <> value dflt
        <> showDefault
        <> help "Cascade depth: 0 dot, 1 line, 2 full"
    )
  where
    readDepth s = case reads s of
      [(n, "")] | n >= (0 :: Int) && n <= 2 -> Right (Depth n)
      _ -> Left ("expected 0, 1 or 2, got: " ++ s)

threadOpt :: Parser (Maybe PostId)
threadOpt =
  optional
    ( option
        auto
        ( long "thread"
            <> metavar "ID"
            <> help "Thread view: the root (or any member) post id"
        )
    )

sayCmd :: Parser BusCommand
sayCmd = do
  root <- rootOpt
  fromName <-
    option
      (T.pack <$> str)
      ( long "from"
          <> metavar "NAME"
          <> value "tony"
          <> showDefault
          <> help "Sender name"
      )
  toNames <-
    many
      ( option
          (T.pack <$> str)
          (long "to" <> metavar "NAME" <> help "Recipient name (repeatable)")
      )
  thr <- threadOpt
  body <- many (argument (T.pack <$> str) (metavar "BODY..." <> help "Post body (or pipe it on stdin)"))
  pure (SayCommand root fromName toNames thr body)

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
  viaDaemon <-
    switch
      ( long "via-daemon"
          <> help "Submit to the stamping daemon instead of self-stamping"
      )
  pure (PostCommand root fromName toNames bodyText viaDaemon)

watchCmd :: Parser BusCommand
watchCmd = WatchCommand <$> rootOpt <*> namesArg

readCmd :: Parser BusCommand
readCmd = do
  root <- rootOpt
  names <- many (argument (T.pack <$> str) (metavar "NAME..." <> help "Subscriber name(s)"))
  since <- optional sinceParser
  thr <- threadOpt
  depth <- depthOpt (Depth 2)
  pure (ReadCommand root names since thr depth)

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
        <> help "Seconds of silence before the bus (and a seat heartbeat) reads as stale"
    )
  where
    readSeconds s =
      case reads s of
        [(n, "")] -> Right (fromInteger n :: NominalDiffTime)
        _ -> Left ("expected a whole number of seconds, got: " ++ s)

statusCmd :: Parser BusCommand
statusCmd = do
  root <- rootOpt
  threshold <- thresholdOpt
  live <- liveOpt
  depth <- depthOpt (Depth 1)
  pure (StatusCommand root threshold live depth)

-- | The @--live@ view selector: seat dots-and-lines from the heartbeats.
liveOpt :: Parser Bool
liveOpt = switch (long "live" <> help "Seat dots-and-lines: heartbeat state, cursor, last post id")

daemonCmd :: Parser BusCommand
daemonCmd = DaemonCommand <$> rootOpt

busParser :: Parser BusCommand
busParser =
  subparser
    ( command "say" (info (sayCmd <**> helper) (progDesc "Say something on the bus (deck defaults: --from tony)"))
        <> command "post" (info (postCmd <**> helper) (progDesc "Post a message to the bus (flags or JSON on stdin)"))
        <> command "watch" (info (watchCmd <**> helper) (progDesc "Watch the log for posts addressed to NAMEs"))
        <> command "read" (info (readCmd <**> helper) (progDesc "Read posts addressed to NAMEs, or a thread by id"))
        <> command "cursor" (info (cursorCmd <**> helper) (progDesc "Read or write agent cursor"))
        <> command "ping-watch" (info (pingWatchCmd <**> helper) (progDesc "Watch the ping file for NAME and exit on change"))
        <> command "status" (info (statusCmd <**> helper) (progDesc "Bus health: post count, last post, seats, seat liveness"))
        <> command "daemon" (info (daemonCmd <**> helper) (progDesc "Run the single-writer stamping daemon"))
    )

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

runBusCommand :: BusCommand -> IO ()
runBusCommand (SayCommand root fromName toNames thr body) = runSay root fromName toNames thr body
runBusCommand (PostCommand root mfrom mto mbody viaDaemon) = runPost root mfrom mto mbody viaDaemon
runBusCommand (WatchCommand root names) = runWatch root names
runBusCommand (ReadCommand root names since thr depth) = runRead root names since thr depth
runBusCommand (CursorGetCommand root name) = runCursorGet root name
runBusCommand (CursorSetCommand root name pid) = runCursorSet root name pid
runBusCommand (PingWatchCommand root name) = runPingWatch root name
runBusCommand (StatusCommand root threshold live depth) = runStatus root threshold live depth
runBusCommand (DaemonCommand root) = runDaemon @Text root

-- ---------------------------------------------------------------------------
-- act: say / post
-- ---------------------------------------------------------------------------

-- | Resolve the bus root: @FREE_AGENT_BUS_ROOT@ wins when @--root@ is the
-- default @.@, so a shell in ~/coffee never grows a stray log.
resolveRoot :: FilePath -> IO FilePath
resolveRoot root = do
  envRoot <- lookupEnv "FREE_AGENT_BUS_ROOT"
  pure $ case envRoot of
    Just r | root == "." -> r
    _ -> root

-- | Post to a root, refusing loudly when there is no bus there.
postChecked :: FilePath -> Post Text -> IO (Stamped Text)
postChecked root p = do
  let logPath = root </> "log.jsonl"
  exists <- doesFileExist logPath
  unless exists $ do
    TIO.putStrLn
      ( "🔴 "
          <> T.pack root
          <> " — no log.jsonl; not a bus. Set FREE_AGENT_BUS_ROOT or use --root."
      )
    exitFailure
  postLocal root p

-- | One command on the act path: body as words or on stdin, sender and
-- recipients defaulted, thread optional.
runSay :: FilePath -> Text -> [Text] -> Maybe PostId -> [Text] -> IO ()
runSay root fromName toNames thr bodyWords = do
  root' <- resolveRoot root
  bodyText <- case bodyWords of
    (w : _) -> pure (T.unwords (w : drop 1 bodyWords))
    [] -> do
      tty <- hIsTerminalDevice stdin
      if tty
        then do
          TIO.hPutStrLn stderr "🔴 say: nothing to post — pass BODY as arguments or pipe it on stdin"
          exitFailure
        else T.strip <$> TIO.getContents
  when (T.null bodyText) $ do
    TIO.hPutStrLn stderr "🔴 say: empty body"
    exitFailure
  stored <- postChecked root' (mkPost fromName toNames bodyText) {thread = maybe [] pure thr}
  TIO.putStrLn (frameStored stored)

runPost :: FilePath -> Maybe Text -> [Text] -> Maybe Text -> Bool -> IO ()
runPost root mfrom mto mbody viaDaemon = do
  root' <- resolveRoot root
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
  stored <-
    if viaDaemon
      then do
        let fifo = fifoPath root'
        fifoExists <- doesFileExist fifo
        unless fifoExists $ do
          TIO.putStrLn ("🔴 " <> T.pack fifo <> " — no bus.fifo; is the daemon running?")
          exitFailure
        postViaDaemon root' p
      else postChecked root' p
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
  stored <- unframeStored @Text line
  let p = stamped stored
  if deliversTo p names then Just line else Nothing

-- ---------------------------------------------------------------------------
-- read
-- ---------------------------------------------------------------------------

-- | Read a thread (all posts sharing a root id with the given post), or
-- posts addressed to NAMEs. A thread view ignores NAMEs.
runRead :: FilePath -> [Text] -> Maybe Since -> Maybe PostId -> Depth -> IO ()
runRead root names since mThread depth = do
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  when (not exists && mThread == Nothing) $ pure ()
  case mThread of
    Just tid -> do
      content <- if exists then readFileStrict path else pure ""
      runThreadView (postsOf content) tid depth
    Nothing -> do
      when (null names) $ do
        TIO.putStrLn "🔴 read requires NAME(s) or --thread ID"
        exitFailure
      sinceId <- case since of
        Nothing -> pure 0
        Just (SinceId pid) -> pure pid
        Just (SinceCursor name) -> readCursor root name
      content <- if exists then readFileStrict path else pure ""
      let lines' = T.lines content
      traverse_ (TIO.putStrLn . renderStored depth) (mapMaybe (filterSince names sinceId) lines')

postsOf :: Text -> [Stamped Text]
postsOf content = mapMaybe (unframeStored @Text) (filter (not . T.null) (T.lines content))

-- | The thread view: the connected component of the given id under the
-- schema's @thread@ field. Roots carry @thread = []@, replies cite their
-- ancestors, so the root of a post is the least ancestor present in the
-- log — the same walk BusStats uses to slice by thread.
runThreadView :: [Stamped Text] -> PostId -> Depth -> IO ()
runThreadView posts tid depth =
  case [vs | (_, vs) <- slicePosts ByThread posts, any ((== tid) . snd . stamp) vs] of
    (vs : _) -> traverse_ (TIO.putStrLn . renderStored depth) (sortOn (snd . stamp) vs)
    [] -> do
      TIO.putStrLn ("🔴 no post with id " <> T.pack (show tid))
      exitFailure

filterSince :: [Name] -> PostId -> Text -> Maybe (Stamped Text)
filterSince names sinceId line = do
  stored <- unframeStored @Text line
  let p = stamped stored
  guard (snd (stamp stored) >= sinceId)
  guard (deliversTo p names)
  pure stored

-- | Render a stored post at a cascade depth: the route, the line, the
-- verbatim frame.
renderStored :: Depth -> Stamped Text -> Text
renderStored (Depth d) stored
  | d <= 0 = headLine
  | d == 1 = headLine <> " " <> excerpt
  | otherwise = frameStored stored
  where
    p = stamped stored
    headLine = pid <> " " <> from p <> "→" <> route
    pid = T.pack (show (snd (stamp stored)))
    route = case [n | n <- to p, not (T.null n)] of
      [] -> "all"
      ns -> T.intercalate "," ns
    excerpt = T.take 72 (T.takeWhile (/= '\n') (T.strip (body p)))

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
      txt <- readFileStrict pingPath
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
--
-- With @--live@ the seat block is derived from the heartbeats instead: one
-- dot per seat, a line of state, cursor and last post, and — at depth 2 —
-- the heartbeat file and the seat's last post verbatim. A heartbeat with no
-- transition inside the threshold reads as crashed, never as live.
runStatus :: FilePath -> NominalDiffTime -> Bool -> Depth -> IO ()
runStatus root threshold live depth@(Depth d) = do
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ do
    TIO.putStrLn ("🔴 " <> T.pack root <> " — no log.jsonl; not a bus")
    exitFailure
  content <- readFileStrict path
  let ls = filter (not . T.null) (T.lines content)
      posts = mapMaybe (unframeStored @Text) ls
      idless = length ls - length posts
      maxId = maximum (0 : map (snd . stamp) posts)
  now <- getCurrentTime
  case posts of
    [] -> TIO.putStrLn ("🟡 " <> T.pack root <> " — empty bus (0 posts)")
    _ -> TIO.putStrLn (busLine now posts idless)
  entries <- listDirectory root
  let cursorNames = sort [T.pack (drop (length cursorPrefix) e) | e <- entries, cursorPrefix `isPrefixOf` e]
      hbNames = sort [T.pack (drop (length hbPrefix) e) | e <- entries, hbPrefix `isPrefixOf` e]
  if live
    then do
      let seats = sortNub (hbNames ++ cursorNames ++ participants posts)
      rows <-
        traverse
          ( \n -> do
              mcursor <- cursorIfExists n
              mhb <- readHeartbeat root n
              pure (n, mcursor, mhb)
          )
          seats
      traverse_ (renderSeat now posts) rows
    else when (d >= 1) $ do
      cursors <- traverse (\n -> (n,) <$> readCursor root n) cursorNames
      let statuses = map (seatStatus posts maxId) cursors
          behinds = [(n, c, u) | (n, c, Behind u) <- statuses]
          dones = [(n, c) | (n, c, Done) <- statuses]
          caughtUps = [(n, c) | (n, c, CaughtUp) <- statuses]
      if null cursorNames
        then TIO.putStrLn "seats: no cursors — nothing has ever read this bus"
        else do
          TIO.putStrLn
            ( "seats: "
                <> T.pack (show (length cursorNames))
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
    cursorPrefix = ".cursor-"
    hbPrefix = ".hb-"

    cursorIfExists n = do
      let p = root </> (".cursor-" <> T.unpack n)
      found <- doesFileExist p
      if found then Just <$> readCursor root n else pure Nothing

    -- The bus verdict: quiet is a read off the log, and a bus with no
    -- durable post in the threshold is quiet, not dead.
    busLine now posts idless =
      let lastPost = last posts
          age = diffUTCTime now (fst (stamp lastPost))
          liveNow = age < threshold
       in (if liveNow then "🟢 " else "🟡 ")
            <> T.pack root
            <> (if liveNow then " — live; " else " — quiet; ")
            <> T.pack (show (length posts))
            <> " posts"
            <> (if idless > 0 then " (" <> T.pack (show idless) <> " id-less)" else "")
            <> "; last id="
            <> T.pack (show (snd (stamp lastPost)))
            <> " from="
            <> from (stamped lastPost)
            <> " at "
            <> T.pack (show (fst (stamp lastPost)))
            <> " ("
            <> fmtAge age
            <> " ago)"

    -- One seat at the requested depth.
    renderSeat now posts (n, mcursor, mhb) = do
      let (dot, stateText) = hbVerdict now mhb
          cursorText = maybe "—" (T.pack . show) mcursor
          lastText = case lastFor n posts of
            Nothing -> "no posts"
            Just s ->
              "last post "
                <> T.pack (show (snd (stamp s)))
                <> " by "
                <> from (stamped s)
                <> " "
                <> fmtAge (diffUTCTime now (fst (stamp s)))
                <> " ago"
      if d <= 0
        then TIO.putStrLn (dot <> " " <> n)
        else do
          TIO.putStrLn (dot <> " " <> n <> " — " <> stateText <> "; cursor " <> cursorText <> "; " <> lastText)
          when (d >= 2) $ do
            forM_ mhb $ \(st, ts) ->
              TIO.putStrLn ("    hb: " <> hbText st <> " " <> T.pack (iso8601Show ts))
            forM_ (lastFor n posts) $ \s ->
              TIO.putStrLn ("    log: " <> frameStored s)

    -- A seat's liveness: no file is unknown, a stale file is crashed, a
    -- retire marker is retired, otherwise the state reads as written.
    hbVerdict now mhb = case mhb of
      Nothing -> ("⚪", "no heartbeat")
      Just (Dead, ts) -> ("🔴", "retired " <> fmtAge (diffUTCTime now ts) <> " ago")
      Just (st, ts)
        | diffUTCTime now ts > threshold ->
            ("🔴", "no transition for " <> fmtAge (diffUTCTime now ts) <> " — crashed?")
        | otherwise -> (hbGlyph st, hbText st <> " " <> fmtAge (diffUTCTime now ts) <> " ago")

    -- The last post a seat was party to, in either direction.
    lastFor n posts =
      listToMaybe $
        sortOn (Down . snd . stamp) $
          [s | s <- posts, from (stamped s) == n || n `elem` to (stamped s)]

    participants posts =
      sortNub
        ( [from (stamped s) | s <- posts]
            ++ concat [[n | n <- to (stamped s), not (T.null n)] | s <- posts]
        )

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
    latestBy n = listToMaybe . sortOn (Down . snd . stamp) . filter ((== n) . from . stamped)

fmtAge :: NominalDiffTime -> Text
fmtAge s
  | s < 120 = T.pack (show (round s :: Int)) <> "s"
  | s < 7200 = T.pack (show (round (s / 60) :: Int)) <> "m"
  | s < 172800 = T.pack (show (round (s / 3600) :: Int)) <> "h"
  | otherwise = T.pack (show (round (s / 86400) :: Int)) <> "d"
