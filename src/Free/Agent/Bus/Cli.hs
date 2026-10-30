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
--   read [ROOT] [NAME...] [--since ID | --cursor NAME] [--thread ID] [--depth 0|1|2]
--   inbox [ROOT] NAME [--mark-read] [--depth 0|1|2]
--   attach [ROOT] NAME
--   detach [ROOT] NAME
--   ready [ROOT] [--depth 0|1|2]
--   cursor [ROOT] get NAME
--   cursor [ROOT] set NAME ID
--   ping-watch [ROOT] NAME
--   status [ROOT] [--threshold SECS] [--live] [--depth 0|1|2]
--
-- @say@ is the act path for the deck: one short posting command, sender
-- defaulting to @runner@, body as positional words or on stdin. @post@ stays
-- the explicit form (JSON on stdin, or flags).
--
-- @inbox@ is the attached human's unread view: posts addressed to NAME at
-- or after @.cursor-NAME@, printed, and — only with @--mark-read@ — the
-- cursor advances to the id after the highest post printed, never to the
-- log tip. A missing cursor means the name has seen nothing: everything
-- addressed to it is unread, and the first @--mark-read@ bootstraps the
-- cursor file. @attach@ writes the per-root @.human-NAME@ marker and @detach@
-- removes it; the marker is evidence that a name is a person rather than a
-- seat, so no delivery, READY, claim, DONE or completion path reads it.
--
-- @--since@ and @--cursor@ are alternatives, not a composition: both are
-- accepted alone (@--since ID@, @--cursor NAME@) and naming both fails, so
-- the help says so rather than leaving the "|" to be read as a combined
-- form.
--
-- @--depth@ selects the cascade: 0 is a dot, 1 is one line, 2 is the
-- verbatim evidence. The depth is always an explicit flag; the api never
-- guesses the consumer. @status --live@ derives its dots from the seat
-- heartbeats (@.hb-NAME@, see "Free.Agent.Bus.Heartbeat"): pid evidence
-- separates a crash from a live but stale seat.
module Free.Agent.Bus.Cli
  ( BusCommand (..),
    Depth (..),
    busParser,
    depthOpt,
    hbVerdict,
    liveOpt,
    readyPosts,
    readyWarnings,
    runBusCommand,
    runStatus,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost, sortNub)
import Circuit.Agent.Framing (Stamped, frameStored, parsePost, stamp, stamped, unframeStored)
import Circuit.Agent.Mark (isHalt, markOf)
import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM_, forever, guard, unless, when)
import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, sort, sortOn)
import Data.Maybe (isJust, listToMaybe, mapMaybe, maybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (LocalTime, NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, localTimeToUTC, utc)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Free.Agent.Bus (postLocal, readFileStrict)
import Free.Agent.Bus.Daemon (fifoPath, postViaDaemon, runDaemon)
import Free.Agent.Bus.File (Human (..), attachHuman, humanNames, readCursor, readCursorAll, readHuman, removeHuman, writeCursor)
import Free.Agent.Bus.Heartbeat (HbState (..), Heartbeat, hbGlyph, hbPath, hbText, readHeartbeat)
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
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid)
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
  | InboxCommand FilePath Text Bool Depth
  | AttachCommand FilePath Text
  | DetachCommand FilePath Text
  | RetireCommand FilePath Text
  | ReadyCommand FilePath Depth
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
            <> help "Thread root (or any member) post id"
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
          <> value "runner"
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

inboxCmd :: Parser BusCommand
inboxCmd = do
  root <- rootOpt
  name <- nameArg
  markRead <-
    switch
      ( long "mark-read"
          <> help "Advance .cursor-NAME to the id after the highest post printed (default: leave the cursor alone)"
      )
  depth <- depthOpt (Depth 2)
  pure (InboxCommand root name markRead depth)

attachCmd :: Parser BusCommand
attachCmd = AttachCommand <$> rootOpt <*> nameArg

detachCmd :: Parser BusCommand
detachCmd = DetachCommand <$> rootOpt <*> nameArg

retireCmd :: Parser BusCommand
retireCmd = RetireCommand <$> rootOpt <*> nameArg

readyCmd :: Parser BusCommand
readyCmd = ReadyCommand <$> rootOpt <*> depthOpt (Depth 1)

-- | @--since@ and @--cursor@ are two independent options, and the parser
-- cannot accept them together: @option auto@ claims the token after
-- @--since@, so @--since --cursor NAME@ is read as @--since@ with the value
-- @--cursor@ and fails to parse it. The help is therefore the half that was
-- wrong — it left "|" to be read as a combined form — and it now states the
-- exclusion instead.
sinceParser :: Parser Since
sinceParser =
  ( SinceId
      <$> option
        auto
        ( long "since"
            <> metavar "ID"
            <> help "Only posts with id >= ID; an alternative to --cursor NAME, not a prefix of it"
        )
  )
    <|> ( SinceCursor . T.pack
            <$> strOption
              ( long "cursor"
                  <> metavar "NAME"
                  <> help "Only posts at or after .cursor-NAME; an alternative to --since ID"
              )
        )

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

-- | The @--live@ view selector: active-seat dots-and-lines from the heartbeats.
liveOpt :: Parser Bool
liveOpt = switch (long "live" <> help "Active seat dots-and-lines: heartbeat state, cursor, last post id")

daemonCmd :: Parser BusCommand
daemonCmd = DaemonCommand <$> rootOpt

busParser :: Parser BusCommand
busParser =
  subparser
    ( command "say" (info (sayCmd <**> helper) (progDesc "Say something on the bus; --thread carries reply ancestry (deck defaults: --from runner)"))
        <> command "post" (info (postCmd <**> helper) (progDesc "Post flags or JSON; flag posts are roots, while JSON may carry thread ancestry"))
        <> command "watch" (info (watchCmd <**> helper) (progDesc "Watch the log for posts addressed to NAMEs"))
        <> command "read" (info (readCmd <**> helper) (progDesc "Read posts addressed to NAMEs, or a thread by id"))
        <> command "inbox" (info (inboxCmd <**> helper) (progDesc "Print unread posts addressed to NAME; --mark-read advances .cursor-NAME to the last printed id"))
        <> command "attach" (info (attachCmd <**> helper) (progDesc "Record NAME as the human attached to this root (evidence only; changes no delivery or READY path)"))
        <> command "detach" (info (detachCmd <**> helper) (progDesc "Remove NAME's attached-human marker (the cursor and unread mail are untouched)"))
        <> command "retire" (info (retireCmd <**> helper) (progDesc "Write the dead marker to NAME's heartbeat: the seat leaves the live roster, its cursor, log and transcripts stay as evidence; no process is signalled"))
        <> command "ready" (info (readyCmd <**> helper) (progDesc "List unclaimed open TASKs with no unresolved blocker"))
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
runBusCommand (InboxCommand root name markRead depth) = runInbox root name markRead depth
runBusCommand (AttachCommand root name) = runAttach root name
runBusCommand (DetachCommand root name) = runDetach root name
runBusCommand (RetireCommand root name) = runRetire root name
runBusCommand (ReadyCommand root depth) = runReady root depth
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
  requireBus root
  postLocal root p

-- | The one wrong-root refusal: a root with no @log.jsonl@ is not a bus.
noBusMessage :: FilePath -> Text
noBusMessage root =
  "🔴 "
    <> T.pack root
    <> " — no log.jsonl; not a bus. Set FREE_AGENT_BUS_ROOT or use --root."

-- | Refuse loudly, and exit non-zero, unless ROOT holds a bus log.
requireBus :: FilePath -> IO ()
requireBus root = do
  exists <- doesFileExist (root </> "log.jsonl")
  unless exists $ do
    TIO.putStrLn (noBusMessage root)
    exitFailure

-- | One command on the act path: body as words or on stdin, sender and
-- recipients defaulted, thread optional.
runSay :: FilePath -> Text -> [Text] -> Maybe PostId -> [Text] -> IO ()
runSay root fromName toNames thr bodyWords = do
  root' <- resolveRoot root
  when (any T.null toNames) $ do
    TIO.hPutStrLn stderr "🔴 say: empty recipient name — omit --to to post unaddressed (visible in the log, delivered to no seat)"
    exitFailure
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
    (Just fromName, Just bodyText) -> do
      when (any T.null mto) $ do
        TIO.hPutStrLn stderr "🔴 post: empty recipient name — omit --to to post unaddressed (visible in the log, delivered to no seat)"
        exitFailure
      pure (mkPost fromName mto bodyText)
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
-- posts addressed to NAMEs. A thread view ignores NAMEs. A path that is not
-- a bus is refused like every other command refuses it.
runRead :: FilePath -> [Text] -> Maybe Since -> Maybe PostId -> Depth -> IO ()
runRead root names since mThread depth = do
  root' <- resolveRoot root
  requireBus root'
  let path = root' </> "log.jsonl"
  content <- readFileStrict path
  case mThread of
    Just tid -> runThreadView (postsOf content) tid depth
    Nothing -> do
      when (null names) $ do
        TIO.putStrLn "🔴 read requires NAME(s) or --thread ID"
        exitFailure
      sinceId <- case since of
        Nothing -> pure 0
        Just (SinceId pid) -> pure pid
        Just (SinceCursor name) -> readCursorAll root' name
      let lines' = T.lines content
      traverse_ (TIO.putStrLn . renderStored depth) (mapMaybe (filterSince names sinceId) lines')

-- | Print NAME's unread mail: posts addressed to NAME with id at or after
-- @.cursor-NAME@ (the cursor holds the next unread id, as 'readCursor' and
-- 'writeCursor' define it). The cursor is the name's own state and moves
-- only with @--mark-read@, and then only to the id after the highest post
-- printed: never to the log tip, never past a post that was not printed.
-- The view labels nothing and changes no delivery path.
runInbox :: FilePath -> Text -> Bool -> Depth -> IO ()
runInbox root name markRead depth = do
  root' <- resolveRoot root
  requireBus root'
  content <- readFileStrict (root' </> "log.jsonl")
  sinceId <- readCursorAll root' name
  let shown = mapMaybe (filterSince [name] sinceId) (T.lines content)
  traverse_ (TIO.putStrLn . renderStored depth) shown
  when (markRead && not (null shown)) $
    writeCursor root' name (highestId shown + 1)

-- | The highest id in a non-empty set of stored posts.
highestId :: [Stamped Text] -> PostId
highestId = maximum . map (snd . stamp)

-- | Record NAME as the human attached to this root: write the per-root
-- @.human-NAME@ marker carrying @root@, @name@, @attached_at@ and the
-- cursor value read at attach time.
--
-- Idempotent: an existing marker is left byte-identical, so re-attaching
-- never rewrites @attached_at@ and never moves the cursor. The marker is
-- evidence, not authority — it changes no delivery, READY, claim, DONE or
-- completion path, and it is not a second cursor owner.
runAttach :: FilePath -> Text -> IO ()
runAttach root name = do
  root' <- resolveRoot root
  requireBus root'
  existing <- readHuman root' name
  case existing of
    Just h -> TIO.putStrLn ("✓ already " <> humanLine h)
    Nothing -> do
      h <- attachHuman root' name
      TIO.putStrLn ("✓ " <> humanLine h)

-- | The one line that says what the marker says: @attached as NAME on ROOT@.
humanLine :: Human -> Text
humanLine h =
  "attached as "
    <> humanName h
    <> " on "
    <> T.pack (humanRoot h)
    <> " (cursor "
    <> T.pack (show (humanCursor h))
    <> ", since "
    <> T.pack (iso8601Show (humanAttachedAt h))
    <> ")"

-- | Remove NAME's attached-human marker. The cursor and the log are
-- untouched, so mail unread while detached stays unread.
runDetach :: FilePath -> Text -> IO ()
runDetach root name = do
  root' <- resolveRoot root
  requireBus root'
  removed <- removeHuman root' name
  TIO.putStrLn
    ( "✓ "
        <> (if removed then "detached" else "not attached")
        <> " as "
        <> name
        <> " on "
        <> T.pack root'
    )

-- | Retire a seat: write the @dead@ marker to its heartbeat. The seat
-- leaves the live roster from the next @status --live@; its cursor, log
-- posts, transcripts and the heartbeat file itself stay as evidence. No
-- process is signalled — a genuinely live seat's next heartbeat write
-- overwrites the marker, which is the honest outcome of a mistaken
-- retirement. The pid recorded is the seat's last known one, not the
-- caller's.
runRetire :: FilePath -> Text -> IO ()
runRetire root name = do
  root' <- resolveRoot root
  requireBus root'
  mhb <- readHeartbeat root' name
  ts <- getCurrentTime
  let mpid = case mhb of
        Just (_, _, p) -> p
        Nothing -> Nothing
  TIO.writeFile
    (hbPath root' name)
    ( "dead "
        <> T.pack (iso8601Show ts)
        <> maybe "" (\p -> " " <> T.pack (show p)) mpid
        <> "\n"
    )
  TIO.putStrLn ("🔴 " <> name <> " retired on " <> T.pack root')

postsOf :: Text -> [Stamped Text]
postsOf content = mapMaybe (unframeStored @Text) (filter (not . T.null) (T.lines content))

runReady :: FilePath -> Depth -> IO ()
runReady root depth = do
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  content <- if exists then readFileStrict path else pure ""
  let posts = postsOf content
  traverse_ (TIO.putStrLn . renderStored depth) (readyPosts posts)
  traverse_ (TIO.hPutStrLn stderr) (readyWarnings posts)

-- | Open TASK offers that have no claim, owner DONE, or unresolved blocker.
-- A directed TASK is assigned at creation and is therefore not an open offer.
readyPosts :: [Stamped Text] -> [Stamped Text]
readyPosts posts = filter isReady [task | task <- posts, isRootTask task, null (to (stamped task))]
  where
    isReady task =
      let component = componentOf posts task
          owner = taskOwner task component
          blockers = blockersFor (snd (stamp task))
       in owner == Nothing
            && not (taskComplete owner component)
            && all blockerResolved blockers

    blockersFor taskId =
      [ blocker
      | stored <- posts,
        Just (blocker, target) <- pure (parseBlockLink (body (stamped stored))),
        target == taskId
      ]

    blockerResolved blocker =
      any
        ( \task ->
            let component = componentOf posts task
             in snd (stamp task) == blocker
                  && isRootTask task
                  && taskComplete (taskOwner task component) component
        )
        posts

-- | Diagnose CLAIM and DONE posts that cannot settle any incomplete public
-- offer because they lie outside every such offer's thread component.
readyWarnings :: [Stamped Text] -> [Text]
readyWarnings posts =
  [ warning stray offer
  | stray <- posts,
    hasKind "CLAIM" stray || hasKind "DONE" stray,
    let offers = openOffers posts,
    all (outside stray . componentOf posts) (taskOffers posts),
    Just offer <- [resemblingOffer stray offers]
  ]
  where
    taskOffers = filter (\task -> isRootTask task && null (to (stamped task)))

    openOffers =
      filter
        ( \task ->
            let component = componentOf posts task
             in not (taskComplete (taskOwner task component) component)
        )
        . taskOffers

    outside stray component = snd (stamp stray) `notElem` map (snd . stamp) component

    warning stray offer =
      let strayId = T.pack (show (snd (stamp stray)))
          offerId = T.pack (show (snd (stamp offer)))
          kind = T.takeWhile (not . (`elem` [' ', '\n', '\r', '\t'])) (T.stripStart (body (stamped stray)))
       in "🟡 post "
            <> strayId
            <> " "
            <> kind
            <> " is outside every open offer component; resembles offer "
            <> offerId
            <> " — re-post with bus say --thread "
            <> offerId

-- | Choose the closest-looking offer without changing offer or component
-- semantics: an explicit id wins, then shared body tokens, then id distance.
resemblingOffer :: Stamped Text -> [Stamped Text] -> Maybe (Stamped Text)
resemblingOffer stray offers =
  listToMaybe
    [ offer
    | offer <- sortOn resemblance offers,
      let (explicitId, sharedTokens) = scores offer,
      explicitId || sharedTokens > 0
    ]
  where
    strayId = snd (stamp stray)
    strayTokens = bodyTokens (body (stamped stray))
    resemblance offer =
      let (explicitId, sharedTokens) = scores offer
       in (Down explicitId, Down sharedTokens, abs (strayId - snd (stamp offer)))

    scores offer =
      let offerId = snd (stamp offer)
          offerTokens = bodyTokens (body (stamped offer))
       in (T.pack (show offerId) `elem` strayTokens, length (filter (`elem` offerTokens) strayTokens))

    bodyTokens = T.words . T.toLower . T.map (\c -> if isAlphaNum c then c else ' ')

componentOf :: [Stamped Text] -> Stamped Text -> [Stamped Text]
componentOf posts task =
  case [vs | (_, vs) <- slicePosts ByThread posts, any ((== snd (stamp task)) . snd . stamp) vs] of
    (vs : _) -> sortOn (snd . stamp) vs
    [] -> [task]

taskOwner :: Stamped Text -> [Stamped Text] -> Maybe Name
taskOwner task component =
  case to (stamped task) of
    [assignee] -> Just assignee
    [] -> listToMaybe [from (stamped reply) | reply <- component, hasKind "CLAIM" reply]
    _ -> Nothing

taskComplete :: Maybe Name -> [Stamped Text] -> Bool
taskComplete owner component =
  maybe False (\name -> any (\reply -> hasKind "DONE" reply && from (stamped reply) == name) component) owner

isRootTask :: Stamped Text -> Bool
isRootTask stored = null (thread (stamped stored)) && hasKind "TASK" stored

hasKind :: Text -> Stamped Text -> Bool
hasKind kind stored = case T.words (body (stamped stored)) of
  (actual : _) -> actual == kind
  [] -> False

parseBlockLink :: Text -> Maybe (PostId, PostId)
parseBlockLink text = case T.words text of
  ("LINK" : "·" : blocker : "blocks" : target : _) ->
    (,) <$> readMaybe (T.unpack blocker) <*> readMaybe (T.unpack target)
  _ -> Nothing

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
renderStored = renderStoredWith (const "")

-- | 'renderStored' with a per-name label appended to the names of the head
-- line. The label is evidence for a reader — this name is a person, not a
-- seat — and it never reaches depth 2, which is the log's own frame.
--
-- The label annotates names the row already carries; it adds, removes and
-- reorders nothing, so a view's rows are the same rows with or without it.
renderStoredWith :: (Name -> Text) -> Depth -> Stamped Text -> Text
renderStoredWith label (Depth d) stored
  | d <= 0 = headLine
  | d == 1 = headLine <> " " <> excerpt
  | otherwise = frameStored stored
  where
    p = stamped stored
    headLine = pid <> " " <> mark (from p) <> "→" <> route
    pid = T.pack (show (snd (stamp stored)))
    route = case [n | n <- to p, not (T.null n)] of
      [] -> "public"
      ns -> T.intercalate "," (map mark ns)
    mark n = n <> label n
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

-- | Liveness from state, age, and (for current files) a process probe.
-- An explicit dead marker is retired and a dead pid is crashed. Any live
-- state silent past the threshold reads stale — a live pid does not rescue
-- it, because pids are recycled and a heartbeat older than the threshold
-- is a stale seat whatever the process table says. Fresh idles stay green.
hbVerdict :: NominalDiffTime -> UTCTime -> Maybe Heartbeat -> IO (Text, Text)
hbVerdict threshold now mhb = case mhb of
  Nothing -> pure ("⚪", "no heartbeat")
  Just (Dead, ts, _) -> pure ("🔴", "retired " <> fmtAge (diffUTCTime now ts) <> " ago")
  Just (st, ts, mpid) -> do
    alive <- maybe (pure Nothing) pidAlive mpid
    let age = diffUTCTime now ts
        stale = age > threshold
        verdict
          | alive == Just False = ("🔴", "pid " <> maybe "?" (T.pack . show) mpid <> " is not running — crashed?")
          | stale && alive == Just True = ("🟡", hbText st <> " " <> fmtAge age <> " ago; pid " <> maybe "?" (T.pack . show) mpid <> " alive — stale?")
          | stale = ("⚪", hbText st <> " " <> fmtAge age <> " ago; liveness unknown — stale?")
          | otherwise = (hbGlyph st, hbText st <> " " <> fmtAge age <> " ago")
    pure verdict

pidAlive :: CPid -> IO (Maybe Bool)
pidAlive pid = do
  result <- try @IOException (signalProcess nullSignal pid)
  pure $ case result of
    Right () -> Just True
    Left e
      | isDoesNotExistError e -> Just False
      | isPermissionError e -> Just True
      | otherwise -> Nothing

fmtAge :: NominalDiffTime -> Text
fmtAge s
  | s < 120 = T.pack (show (round s :: Int)) <> "s"
  | s < 7200 = T.pack (show (round (s / 60) :: Int)) <> "m"
  | s < 172800 = T.pack (show (round (s / 3600) :: Int)) <> "h"
  | otherwise = T.pack (show (round (s / 86400) :: Int)) <> "d"

-- | Print a bus status report.
--
-- A seat whose latest own post is a halt mark (🟢/🔵) is reported as @done@
-- rather than @behind@: decided quiet is a read, not an inference. The
-- unread figure behind a seat is the mail count — what 'runInbox' would
-- print for that name — so another seat's posts and public posts never
-- read as its mail.
--
-- With @--live@ the active seat block is derived from the heartbeats instead:
-- one dot per seat, a line of state, cursor and last post, and — at depth 2 —
-- the heartbeat file and the seat's last post verbatim. Explicitly retired
-- seats leave this view; their heartbeats and log evidence stay on disk. Stale
-- wake and working states warn as stale when their pid is alive, or as unknown
-- when the probe is inconclusive; a dead pid reads crashed and stays visible.
-- Idle uses the heartbeat pid for liveness.
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
      humans <- humanNames root
      let seats = sortNub (hbNames ++ cursorNames ++ humans ++ participants posts)
      rows <-
        traverse
          ( \n -> do
              mcursor <- cursorIfExists n
              mhb <- readHeartbeat root n
              pure (n, mcursor, mhb)
          )
          seats
      traverse_ (renderSeat now posts) (filter (\(_, _, mhb) -> activeSeat mhb) rows)
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
    activeSeat (Just (Dead, _, _)) = False
    activeSeat _ = True

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

    -- One seat at the requested depth. A name is its own label: attached
    -- humans render exactly like seats, by name, with no role tag.
    renderSeat now posts (n, mcursor, mhb) = do
      (dot, stateText) <- hbVerdict threshold now mhb
      let cursorText = maybe "—" (T.pack . show) mcursor
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
            forM_ mhb $ \(st, ts, mpid) ->
              TIO.putStrLn
                ( "    hb: "
                    <> hbText st
                    <> " "
                    <> T.pack (iso8601Show ts)
                    <> " pid="
                    <> maybe "legacy" (T.pack . show) mpid
                )
            forM_ (lastFor n posts) $ \s ->
              TIO.putStrLn ("    log: " <> frameStored s)

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
      | otherwise = (n, c, Behind (fromIntegral unread))
      where
        -- The unread figure is the mail count — what 'runInbox' would print
        -- for the name — not the log span after the cursor, so other
        -- people's posts and public posts do not read as this name's mail.
        unread = length [() | s <- posts, snd (stamp s) >= c, deliversTo (stamped s) [n]]
        isDone =
          case latestBy n posts of
            Nothing -> False
            Just stored -> maybe False isHalt (markOf (stamped stored))

    latestBy :: Name -> [Stamped Text] -> Maybe (Stamped Text)
    latestBy n = listToMaybe . sortOn (Down . snd . stamp) . filter ((== n) . from . stamped)
