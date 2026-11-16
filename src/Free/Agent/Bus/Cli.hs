{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | free-agent bus CLI.
--
-- Subcommands:
--
--   post [ROOT] [--from NAME] [--to NAME]... [--body TEXT]
--   watch [ROOT] NAME [NAME...]
--   read [ROOT] [NAME...] [--since ID | --cursor NAME] [--thread ID] [--depth 0|1|2]
--   org [ROOT] --thread ID
--   inbox [ROOT] NAME [--mark-read] [--depth 0|1|2]
--   attach [ROOT] NAME
--   detach [ROOT] NAME
--   ready [ROOT] [--depth 0|1|2]
--   cursor [ROOT] get NAME
--   cursor [ROOT] set NAME ID
--   status [ROOT] [--threshold SECS] [--depth 0|1|2] [--org]
--
-- @post@ is the only posting command: the body is @--body TEXT@ or piped on
-- stdin with @--from@, and a whole post line on stdin is the JSON mode where
-- the caller states its own ancestry and channel. @--to@ and @--channel@ are
-- alternative addresses, never both: delivery follows the channel, so the
-- pair is refused rather than accepted with the addressee dropped.
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
-- @--depth@ selects the cascade: 0 is a census, 1 adds seat rows, 2 adds
-- heartbeat, cursor, and last-post evidence. The depth is always an explicit
-- flag; the api never guesses the consumer.
--
-- @status@ reads one snapshot of heartbeats, pids, cursors, and the log:
-- circle membership, directed unread counts, and last-post anchors. @--org@
-- selects the Org renderer for @fab-open-status@, not another projection.
module Free.Agent.Bus.Cli
  ( BusCommand (..),
    Depth (..),
    SeatCircle (..),
    addressConflict,
    applyPostFlags,
    busParser,
    checkThreadId,
    depthOpt,
    orgOpt,
    readyPosts,
    readyWarnings,
    runBusCommand,
    runStatus,
    seatCircle,
  )
where

import Bus.Flow (SliceMode (..), slicePosts)
import Circuit.Agent (Name)
import Circuit.Parser.Framing (Stored, encodeRow)
import Circuit.Post (Post (..), PostId, deliversTo, mkPost, sortNub)
import Circuit.Shared (stamp, stamped)
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (forM_, unless, when)
import Data.Aeson (eitherDecodeStrict, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum, isAsciiUpper, isDigit)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, sort, sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Free.Agent.Bus (postLocal, readLogStrict, readSince)
import Free.Agent.Bus.File (Flow (Continue), Human (..), attachHuman, humanNames, readCursor, readCursorAll, readHuman, removeHuman, tailLog, writeCursor)
import Free.Agent.Bus.Heartbeat (HbState (..), Heartbeat, hbPath, hbText, readHeartbeat)
import Free.Agent.Cli.Config (rootOpt)
import Options.Applicative
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hIsTerminalDevice, stderr, stdin)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid)
import Text.Read (readMaybe)

-- | Cascade depth: 0 is a census, 1 adds rows, 2 adds verbatim evidence.
newtype Depth = Depth Int
  deriving (Eq, Show)

data Since = SinceId PostId | SinceCursor Text
  deriving (Show)

data BusCommand
  = PostCommand FilePath (Maybe Text) [Text] (Maybe Text) (Maybe PostId) (Maybe Text)
  | WatchCommand FilePath [Text]
  | ReadCommand FilePath [Text] (Maybe Since) (Maybe PostId) Depth
  | OrgCommand FilePath (Maybe PostId)
  | InboxCommand FilePath Text Bool Depth
  | AttachCommand FilePath Text
  | DetachCommand FilePath Text
  | RetireCommand FilePath Text
  | ReadyCommand FilePath Depth
  | CursorGetCommand FilePath Text
  | CursorSetCommand FilePath Text PostId
  | StatusCommand FilePath NominalDiffTime Depth Bool
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

nameArg :: Parser Text
nameArg = argument (T.pack <$> str) (metavar "NAME" <> help "Agent name")

recipientReader :: ReadM Text
recipientReader = eitherReader $ \s ->
  let name = T.pack s
   in if T.isInfixOf "," name
        then Left ("recipient contains a comma: " <> s)
        else Right name

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
        <> help "Cascade depth: 0 summary, 1 rows, 2 evidence"
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

-- | @--channel NAME@: the channel is the address, and a post in a channel
-- needs no @--to@. Delivery follows the channel: 'deliversTo' matches a channel
-- post against a seat's subscriptions, and ignores its @to@ list.
channelOpt :: Parser (Maybe Text)
channelOpt =
  optional
    ( option
        (T.pack <$> str)
        ( long "channel"
            <> metavar "NAME"
            <> help "Channel name: the channel is the whole address, so it needs no --to"
        )
    )

-- | The @--thread@ rule shared by the flag path and the JSON path, so the two
-- paths agree on one thing: the parent must exist in the log. Ids are
-- append-order, so a parent missing at write time is a typo, not a post still
-- to come. The raw JSON path
-- keeps the log's tolerance for a dangling link: that ancestry is the caller's
-- own statement, while a flag is this tool's.
-- | Fold the @--thread@ and @--channel@ flags into a post. A flag is this
-- tool's statement, so it wins over whatever the JSON said; with neither flag
-- the post is exactly what the caller wrote, which is the compatibility rule
-- the JSON path relies on.
-- | Parse the un-stamped JSON accepted by @bus post@ on stdin.
parsePostText :: Text -> Either String (Post Text Text)
parsePostText line =
  eitherDecodeStrict (TE.encodeUtf8 line)
    >>= parseEither
      ( withObject "Post" $ \o ->
          Post
            <$> o .: "from"
            <*> o .:? "channel"
            <*> o .: "to"
            <*> o .:? "thread" .!= []
            <*> o .: "body"
      )

applyPostFlags :: Maybe PostId -> Maybe Text -> Post Text a -> Post Text a
applyPostFlags mthr mchan p =
  p
    { thread = maybe (thread p) pure mthr,
      channel = mchan <|> channel p
    }

-- | A post addressed two ways at once. Delivery follows the channel
-- ('deliversTo' matches it before @to@), so a @--to@ list beside a
-- @--channel@ would be written to the log and silently dropped. Rather than
-- choosing for the caller, the pair is refused with a diagnostic that names
-- both addresses: an accepted-then-ignored addressee is the failure a seat
-- cannot see.
addressConflict :: Maybe Text -> [Text] -> Maybe Text
addressConflict mchan tos = case (mchan, filter (not . T.null) tos) of
  (Just chan, named@(_ : _))
    | not (T.null chan) ->
        Just $
          "🔴 post: --channel "
            <> chan
            <> " and --to "
            <> T.intercalate " " named
            <> " both name an address — delivery follows the channel, so the addressee would be dropped; pass one"
  _ -> Nothing

checkThreadId :: FilePath -> PostId -> IO (Either Text ())
checkThreadId root' tid = do
  exists <- doesFileExist (root' </> "log.jsonl")
  if not exists
    then pure (Right ())
    else do
      posts <- loadPosts (root' </> "log.jsonl")
      let ids = map (snd . stamp) posts
      pure $
        if tid `elem` ids
          then Right ()
          else Left ("no post with id " <> T.pack (show tid) <> " — threads attach to an existing post")

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
          recipientReader
          (long "to" <> metavar "NAME" <> help "Recipient name (repeatable, no commas); refused beside --channel, which is the whole address")
      )
  bodyText <-
    optional
      ( option
          (T.pack <$> str)
          (long "body" <> metavar "TEXT" <> help "Post body")
      )
  thr <- threadOpt
  chan <- channelOpt
  pure (PostCommand root fromName toNames bodyText thr chan)

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

orgCmd :: Parser BusCommand
orgCmd = OrgCommand <$> rootOpt <*> threadOpt

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

thresholdOpt :: Parser NominalDiffTime
thresholdOpt =
  option
    (eitherReader readSeconds)
    ( long "threshold"
        <> short 't'
        <> metavar "SECS"
        <> value 900
        <> showDefault
        <> help "Seconds before an unverifiable heartbeat reads as non-fab"
    )
  where
    readSeconds s =
      case reads s of
        [(n, "")] -> Right (fromInteger n :: NominalDiffTime)
        _ -> Left ("expected a whole number of seconds, got: " ++ s)

statusCmd :: Parser BusCommand
statusCmd = StatusCommand <$> rootOpt <*> thresholdOpt <*> depthOpt (Depth 1) <*> orgOpt

-- | Org renders the same status snapshot for @fab-open-status@.
orgOpt :: Parser Bool
orgOpt = switch (long "org" <> help "Org status output for fab-open-status")

busParser :: Parser BusCommand
busParser =
  subparser
    ( command "post" (info (postCmd <**> helper) (progDesc "Post flags or JSON; --thread carries reply ancestry, --channel names the channel; the body is --body or piped on stdin"))
        <> command "watch" (info (watchCmd <**> helper) (progDesc "Watch the log for posts addressed to NAMEs"))
        <> command "read" (info (readCmd <**> helper) (progDesc "Read posts addressed to NAMEs, or a thread by id"))
        <> command "org" (info (orgCmd <**> helper) (progDesc "Render a thread as an Org-mode conversation: * [#ID] with channel when present, ** sender LABEL per post"))
        <> command "inbox" (info (inboxCmd <**> helper) (progDesc "Print unread posts addressed to NAME; --mark-read advances .cursor-NAME to the last printed id"))
        <> command "attach" (info (attachCmd <**> helper) (progDesc "Record NAME as the human attached to this root (evidence only; changes no delivery or READY path)"))
        <> command "detach" (info (detachCmd <**> helper) (progDesc "Remove NAME's attached-human marker (the cursor and unread mail are untouched)"))
        <> command "retire" (info (retireCmd <**> helper) (progDesc "Write the dead marker to NAME's heartbeat: the seat joins the retired circle, its cursor, log and transcripts stay as evidence; no process is signalled"))
        <> command "ready" (info (readyCmd <**> helper) (progDesc "List unclaimed open TASKs with no unresolved blocker"))
        <> command "cursor" (info (cursorCmd <**> helper) (progDesc "Read or write agent cursor"))
        <> command "status" (info (statusCmd <**> helper) (progDesc "Unified seat census: circle counts, directed unread, and last-post anchors"))
    )

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

runBusCommand :: BusCommand -> IO ()
runBusCommand (PostCommand root mfrom mto mbody mthr mchan) =
  runPost root mfrom mto mbody mthr mchan
runBusCommand (WatchCommand root names) = runWatch root names
runBusCommand (ReadCommand root names since thr depth) = runRead root names since thr depth
runBusCommand (OrgCommand root thr) = runOrg root thr
runBusCommand (InboxCommand root name markRead depth) = runInbox root name markRead depth
runBusCommand (AttachCommand root name) = runAttach root name
runBusCommand (DetachCommand root name) = runDetach root name
runBusCommand (RetireCommand root name) = runRetire root name
runBusCommand (ReadyCommand root depth) = runReady root depth
runBusCommand (CursorGetCommand root name) = runCursorGet root name
runBusCommand (CursorSetCommand root name pid) = runCursorSet root name pid
runBusCommand (StatusCommand root threshold depth org) = runStatus root threshold depth org

-- ---------------------------------------------------------------------------
-- act: post
-- ---------------------------------------------------------------------------

-- | Resolve the bus root: @FREE_AGENT_BUS_ROOT@ wins when @--root@ is the
-- default @bus/@, so a shell in ~/coffee never grows a stray log.
resolveRoot :: FilePath -> IO FilePath
resolveRoot root = do
  envRoot <- lookupEnv "FREE_AGENT_BUS_ROOT"
  pure $ case envRoot of
    Just r | root == "bus/" -> r
    _ -> root

-- | Post to a root, refusing loudly when there is no bus there.
postChecked :: FilePath -> Post Text Text -> IO (Stored)
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
runPost :: FilePath -> Maybe Text -> [Text] -> Maybe Text -> Maybe PostId -> Maybe Text -> IO ()
runPost root mfrom mto mbody mthr mchan = do
  root' <- resolveRoot root
  -- An addressee beside a channel would be written and then ignored; refuse it
  -- here, where the flags are visible, rather than let the log carry a
  -- delivery that cannot happen.
  forM_ (addressConflict mchan mto) $ \why -> do
    TIO.hPutStrLn stderr why
    exitFailure
  -- Two modes, chosen by `--from`: the flag mode (the body is `--body` or piped
  -- stdin) and the JSON mode (a whole post line on stdin, which is the caller's
  -- own statement of ancestry and channel). The old positional-body @say@ body
  -- path lives here, so a caller that pipes text keeps working with one word
  -- changed.
  built <- case mfrom of
    Just fromName -> do
      when (any T.null mto) $ do
        TIO.hPutStrLn stderr "🔴 post: empty recipient name — omit --to to post unaddressed (visible in the log, delivered to no seat)"
        exitFailure
      bodyText <- case mbody of
        Just b -> do
          -- A body named twice is ambiguous: refuse rather than silently drop
          -- the pipe. A terminal on stdin is not a second body.
          tty <- hIsTerminalDevice stdin
          piped <- if tty then pure "" else T.strip <$> TIO.getContents
          unless (T.null piped) $ do
            TIO.hPutStrLn stderr "🔴 post: stdin and --body both name a body — pass one"
            exitFailure
          pure b
        Nothing -> do
          tty <- hIsTerminalDevice stdin
          if tty
            then do
              TIO.hPutStrLn stderr "🔴 post: nothing to post — pass --body TEXT or pipe the body on stdin"
              exitFailure
            else T.strip <$> TIO.getContents
      when (T.null bodyText) $ do
        TIO.hPutStrLn stderr "🔴 post: empty body"
        exitFailure
      pure (mkPost fromName mto bodyText)
    Nothing -> case mbody of
      Just _ -> do
        TIO.putStrLn "🔴 post requires either --from (with --body or a piped body) or JSON on stdin"
        exitFailure
      Nothing -> do
        line <- TIO.getLine
        case parsePostText line of
          Left _ -> do
            TIO.putStrLn "🔴 invalid post JSON"
            exitFailure
          Right p -> pure p
  forM_ mchan $ \c ->
    when (T.null c) $ do
      TIO.hPutStrLn stderr "🔴 post: empty channel name — omit --channel to address by --to"
      exitFailure
  forM_ mthr $ \tid -> do
    verdict <- checkThreadId root' tid
    case verdict of
      Left why -> do
        TIO.hPutStrLn stderr ("🔴 post: " <> why)
        exitFailure
      Right () -> pure ()
  stored <- postChecked root' (applyPostFlags mthr mchan built)
  TIO.putStrLn (encodeRow stored)

-- ---------------------------------------------------------------------------
-- watch
-- ---------------------------------------------------------------------------

runWatch :: FilePath -> [Text] -> IO ()
runWatch root names = do
  when (null names) $ do
    TIO.putStrLn "🔴 watch requires at least one name"
    exitFailure
  let path = root </> "log.jsonl"
  exists <- doesFileExist path
  posts <- if exists then loadPosts path else pure []
  let start = case posts of
        [] -> 0
        _ -> highestId posts + 1
  tailLog path names start Nothing $ \batch -> do
    traverse_ (TIO.putStrLn . encodeRow) batch
    pure Continue

-- | Strictly decode the complete log. A malformed row is an error, never an
-- empty view; the diagnostic carries its 1-based row and 0-based character
-- offset.
loadPosts :: FilePath -> IO [Stored]
loadPosts path = do
  posts <- readLogStrict path
  pure posts

-- | Read a thread (all posts sharing a root id with the given post), or
-- posts addressed to NAMEs. A thread view ignores NAMEs. A path that is not
-- a bus is refused like every other command refuses it.
runRead :: FilePath -> [Text] -> Maybe Since -> Maybe PostId -> Depth -> IO ()
runRead root names since mThread depth = do
  root' <- resolveRoot root
  requireBus root'
  let path = root' </> "log.jsonl"
  posts <- loadPosts path
  case mThread of
    Just tid -> runThreadView posts tid depth
    Nothing -> do
      when (null names) $ do
        TIO.putStrLn "🔴 read requires NAME(s) or --thread ID"
        exitFailure
      sinceId <- case since of
        Nothing -> pure 0
        Just (SinceId pid) -> pure pid
        Just (SinceCursor name) -> readCursorAll root' name
      traverse_ (TIO.putStrLn . renderStored depth) (readSince posts names sinceId)

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
  let path = root' </> "log.jsonl"
  posts <- loadPosts path
  sinceId <- readCursorAll root' name
  let shown = readSince posts [name] sinceId
  traverse_ (TIO.putStrLn . renderStored depth) shown
  when (markRead && not (null shown)) $
    writeCursor root' name (highestId shown + 1)

-- | The highest id in a non-empty set of stored posts.
highestId :: [Stored] -> PostId
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

-- | Retire a seat: write the @dead@ marker to its heartbeat. The seat joins
-- the black retired circle in the next status; its cursor, log posts,
-- transcripts and the heartbeat file itself stay as evidence. No process is
-- signalled — a genuinely live seat's next heartbeat write overwrites the
-- marker, which is the honest outcome of a mistaken retirement. The pid
-- recorded is the seat's last known one, not the caller's.
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
  TIO.putStrLn ("⚫ " <> name <> " retired on " <> T.pack root')

-- | Print the open offers, then the lint rows.  The root must be a bus
-- like every other command requires, and a view that dies partway names
-- its death on stderr and exits non-zero rather than trailing off like
-- an empty queue.
runReady :: FilePath -> Depth -> IO ()
runReady root depth = do
  root' <- resolveRoot root
  requireBus root'
  view <- try @SomeException $ do
    posts <- loadPosts (root' </> "log.jsonl")
    traverse_ (TIO.putStrLn . renderStored depth) (readyPosts posts)
    traverse_ (TIO.hPutStrLn stderr) (readyWarnings posts)
  case view of
    Right () -> pure ()
    Left e -> do
      TIO.hPutStrLn
        stderr
        ( "🔴 ready: the view died: "
            <> T.pack (displayException e)
            <> " — the rows above are not the full view"
        )
      exitFailure

-- | Open TASK offers that have no claim, owner DONE, or unresolved blocker.
-- A directed TASK is assigned at creation and is therefore not an open offer.
readyPosts :: [Stored] -> [Stored]
readyPosts posts = filter isReady [task | task <- posts, isRootTask task, null (to (stamped task))]
  where
    index = componentIndex posts
    -- Target id to the blockers that cite it, built once: scanning the
    -- whole log for a @LINK@ per task is the quadratic this hoist removes.
    blockersByTarget = Map.fromListWith (++) [(target, [blocker]) | stored <- posts, Just (blocker, target) <- [parseBlockLink (body (stamped stored))]]

    -- Root-TASK ids whose owner has posted a DONE in the task's own
    -- component: a blocker id is resolved exactly when it names such a
    -- task. Built once so resolving each blocker is one Set membership,
    -- not a whole-log scan per blocker — the quadratic free's
    -- blocker-heavy witness caught in @any@ over every post per blocker.
    resolvedTaskIds =
      Set.fromList
        [ snd (stamp task)
        | task <- posts,
          isRootTask task,
          let component = componentOf index task,
          taskComplete (taskOwner task component) component
        ]

    isReady task =
      let component = componentOf index task
          owner = taskOwner task component
          blockers = blockersFor (snd (stamp task))
       in owner == Nothing
            && not (taskComplete owner component)
            && all blockerResolved blockers

    blockersFor taskId = Map.findWithDefault [] taskId blockersByTarget

    blockerResolved blocker = Set.member blocker resolvedTaskIds

-- | Diagnose CLAIM and DONE posts that cannot settle any incomplete public
-- offer because they lie outside every such offer's thread component.
readyWarnings :: [Stored] -> [Text]
readyWarnings posts =
  [ warning stray offer
  | stray <- posts,
    hasKind "CLAIM" stray || hasKind "DONE" stray,
    outside stray,
    Just offer <- [resemblingOffer stray offers]
  ]
  where
    -- Shared across strays: recomputing the components and the offer set
    -- per stray turned a page of lint into minutes on a long log. The
    -- component index is built once here and reused for every offer lookup.
    index = componentIndex posts
    offerComponents = map (componentOf index) (taskOffers posts)
    -- Every post id held by a task-offer component, so a stray CLAIM/DONE
    -- outside all of them is one Set test instead of a per-offer scan.
    offerComponentIds = Set.fromList [snd (stamp p) | comp <- offerComponents, p <- comp]
    offers = openOffers posts

    taskOffers = filter (\task -> isRootTask task && null (to (stamped task)))

    openOffers =
      filter
        ( \task ->
            let component = componentOf index task
             in not (taskComplete (taskOwner task component) component)
        )
        . taskOffers

    outside stray = snd (stamp stray) `Set.notMember` offerComponentIds

    warning stray offer =
      let strayId = T.pack (show (snd (stamp stray)))
          offerId = T.pack (show (snd (stamp offer)))
          kind = postKind stray
       in "🟡 post "
            <> strayId
            <> " "
            <> kind
            <> " is outside every open offer component; resembles offer "
            <> offerId
            <> " — re-post from the bus root you queried: bus post --from "
            <> from (stamped stray)
            <> " --thread "
            <> offerId
            <> " --body "
            <> kind

-- | Absolute distance between post ids.  'Natural' subtraction is
-- partial: @abs (a - b)@ throws underflow when @a < b@ instead of
-- returning the distance, so the difference is taken in the larger
-- direction.
idDistance :: PostId -> PostId -> PostId
idDistance a b
  | a >= b = a - b
  | otherwise = b - a

-- | Choose the closest-looking offer without changing offer or component
-- semantics: an explicit id wins, then shared body tokens, then id distance.
resemblingOffer :: Stored -> [Stored] -> Maybe (Stored)
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
       in (Down explicitId, Down sharedTokens, idDistance strayId (snd (stamp offer)))

    scores offer =
      let offerId = snd (stamp offer)
          offerTokens = bodyTokens (body (stamped offer))
       in (T.pack (show offerId) `elem` strayTokens, length (filter (`elem` offerTokens) strayTokens))

    bodyTokens = T.words . T.toLower . T.map (\c -> if isAlphaNum c then c else ' ')

-- | The thread component holding a post, looked up by post id from a
-- partition built once by 'componentIndex'. The fallback to @[task]@ keeps
-- the prior behaviour when a post belongs to no slice.
componentOf :: Map.Map PostId [Stored] -> Stored -> [Stored]
componentOf index task =
  case Map.lookup (snd (stamp task)) index of
    Just vs -> sortOn (snd . stamp) vs
    Nothing -> [task]

-- | Post-id to thread-component index. 'slicePosts' 'ByThread' already
-- groups every post by its thread root; flattening it into a @Map@ turns
-- each READY component lookup from a whole-log re-slice into a single
-- logarithmic lookup, which is what keeps @bus ready@ linear on long logs.
componentIndex :: [Stored] -> Map.Map PostId [Stored]
componentIndex posts =
  Map.fromList [(snd (stamp p), vs) | (_, vs) <- slicePosts ByThread posts, p <- vs]

taskOwner :: Stored -> [Stored] -> Maybe Name
taskOwner task component =
  case to (stamped task) of
    [assignee] -> Just assignee
    [] -> listToMaybe [from (stamped reply) | reply <- component, hasKind "CLAIM" reply]
    _ -> Nothing

taskComplete :: Maybe Name -> [Stored] -> Bool
taskComplete owner component =
  maybe False (\name -> any (\reply -> hasKind "DONE" reply && from (stamped reply) == name) component) owner

isRootTask :: Stored -> Bool
isRootTask stored = null (thread (stamped stored)) && hasKind "TASK" stored

-- | The post's kind is its first meaningful word. A leading @@@name:@@@
-- announcement address is stripped first: seats habitually write
-- @\@fox: CLAIM · task 2@, and the kind is still the second word. Counted as
-- a kind, the addressed claim would be invisible to 'taskOwner' and to the
-- READY lint, which is the silence this strips away.
postKind :: Stored -> Text
postKind stored = case T.words (dropAddress (body (stamped stored))) of
  (w : _) -> w
  [] -> ""
  where
    dropAddress t = case T.words t of
      (w : rest) | isAddress w -> T.unwords rest
      _ -> t
    isAddress w = "@" `T.isPrefixOf` w && ":" `T.isSuffixOf` w && T.length w > 2

hasKind :: Text -> Stored -> Bool
hasKind kind stored = postKind stored == kind

parseBlockLink :: Text -> Maybe (PostId, PostId)
parseBlockLink text = case T.words text of
  ("LINK" : "·" : blocker : "blocks" : target : _) ->
    (,) <$> readMaybe (T.unpack blocker) <*> readMaybe (T.unpack target)
  _ -> Nothing

-- | The thread view: the connected component of the given id under the
-- schema's @thread@ field. Roots carry @thread = []@, replies cite their
-- ancestors, so the root of a post is the least ancestor present in the
-- log — the same walk Bus.Flow uses to slice by thread.
runThreadView :: [Stored] -> PostId -> Depth -> IO ()
runThreadView posts tid depth =
  case [vs | (_, vs) <- slicePosts ByThread posts, any ((== tid) . snd . stamp) vs] of
    (vs : _) -> traverse_ (TIO.putStrLn . renderStored depth) (sortOn (snd . stamp) vs)
    [] -> do
      TIO.putStrLn ("🔴 no post with id " <> T.pack (show tid))
      exitFailure

-- | Print a thread as an Org-mode conversation: @* [#ID]@ — the id asked
-- for, with the thread root's channel appended when one is present — then
-- one @** sender [#id] LABEL@ subheading per post, body beneath. The
-- label is the first ALL-CAPS word of the body when it is a post-type
-- token (NOTE, DONE, …); it is consumed from the body render, and a post
-- without one carries a blank label and renders its body whole.
runOrg :: FilePath -> Maybe PostId -> IO ()
runOrg root mThread = do
  root' <- resolveRoot root
  requireBus root'
  case mThread of
    Nothing -> do
      TIO.putStrLn "🔴 org requires --thread ID"
      exitFailure
    Just tid -> do
      posts <- loadPosts (root' </> "log.jsonl")
      case [vs | (_, vs) <- slicePosts ByThread posts, any ((== tid) . snd . stamp) vs] of
        (vs : _) -> TIO.putStr (renderOrgThread tid vs)
        [] -> do
          TIO.putStrLn ("🔴 no post with id " <> T.pack (show tid))
          exitFailure

renderOrgThread :: PostId -> [Stored] -> Text
renderOrgThread tid posts =
  heading
    <> "\n"
    <> T.intercalate "\n" (map renderOrgPost (sortOn (snd . stamp) posts))
    <> "\n"
  where
    heading =
      "* [#"
        <> T.pack (show tid)
        <> "]"
        <> case rootChannel of
          Just c -> " " <> c
          Nothing -> ""
    rootChannel =
      case [channel p | stored <- posts, let p = stamped stored, null (thread p)] of
        (Just c : _) -> Just c
        _ -> Nothing

renderOrgPost :: Stored -> Text
renderOrgPost stored =
  "** " <> from p <> " [#" <> T.pack (show (snd (stamp stored))) <> "]" <> label <> "\n" <> rest
  where
    p = stamped stored
    (word, bodyText) = splitLabel (body p)
    label = if T.null word then "" else " " <> word
    -- A body line that would read as an Org headline or start with a
    -- comma is escaped with a leading comma, the Org protection
    -- convention; `,* not a headline', `,, literal comma'.  fab-send
    -- unescapes, so a rendered post can be copied into a draft and the
    -- stored body round-trips exactly.
    rest = T.intercalate "\n" (map escape (T.lines bodyText))
    escape l
      | "*" `T.isPrefixOf` l = "," <> l
      | "," `T.isPrefixOf` l = "," <> l
      | otherwise = l

-- | Split a body into its post-type label and the remaining text. The
-- label is the first word of the first line when it is two or more ASCII
-- uppercase letters or digits; it is consumed, and everything else —
-- including any later lines — passes through verbatim. Any other body
-- carries a blank label and renders whole.
splitLabel :: Text -> (Text, Text)
splitLabel b =
  case T.break (== '\n') b of
    (line1, rest) ->
      case T.break (== ' ') (T.stripStart line1) of
        (w, afterW)
          | isPostType w -> (w, T.dropWhile (== ' ') afterW <> rest)
        _ -> ("", b)

-- | A post-type token: two or more ASCII uppercase letters or digits, the
-- shape of NOTE, TASK, DONE. Anything else is body text, not a label.
isPostType :: Text -> Bool
isPostType w =
  let n = T.length w
   in n >= 2 && T.all (\c -> isAsciiUpper c || isDigit c) w

-- | Render a stored post at a cascade depth: the route, the line, the
-- verbatim frame.
renderStored :: Depth -> Stored -> Text
renderStored = renderStoredWith (const "")

-- | 'renderStored' with a per-name label appended to the names of the head
-- line. The label is evidence for a reader — this name is a person, not a
-- seat — and it never reaches depth 2, which is the log's own frame.
--
-- The label annotates names the row already carries; it adds, removes and
-- reorders nothing, so a view's rows are the same rows with or without it.
renderStoredWith :: (Name -> Text) -> Depth -> Stored -> Text
renderStoredWith label (Depth d) stored
  | d <= 0 = headLine
  | d == 1 = headLine <> " " <> excerpt
  | otherwise = encodeRow stored
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
-- status
-- ---------------------------------------------------------------------------

-- | An idle seat parks: 🟡 past this many seconds of silence. Below it
-- silence still reads as readiness; past it, as storage.
idleParkedAfter :: NominalDiffTime
idleParkedAfter = 3600

pidAlive :: CPid -> IO (Maybe Bool)
pidAlive pid = do
  result <- try @IOException (signalProcess nullSignal pid)
  pure $ case result of
    Right () -> Just True
    Left e
      | isDoesNotExistError e -> Just False
      | isPermissionError e -> Just True
      | otherwise -> Nothing

data SeatCircle = CircleWorking | CircleReady | CircleParked | CircleCrashed | CircleRetired | CircleNonFab
  deriving (Eq, Ord, Show, Enum, Bounded)

circleGlyph :: SeatCircle -> Text
circleGlyph = \case
  CircleWorking -> "🟣"
  CircleReady -> "🟢"
  CircleParked -> "🟡"
  CircleCrashed -> "🔴"
  CircleRetired -> "⚫"
  CircleNonFab -> "⚪"

-- | Classify the current fab presence. White means non-fab: there is no
-- heartbeat, or a stale heartbeat has no process evidence. Explicit death is
-- retired, while a dead pid without that marker is crashed.
seatCircle :: NominalDiffTime -> UTCTime -> Maybe Heartbeat -> IO SeatCircle
seatCircle _ _ Nothing = pure CircleNonFab
seatCircle _ _ (Just (Dead, _, _)) = pure CircleRetired
seatCircle threshold now (Just (st, ts, mpid)) = do
  alive <- maybe (pure Nothing) pidAlive mpid
  let age = diffUTCTime now ts
      stale = age > threshold
      circle
        | alive == Just False = CircleCrashed
        | st == Working && alive == Just True = CircleWorking
        | (st == Wake || st == Idle) && alive == Just True =
            if age > idleParkedAfter then CircleParked else CircleReady
        | stale = CircleNonFab
        | otherwise = case st of
            Wake -> CircleReady
            Idle -> CircleReady
            Working -> CircleWorking
  pure circle

data StatusRow = StatusRow
  { statusName :: Name,
    statusCircle :: SeatCircle,
    statusUnread :: Int,
    statusCursor :: Maybe PostId,
    statusHeartbeat :: Maybe Heartbeat,
    statusLast :: Maybe (Stored)
  }

data StatusSnapshot = StatusSnapshot
  { snapshotLast :: Maybe (Stored),
    snapshotRows :: [StatusRow]
  }

-- | Read the one status model: circle membership, directed unread mail, and
-- last posts. A missing cursor has the same meaning as in 'runInbox': every
-- addressed post is unread. Reading status never advances a cursor.
readStatus :: FilePath -> NominalDiffTime -> IO StatusSnapshot
readStatus root threshold = do
  requireBus root
  posts <- loadPosts (root </> "log.jsonl")
  now <- getCurrentTime
  entries <- listDirectory root
  let cursorNames = sort [T.pack (drop (length (".cursor-" :: String)) e) | e <- entries, ".cursor-" `isPrefixOf` e]
      hbNames = sort [T.pack (drop (length (".hb-" :: String)) e) | e <- entries, ".hb-" `isPrefixOf` e]
  humans <- humanNames root
  rows <- traverse (statusRow now posts) (sortNub (hbNames ++ cursorNames ++ humans ++ participants posts))
  pure
    StatusSnapshot
      { snapshotLast = listToMaybe (sortOn (Down . snd . stamp) posts),
        snapshotRows = rows
      }
  where
    statusRow now posts n = do
      mcursor <- cursorIfExists n
      mhb <- readHeartbeat root n
      circle <- seatCircle threshold now mhb
      let mailCursor = fromMaybe 0 mcursor
          unread = length [() | s <- posts, snd (stamp s) >= mailCursor, deliversTo (stamped s) [n]]
      pure
        StatusRow
          { statusName = n,
            statusCircle = circle,
            statusUnread = unread,
            statusCursor = mcursor,
            statusHeartbeat = mhb,
            statusLast = lastFor n posts
          }

    cursorIfExists n = do
      let p = root </> (".cursor-" <> T.unpack n)
      found <- doesFileExist p
      if found then Just <$> readCursorAll root n else pure Nothing

    lastFor n posts =
      listToMaybe $
        sortOn (Down . snd . stamp) $
          [s | s <- posts, from (stamped s) == n || n `elem` to (stamped s)]

    participants posts =
      sortNub
        ( [from (stamped s) | s <- posts]
            ++ concat [[n | n <- to (stamped s), not (T.null n)] | s <- posts]
        )

renderStatus :: Depth -> Bool -> StatusSnapshot -> Text
renderStatus (Depth d) org snapshot =
  T.intercalate "\n" (censusLine : body) <> "\n"
  where
    rows = snapshotRows snapshot
    censusLine = (if org then "* " else "") <> statusHeader (snapshotLast snapshot) rows
    body
      | d <= 0 = []
      | org = concatMap (renderOrgGroup d rows) (presentCircles rows)
      | otherwise = concatMap (renderPlainRow d) rows

presentCircles :: [StatusRow] -> [SeatCircle]
presentCircles rows = [c | c <- [minBound .. maxBound], any ((== c) . statusCircle) rows]

statusHeader :: Maybe (Stored) -> [StatusRow] -> Text
statusHeader mlast rows =
  T.intercalate " " (counts <> ["| " <> lastText])
  where
    counts =
      [ circleGlyph c <> " " <> T.pack (show (length members))
      | c <- [minBound .. maxBound],
        let members = filter ((== c) . statusCircle) rows,
        not (null members)
      ]
    lastText = case mlast of
      Nothing -> "last —"
      Just s -> "last [#" <> T.pack (show (snd (stamp s))) <> "]"

renderOrgGroup :: Int -> [StatusRow] -> SeatCircle -> [Text]
renderOrgGroup d rows circle =
  ("** " <> circleGlyph circle) : concatMap (renderOrgRow d) (filter ((== circle) . statusCircle) rows)

renderOrgRow :: Int -> StatusRow -> [Text]
renderOrgRow d row =
  ("*** " <> statusLine row) : (if d >= 2 then statusEvidence row else [])

renderPlainRow :: Int -> StatusRow -> [Text]
renderPlainRow d row =
  (circleGlyph (statusCircle row) <> " " <> statusLine row) : (if d >= 2 then statusEvidence row else [])

statusLine :: StatusRow -> Text
statusLine row =
  statusName row
    <> " ("
    <> T.pack (show (statusUnread row))
    <> ") "
    <> maybe "no posts" (\s -> "post [#" <> T.pack (show (snd (stamp s))) <> "]") (statusLast row)

statusEvidence :: StatusRow -> [Text]
statusEvidence row =
  maybe [] heartbeatEvidence (statusHeartbeat row)
    <> ["    cursor: " <> maybe "—" (T.pack . show) (statusCursor row)]
    <> maybe [] (\s -> ["    log: " <> encodeRow s]) (statusLast row)
  where
    heartbeatEvidence (st, ts, mpid) =
      [ "    hb: "
          <> hbText st
          <> " "
          <> T.pack (iso8601Show ts)
          <> " pid="
          <> maybe "legacy" (T.pack . show) mpid
      ]

-- | Print the unified status snapshot. Depth 0 is the census, depth 1 adds
-- seat rows, and depth 2 adds the heartbeat, cursor, and last-post evidence.
-- Org is a renderer of this same model, not a second status projection.
runStatus :: FilePath -> NominalDiffTime -> Depth -> Bool -> IO ()
runStatus root threshold depth org = do
  snapshot <- readStatus root threshold
  TIO.putStr (renderStatus depth org snapshot)
