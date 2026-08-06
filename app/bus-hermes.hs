{-# LANGUAGE OverloadedStrings #-}

-- | Hermes-backed agent runner for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), forwards each body to
-- Hermes with the given system prompt, and writes the cleaned reply lines back
-- through the scribe with proper thread edges.
--
-- Usage:
--   free-agent-bus-hermes ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]
--
-- Example:
--   free-agent-bus-hermes ./bus hermes-bot prompt.md ./bus/hermes-bot.sid
module Main (main) where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), PostId, deliversTo, sortNub)
import Circuit.Agent.Framing (StoredPost, framePost, parseLine, stampId, stamped)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forever, guard, unless, when)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Host (hermesHost)
import Free.Agent.Seat (FreeSeat, hostSeat, interpretSeat)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Text.Read (readMaybe)
import System.FSNotify
  ( Event (..),
    WatchManager,
    watchDir,
    withManager,
  )
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
import System.Process (readProcess)

-- | Parse arguments of the form:
--   ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]
parseArgs :: [String] -> Maybe (FilePath, [Name], FilePath, FilePath)
parseArgs [] = Nothing
parseArgs (root : rest) = do
  let (names, promptAndSess) = break (".md" `isSuffixOf`) rest
  promptFile <- listToMaybe promptAndSess
  let sessCandidates = drop 1 promptAndSess
  let sessionFile =
        case sessCandidates of
          [s] -> s
          _ -> root </> ".sessions" </> agentNameOf names <> ".sid"
  guard (not (null names))
  pure (root, map T.pack names, promptFile, sessionFile)
  where
    agentNameOf [] = "agent"
    agentNameOf (n : _) = n

usage :: IO ()
usage = do
  TIO.putStrLn "Usage: free-agent-bus-hermes ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]"
  TIO.putStrLn ""
  TIO.putStrLn "  ROOT            directory containing log.jsonl"
  TIO.putStrLn "  NAME            agent name(s) to subscribe to"
  TIO.putStrLn "  PROMPT.md       system prompt markdown file"
  TIO.putStrLn "  SESSION-FILE    optional Hermes session file (default: ROOT/.sessions/NAME.sid)"

-- | Locate the scribe executable. Prefer the sibling in the cabal build tree,
-- then PATH.
findScribe :: IO FilePath
findScribe = do
  self <- getExecutablePath
  let buildTree =
        takeDirectory self
          </> ".."
          </> ".."
          </> ".."
          </> "free-agent-bus"
          </> "build"
          </> "free-agent-bus"
          </> "free-agent-bus"
  candidates <- filterM doesFileExist [buildTree, takeDirectory self </> "free-agent-bus"]
  case candidates of
    (p : _) -> pure p
    [] -> do
      mPath <- findExecutable "free-agent-bus"
      case mPath of
        Just p -> pure p
        Nothing -> do
          TIO.putStrLn "🔴 free-agent-bus scribe executable not found"
          exitFailure

-- | Scribe one post by invoking the external scribe executable.
scribePost :: FilePath -> FilePath -> Post Text -> IO ()
scribePost scribe root p = do
  _ <- readProcess scribe [root] (T.unpack (framePost p))
  pure ()

-- | Drop Hermes CLI noise lines that can precede the actual reply text,
-- so routing and empty-reply filtering work on the real model output.
scrubReply :: Post Text -> Post Text
scrubReply p =
  let ls = T.lines (body p)
      clean = filter (not . noise) ls
      noise l =
        T.null l
          || "↪" `T.isPrefixOf` l
          || "session_id:" `T.isPrefixOf` l
          || "Warning:" `T.isPrefixOf` l
          || "Resumed session" `T.isInfixOf` l
          || "Resume this session with:" `T.isInfixOf` l
          || "⚕" `T.isPrefixOf` l
          || "❯" `T.isPrefixOf` l
   in p {body = T.strip (T.unlines clean)}

-- | Parse a leading @name: prefix from a reply body. When present, redirect
-- the post to that name and strip the prefix. This lets the LLM route a reply
-- to another bus participant instead of always replying to the sender.
routeReply :: Post Text -> Post Text
routeReply p =
  case T.stripPrefix "@" (body p) of
    Nothing -> p
    Just rest ->
      let (name, afterName) = T.break (== ':') rest
          name' = T.strip name
       in if T.null name' || T.null afterName
            then p
            else p {to = [name'], body = T.strip (T.drop 1 afterName)}

-- | Decorate an incoming post body with its sender so the LLM can tell who is
-- speaking on the bus. The original stamped post is untouched; only the copy
-- fed to the seat is decorated.
decorateSender :: StoredPost -> StoredPost
decorateSender stored =
  let p = stamped stored
   in stored {stamped = p {body = from p <> ": " <> body p}}

-- | Run one stored post through the seat and produce reply posts with thread
-- edges citing the parent id.
runOne :: FreeSeat -> StoredPost -> IO [Post Text]
runOne seat stored = do
  let stored' = decorateSender stored
      p = stamped stored'
      parentId = stampId stored
      sh = interpretSeat seat
  (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
  pure [routeReply (scrubReply out) {thread = sortNub (parentId : thread out)} | out <- outs]

-- | Path to the cursor file for an agent. The cursor stores the next
-- `postId` the agent should process, so restarts catch up without re-reading
-- the whole log. A missing cursor defaults to 0, meaning "start from the
-- first post".
cursorPath :: FilePath -> Name -> FilePath
cursorPath root name = root </> (".cursor-" <> T.unpack name)

-- | Read the cursor for an agent. Returns 0 if no cursor exists yet.
readCursor :: FilePath -> Name -> IO PostId
readCursor root name = do
  let path = cursorPath root name
  exists <- doesFileExist path
  if not exists
    then pure 0
    else do
      txt <- TIO.readFile path
      pure $ maybe 0 fromIntegral (readMaybe @Integer (T.unpack (T.strip txt)))

-- | Persist the cursor for an agent. Writes @stampId + 1@ so the next wake
-- starts after the post just processed.
writeCursor :: FilePath -> Name -> PostId -> IO ()
writeCursor root name pid =
  TIO.writeFile (cursorPath root name) (T.pack (show pid))

-- | Event-tail a log file and invoke the callback for every new stored post
-- addressed to any of the subscribed names.
--
-- On startup the file is scanned from the beginning; posts with 'stampId'
-- greater than or equal to the supplied cursor and addressed to any subscribed
-- name are delivered. After catch-up the handle is parked at EOF and fsnotify
-- wakes it for new lines.
tailLog :: FilePath -> [Name] -> PostId -> (StoredPost -> IO ()) -> IO ()
tailLog path names startCursor cb = do
  exists <- doesFileExist path
  unless exists $ do
    -- Touch an empty log so the scribe has a file to append to.
    withFile path AppendMode (\_ -> pure ())
  h <- openFile path ReadMode
  hSetBuffering h LineBuffering
  -- Catch up on any posts missed while this agent was offline.
  catchUp h startCursor
  -- Park at EOF and wait for new posts via fsnotify.
  size <- hFileSize h
  hSeek h AbsoluteSeek size
  let logName = takeFileName path
      dir = takeDirectory path
  withManager $ \mgr -> do
    _ <- watchDir mgr dir (\ev -> takeFileName (eventPath ev) == logName) $ \_ev -> do
      drain h names cb
    -- Block forever; the watch listener runs in the background.
    forever (threadDelay 1000000)
  where
    catchUp h cursor = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ cb (filterStoredSince names cursor line)
        catchUp h cursor

    drain h names' cb' = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        traverse_ cb' (filterStored names' line)
        drain h names' cb'

    filterStoredSince names' cursor line = do
      stored <- parseLine line
      guard (stampId stored >= cursor)
      guard (deliversTo (stamped stored) names')
      pure stored

    filterStored :: [Name] -> Text -> Maybe StoredPost
    filterStored names' line = do
      stored <- parseLine line
      if deliversTo (stamped stored) names' then Just stored else Nothing

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Nothing -> do
      usage
      exitFailure
    Just (root, names, promptFile, sessionFile) -> do
      when (null names) $ do
        TIO.putStrLn "🔴 at least one NAME is required"
        exitFailure
      systemPrompt <- TIO.readFile promptFile
      createDirectoryIfMissing True (takeDirectory sessionFile)
      let agentName = case names of (n : _) -> n; [] -> "agent"
          host = hermesHost agentName systemPrompt sessionFile
          seat = hostSeat host
      scribe <- findScribe
      TIO.putStrLn $ "🟢 bus hermes agent starting: " <> T.intercalate "," names
      TIO.putStrLn $ "   root: " <> T.pack root
      TIO.putStrLn $ "   prompt: " <> T.pack promptFile
      TIO.putStrLn $ "   session: " <> T.pack sessionFile
      TIO.putStrLn $ "   scribe: " <> T.pack scribe
      let path = root </> "log.jsonl"
          -- Drop empty replies and Hermes "no reply" error placeholders.
          keepReply p =
            let b = T.strip (body p)
             in not (T.null b) && not ("⚠️" `T.isPrefixOf` b)
      cursor <- readCursor root agentName
      TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
      tailLog path names cursor $ \stored -> do
        replies <- filter keepReply <$> runOne seat stored
        traverse_ (scribePost scribe root) replies
        writeCursor root agentName (stampId stored + 1)
