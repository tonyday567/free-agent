{-# LANGUAGE OverloadedStrings #-}

-- | The live seat runtime over one Backend session. The bus remains ground;
-- clients supply events and evidence, while this loop owns explicit posts,
-- transcripts, transactions, cursors, heartbeats, and control marks.
module Seat.Runtime
  ( Seat (..),
    runSeat,
  )
where

import Circuit.Parser.Framing (Stored)
import Circuit.Post (Post (..), PostId, mkPost)
import Circuit.Shared (stamp, stamped)
import Control.Exception (IOException, bracket, onException, throwIO, try)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Free.Agent.Agent (Name)
import Free.Agent.Backend
import Free.Agent.Bus (postLocal, readFileStrict, readLogStrict)
import Free.Agent.Bus.File
  ( Decision (..),
    Flow (..),
    QuiesceConfig (..),
    cursorPath,
    readCursor,
    recoverTransaction,
    tailLog,
    writeCursor,
    writeTransaction,
  )
import Free.Agent.Bus.Heartbeat (HbState (..), writeHeartbeat)
import Free.Agent.Mark (Mark (..), controlMark, isEscalate, isHalt, markGlyph)
import Free.Agent.Protocol (defaultProtocol, isSuppressedReply)
import Seat.Backend (appendBackendTranscript, runBackendTurn)
import System.Directory (doesFileExist, makeAbsolute, removeFile)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.IO (OpenMode (WriteOnly), closeFd, creat, defaultFileFlags, exclusive, openFd)
import System.Posix.Process (getProcessID)
import Text.Read (readMaybe)

data Seat = Seat
  { seatBudget :: Int,
    seatName :: Name,
    seatNames :: [Name],
    seatRoot :: FilePath,
    seatQuiesce :: Maybe QuiesceConfig,
    seatBackend :: Backend,
    seatPrompt :: [Stored] -> IO Text,
    seatAfterCompleted :: IO [Post Text Text]
  }

-- | What disk already knows about an input batch. An active marker, any
-- started-turn row, or published response makes automatic crash recovery
-- ambiguous: exactly-once cannot be reconstructed by invoking the opaque
-- client again. Failed/cancelled rows without an active marker are completed
-- terminal evidence, so a later explicit launch remains the declared retry.
data BatchFootprint = BatchFootprint
  { footprintPublished :: [PostId],
    footprintTranscript :: [(PostId, Text)],
    footprintActive :: Bool
  }

runSeat :: Seat -> IO ()
runSeat seat = do
  hSetBuffering stdout LineBuffering
  absoluteRoot <- makeAbsolute (seatRoot seat)
  setEnv "FREE_AGENT_BUS_ROOT" absoluteRoot
  bracket (openBackend (seatBackend seat)) closeSession $ \session ->
    case sessionStartupFailure session of
      Just outcome -> do
        TIO.hPutStrLn stderr $ "🔴 backend startup failed: " <> T.take 400 (T.pack (show outcome))
        ioError (userError "backend startup failed")
      Nothing -> run absoluteRoot session
  where
    run root session = do
      let agentName = seatName seat
          names = seatNames seat
          path = root </> "log.jsonl"
          transcript = root </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      pid <- getProcessID
      TIO.putStrLn $ "🟢 free-agent seat starting: " <> T.intercalate "," names <> " (pid=" <> T.pack (show pid) <> ")"
      TIO.putStrLn $ "   root: " <> T.pack root
      budget <- newIORef (max 0 (seatBudget seat))
      skips <- newIORef (0 :: Int)

      let onQuiesce decision = do
            let qc = fromMaybe (error "quiesce action without config") (seatQuiesce seat)
            _ <- postLocal root (mkPost agentName [qcPitboss qc] (decisionBody decision))
            pure ()
          stopping stored = case controlMark (stamped stored) of
            Just mark -> isHalt mark || isEscalate mark
            Nothing -> False
          controlFlow stored = do
            case controlMark (stamped stored) of
              Just mark
                | isEscalate mark ->
                    forM_ (seatQuiesce seat) $ \qc -> do
                      let relay =
                            (mkPost agentName [] (markGlyph Escalate <> " escalation received; standing down"))
                              { thread = [snd (stamp stored)],
                                to = [qcPitboss qc]
                              }
                      _ <- postLocal root relay
                      pure ()
              _ -> pure ()
            writeCursor root agentName (snd (stamp stored) + 1)
            pure Halt

          runBatch [] = pure Continue
          runBatch (stored : rest)
            | stopping stored = controlFlow stored
            | otherwise = do
                let (ordinary, remaining) = break stopping (stored : rest)
                    addressed = filter ((/= agentName) . from . stamped) ordinary
                    nextCursor = snd (stamp (last ordinary)) + 1
                if null addressed
                  then writeCursor root agentName nextCursor >> runBatch remaining
                  else
                    deliverBatch addressed nextCursor >>= \case
                      Continue -> runBatch remaining
                      Halt -> pure Halt

          deliverBatch storedBatch nextCursor = do
            prior <- readBatchFootprint root agentName (map (snd . stamp) storedBatch)
            if footprintAmbiguous prior
              then ambiguousBatch
              else attempt 1 AutomaticSession
            where
              batchIds = map (snd . stamp) storedBatch
              senders = nub [from p | p <- map stamped storedBatch, from p /= agentName]
              ids = T.intercalate "," (map (T.pack . show) batchIds)
              senderNote
                | null senders = ""
                | otherwise = "; sender " <> T.intercalate "," senders

              notice line = do
                footprint <- readBatchFootprint root agentName batchIds
                postLocal root $
                  mkPost agentName [] $
                    line
                      <> senderNote
                      <> "; "
                      <> renderFootprint footprint
                      <> "; unaddressed on purpose — answering this would spend a turn on it"

              ambiguousBatch = do
                TIO.putStrLn $ "🟡 " <> agentName <> " ambiguous recovery on posts " <> ids <> "; client not re-invoked"
                _ <- notice $ markGlyph Escalate <> " ambiguous recovery on posts " <> ids <> "; prior output or turn evidence exists, so automatic replay is refused; reconcile the batch explicitly"
                pure Halt

              attempt n sessionChoice = do
                remaining <- readIORef budget
                if remaining <= 0
                  then do
                    TIO.putStrLn $ "delivery budget exhausted for " <> agentName <> " on batch through post " <> T.pack (show (nextCursor - 1))
                    pure Halt
                  else do
                    writeIORef budget (remaining - 1)
                    prompt <- seatPrompt seat storedBatch
                    started <- getCurrentTime
                    claimed <- claimActiveBatch root agentName batchIds
                    if not claimed
                      then ambiguousBatch
                      else do
                        result <- runBackendTurn session (BackendRequest prompt sessionChoice)
                        case result of
                          Left err -> clearActive >> environmentFailed ("send refused: " <> T.pack (show err))
                          Right (_, Unavailable why) -> clearActive >> environmentFailed why
                          Right (_, outcome) -> do
                            -- Raw reply output is local evidence. An intentional
                            -- publication uses the bus-post tool path itself.
                            ended <- getCurrentTime
                            recordTranscript n outcome started ended
                            case outcome of
                              Ceiling {} -> pure ()
                              Exhausted {} -> pure ()
                              _ -> clearActive
                            case outcome of
                              Completed _ -> do
                                extra <- seatAfterCompleted seat
                                publish extra
                                writeTransaction root agentName nextCursor
                                writeCursor root agentName nextCursor
                                writeIORef skips 0
                                pure $ if any ((== Just StandDown) . controlMark) extra then Halt else Continue
                              Cancelled _ -> pure Halt
                              Stale why _
                                | sessionChoice == AutomaticSession -> do
                                    TIO.putStrLn $ "🟡 " <> agentName <> " stale session on posts " <> ids <> "; one explicit fresh attempt: " <> T.take 200 why
                                    attempt (n + 1) FreshSession
                                | otherwise -> failedBatch ("stale fresh session: " <> why)
                              Failed kind why _ -> failedBatch (outcomeDetail kind why)
                              Ceiling why _ -> terminalBatch "tool-iteration ceiling" why
                              Exhausted why _ -> terminalBatch "context exhaustion" why
                              Interrupted why _ -> interruptedBatch why

              clearActive = removeFile (activeBatchPath root agentName)

              publish posts =
                forM_ (filter keep posts) $ \post -> do
                  _ <- postLocal root post
                  pure ()
                where
                  keep = not . isSuppressedReply defaultProtocol . body

              recordTranscript n outcome started ended =
                appendBackendTranscript transcript (snd (stamp (last storedBatch))) started ended (Just n) outcome

              environmentFailed why = do
                TIO.hPutStrLn stderr $ "🔴 " <> agentName <> " backend unavailable on posts " <> ids <> ": " <> T.take 400 why
                _ <- notice $ markGlyph Escalate <> " backend unavailable on posts " <> ids <> ": " <> T.take 200 why <> "; no turn started and the cursor is unmoved"
                ioError (userError (T.unpack ("backend unavailable on posts " <> ids <> "; input left unsettled")))

              failedBatch why = do
                TIO.hPutStrLn stderr $ "🔴 " <> agentName <> " backend failed on posts " <> ids <> ": " <> T.take 400 why
                _ <- notice $ markGlyph Escalate <> " backend failed on posts " <> ids <> ": " <> T.take 200 why <> "; no automatic retry or skip; reconcile before restart"
                ioError (userError (T.unpack ("backend failed on posts " <> ids <> "; input left unsettled")))

              interruptedBatch why = do
                TIO.putStrLn $ "🔴 " <> agentName <> " interrupted on posts " <> ids <> "; input left unsettled: " <> T.take 400 why
                _ <- notice $ markGlyph Escalate <> " interrupted on posts " <> ids <> ": " <> T.take 200 why <> "; no automatic replay and the cursor is unmoved"
                pure Halt

              terminalBatch kind why = do
                TIO.putStrLn $ "🔴 " <> agentName <> " " <> kind <> " on posts " <> ids <> "; committed without replay: " <> T.take 400 why
                _ <- notice $ markGlyph Escalate <> " " <> kind <> " on posts " <> ids <> ": " <> T.take 200 why <> "; batch closed as a terminal decision without replay"
                commitSkip

              commitSkip = do
                writeTransaction root agentName nextCursor
                writeCursor root agentName nextCursor
                clearActive
                seen <- readIORef skips
                writeIORef skips (seen + 1)
                pure $ if seen + 1 >= 3 then Halt else Continue

      recoverTransaction root agentName
      cursor <- readCursor root agentName
      TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
      writeHeartbeat root agentName Wake
      let follow =
            tailLog path names cursor (fmap (\qc -> (qc, onQuiesce)) (seatQuiesce seat)) $ \storedBatch -> do
              writeHeartbeat root agentName Working
              result <- runBatch storedBatch
              writeHeartbeat root agentName Idle
              pure result
      follow `onException` writeHeartbeat root agentName Dead
      writeHeartbeat root agentName Dead

outcomeDetail :: FailureKind -> Text -> Text
outcomeDetail kind why = T.pack (show kind) <> ": " <> why

-- Published output and a settled row make a crash ambiguous. Failed and
-- cancelled rows do not: a later explicit launch is the declared retry for an
-- interrupted or failed turn whose reply was never published.
footprintAmbiguous :: BatchFootprint -> Bool
footprintAmbiguous footprint =
  footprintActive footprint
    || not (null (footprintPublished footprint))
    || any ((== "settled") . snd) (footprintTranscript footprint)

renderFootprint :: BatchFootprint -> Text
renderFootprint footprint =
  "evidence published "
    <> published
    <> "; transcript "
    <> rows
    <> "; active "
    <> if footprintActive footprint then "yes" else "no"
  where
    published = case footprintPublished footprint of
      [] -> "none"
      ids -> T.intercalate "," (map (T.pack . show) ids)
    rows = case footprintTranscript footprint of
      [] -> "none"
      values ->
        T.pack (show (length values))
          <> " rows ("
          <> T.intercalate "," [T.pack (show pid) <> ":" <> outcome | (pid, outcome) <- values]
          <> ")"

-- | Claim one same-name active batch atomically. A competing process that
-- loses exclusive creation must take the ambiguity path; it never waits for
-- the winner to clear the marker and then invokes the input itself.
claimActiveBatch :: FilePath -> Name -> [PostId] -> IO Bool
claimActiveBatch root agentName batchIds = do
  let path = activeBatchPath root agentName
  created <-
    try
      ( bracket
          (openFd path WriteOnly defaultFileFlags {creat = Just 0o600, exclusive = True})
          closeFd
          (\_ -> pure ())
      ) ::
      IO (Either IOException ())
  case created of
    Right () -> TIO.writeFile path (T.pack (show batchIds)) >> pure True
    Left err
      | isAlreadyExistsError err -> pure False
      | otherwise -> throwIO err

activeBatchPath :: FilePath -> Name -> FilePath
activeBatchPath root agentName = root </> (".active-" <> T.unpack agentName)

readBatchFootprint :: FilePath -> Name -> [PostId] -> IO BatchFootprint
readBatchFootprint root agentName ids = do
  published <- readPublished
  transcript <- readTranscript
  BatchFootprint published transcript <$> readActive
  where
    readActive = do
      let path = activeBatchPath root agentName
      exists <- doesFileExist path
      if not exists
        then pure False
        else do
          content <- readFileStrict path
          pure $ case (readMaybe (T.unpack content) :: Maybe [PostId]) of
            Just activeIds -> any (`elem` ids) activeIds
            Nothing -> True
    readPublished = do
      let path = root </> "log.jsonl"
      exists <- doesFileExist path
      if not exists
        then pure []
        else do
          posts <- readLogStrict path
          pure
            [ snd (stamp stored)
            | stored <- posts,
              let post = stamped stored,
              from post == agentName,
              any (`elem` ids) (thread post)
            ]
    readTranscript = do
      let path = root </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      exists <- doesFileExist path
      if not exists
        then pure []
        else do
          content <- readFileStrict path
          pure
            [ (pid, outcome)
            | line <- T.lines content,
              Just pid <- [rowPost line],
              pid `elem` ids,
              Just outcome <- [rowOutcome line]
            ]
    rowPost line =
      readMaybe . T.unpack . T.takeWhile (/= ',')
        =<< T.stripPrefix "{\"post_id\":" line
    rowOutcome line = case T.breakOn "\"outcome\":\"" line of
      (_, rest)
        | T.null rest -> Nothing
        | otherwise -> Just (T.takeWhile (/= '"') (T.drop (T.length "\"outcome\":\"") rest))
