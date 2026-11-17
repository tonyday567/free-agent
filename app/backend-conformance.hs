{-# LANGUAGE OverloadedStrings #-}

-- | Two clients, one scripted Backend/seat conformance run. No provider is
-- called: local fake hermes and dsh executables supply their wire formats.
module Main (main) where

import Circuit.Parser.Framing (Stored)
import Circuit.Post (Post (..), PostId, mkPost)
import Circuit.Shared (stamp, stamped)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Free.Agent.Backend
import Free.Agent.Bus (postLocal, readLogStrict)
import Free.Agent.Bus.File (readCursor, readTransaction, writeCursor, writeTransaction)
import Options.Applicative
import Seat.Backend (appendBackendTranscript, drainBackend, replyActs, runBackendTurn)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (getEnv, setEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)

data Client = Hermes | Dsh | Both
  deriving (Eq, Show)

data ClientReport = ClientReport
  { reportActs :: [Text],
    reportOutputChannels :: [[OutputChannel]],
    reportThreads :: [[PostId]],
    reportTranscriptRows :: Int,
    reportCursor :: PostId
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  client <- execParser opts
  base <- scratchBase
  case client of
    Both -> do
      hermes <- runClient Hermes (base </> "hermes")
      dsh <- runClient Dsh (base </> "dsh")
      require "compare identical normalized acts, outputs, ancestry, transcripts, and cursor" (hermes == dsh)
      TIO.putStrLn ("artifact: " <> T.pack base)
    other -> do
      _ <- runClient other (base </> clientName other)
      TIO.putStrLn ("artifact: " <> T.pack base)
  where
    opts =
      info
        (option (eitherReader parseClient) (long "backend" <> metavar "hermes|dsh|both" <> value Both <> showDefault) <**> helper)
        (fullDesc <> progDesc "Run identical Backend client conformance scripts on one scratch root")
    parseClient "hermes" = Right Hermes
    parseClient "dsh" = Right Dsh
    parseClient "both" = Right Both
    parseClient other = Left ("unknown backend: " <> other)

clientName :: Client -> String
clientName Hermes = "hermes"
clientName Dsh = "dsh"
clientName Both = "both"

scratchBase :: IO FilePath
scratchBase = do
  home <- getHomeDirectory
  pid <- getProcessID
  let root = home </> ".hermes" </> "cache" </> "scratch" </> ("free-agent-backend-conformance-" <> show pid)
  createDirectoryIfMissing True root
  pure root

require :: Text -> Bool -> IO ()
require label ok = do
  TIO.putStrLn ((if ok then "PASS " else "FAIL ") <> label)
  unless ok exitFailure

runClient :: Client -> FilePath -> IO ClientReport
runClient client root = do
  createDirectoryIfMissing True root
  writeFakeClient client root
  oldPath <- getEnv "PATH"
  bracket (pure oldPath) (setEnv "PATH") $ \_ -> do
    setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
    execute client root

execute :: Client -> FilePath -> IO ClientReport
execute client root = do
  let name = "seat"
      transcript = root </> ".transcripts" </> "seat.jsonl"
      sessionFile = root </> ".sessions" </> "seat.sid"
      backend = case client of
        Hermes -> hermesBackend Nothing Nothing Nothing False sessionFile
        Dsh -> dshBackend (Just "dsh system") (root </> "base.yml") Nothing sessionFile
        Both -> error "comparison has no single backend"
  writeCursor root name 0
  createDirectoryIfMissing True (takeDirectory sessionFile)
  session <- openBackend backend
  let caps = sessionCapabilities session
  require "open exposes the selected client and declared capabilities" $
    backendKind caps == (if client == Hermes then HermesBackend else DshBackend)
      && backendSessions caps == FileSessions
      && backendDelivery caps == OneShot

  firstCause <- postLocal root (mkPost "runner" [name] "send-turn")
  (firstOutputs, firstOutcome) <-
    completedTurn root transcript session name firstCause (defaultRequest "send-turn")
  require "send settles once with declared session and process evidence" $
    completedEvidence caps firstOutcome

  secondCause <- postLocal root (mkPost "runner" [name] "resume-turn")
  (secondOutputs, secondOutcome) <-
    completedTurn root transcript session name secondCause (defaultRequest "resume-turn")
  require "resume reuses the stored session through AutomaticSession" $
    completedEvidence caps secondOutcome

  cancelCause <- postLocal root (mkPost "runner" [name] "cancel-turn")
  let cancelMark = root </> "cancel-started"
  setEnv "FREE_AGENT_CONFORMANCE_CANCEL_MARK" cancelMark
  started <- getCurrentTime
  accepted <- sendSession session (BackendRequest "cancel-turn" FreshSession)
  require "cancel act starts its own input" (accepted == Right ())
  waitForFile cancelMark
  won <- cancelSession session
  (_, cancelOutcome) <- drainBackend session
  ended <- getCurrentTime
  appendBackendTranscript transcript (snd (stamp cancelCause)) started ended Nothing cancelOutcome
  require "cancel follows the declared cancellation capability" $
    if backendCancellation caps
      then won && isCancelled cancelOutcome
      else not won && not (isCancelled cancelOutcome)
  require "cancel does not publish or advance the cursor" =<< do
    cursor <- readCursor root name
    pure (cursor == snd (stamp secondCause) + 1)
  closeSession session

  posts <- readLogStrict (root </> "log.jsonl")
  let replies = [stamped stored | stored <- posts, from (stamped stored) == name]
      threads = map thread replies
  require "each reply output becomes one sender-directed post with input ancestry" $
    length replies == 2
      && map body replies == ["conformance-one", "conformance-two"]
      && all ((== ["runner"]) . to) replies
      && threads == [[snd (stamp firstCause)], [snd (stamp secondCause)]]
  transaction <- readTransaction root name
  cursor <- readCursor root name
  require "two completed turns commit twice; the final cursor is exact" $
    transaction == Just (snd (stamp secondCause) + 1) && cursor == snd (stamp secondCause) + 1
  transcriptText <- TIO.readFile transcript
  let rows = filter (not . T.null) (T.lines transcriptText)
  require "open/send/resume/cancel leave three same-shaped transcript records" $
    length rows == 3 && all validTranscriptRow rows
  pure
    ClientReport
      { reportActs = ["open", "send", "resume", "cancel"],
        reportOutputChannels = [map outputChannel firstOutputs, map outputChannel secondOutputs],
        reportThreads = threads,
        reportTranscriptRows = length rows,
        reportCursor = cursor
      }

completedTurn :: FilePath -> FilePath -> Session -> Text -> Stored -> BackendRequest -> IO ([BackendOutput], BackendOutcome)
completedTurn root transcript session name cause request = do
  started <- getCurrentTime
  result <- runBackendTurn session request
  case result of
    Left err -> do
      require "backend accepts the scripted input" False
      error ("send refused: " <> show err)
    Right (outputs, outcome) -> do
      ended <- getCurrentTime
      appendBackendTranscript transcript (snd (stamp cause)) started ended Nothing outcome
      case outcome of
        Completed _ -> do
          let replies = replyActs name [cause] outputs
          forM_ replies (postLocal root)
          let next = snd (stamp cause) + 1
          writeTransaction root name next
          writeCursor root name next
          pure (outputs, outcome)
        _ -> do
          require "scripted input completes" False
          error "scripted input did not complete"

completedEvidence :: BackendCapabilities -> BackendOutcome -> Bool
completedEvidence caps (Completed evidence) =
  evidenceSessionId evidence == Just "conformance-session"
    && case backendSessions caps of
      FileSessions -> isJust (evidenceExitCode evidence)
      ServerSessions -> evidenceExitCode evidence == Nothing
      NoSessions -> False
completedEvidence _ _ = False

isCancelled :: BackendOutcome -> Bool
isCancelled (Cancelled _) = True
isCancelled _ = False

validTranscriptRow :: Text -> Bool
validTranscriptRow row =
  "\"post_id\":" `T.isInfixOf` row
    && "\"outcome\":" `T.isInfixOf` row
    && "\"session_id\":" `T.isInfixOf` row
    && "\"raw\":" `T.isInfixOf` row

waitForFile :: FilePath -> IO ()
waitForFile path = go (100 :: Int)
  where
    go 0 = do
      require "cancellable fake client started" False
      error "cancellable fake client did not start"
    go remaining = do
      exists <- doesFileExist path
      unless exists (threadDelay 50_000 >> go (remaining - 1))

writeFakeClient :: Client -> FilePath -> IO ()
writeFakeClient client root = do
  let bin = root </> "bin"
      path = bin </> clientName client
  createDirectoryIfMissing True bin
  TIO.writeFile path (fakeSource client)
  setFileMode path 0o755

fakeSource :: Client -> Text
fakeSource client =
  T.pack $
    unlines
      [ "#!/bin/sh",
        "input=\"$*\"",
        "case \"$input\" in",
        "  *cancel-turn*)",
        "    touch \"$FREE_AGENT_CONFORMANCE_CANCEL_MARK\"",
        "    trap 'exit 0' TERM",
        "    while :; do sleep 1; done ;;",
        "esac",
        ( case client of
            Hermes -> hermesSuccess
            Dsh -> dshSuccess
            Both -> "exit 2"
        )
      ]
  where
    hermesSuccess =
      unlines
        [ "usage=\"\"",
          "prev=\"\"",
          "for a in \"$@\"; do",
          "  if [ \"$prev\" = \"--usage-file\" ]; then usage=\"$a\"; fi",
          "  prev=\"$a\"",
          "done",
          "if [ -n \"$usage\" ]; then printf '{\"session_id\":\"conformance-session\",\"completed\":true}\\n' > \"$usage\"; fi",
          "case \"$input\" in *resume-turn*) printf 'conformance-two\\n' ;; *) printf 'conformance-one\\n' ;; esac"
        ]
    dshSuccess =
      unlines
        [ "printf '%s\\n' '{\"type\":\"session\",\"sessionId\":\"conformance-session\"}'",
          "case \"$input\" in *resume-turn*) text=conformance-two ;; *) text=conformance-one ;; esac",
          "printf '{\"type\":\"final\",\"text\":\"%s\"}\\n' \"$text\""
        ]
