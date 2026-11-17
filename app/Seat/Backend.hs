{-# LANGUAGE OverloadedStrings #-}

-- | Backend acts at the seat boundary: drain events, retain transcript
-- evidence, and construct explicit reply posts with input ancestry.
module Seat.Backend
  ( runBackendTurn,
    drainBackend,
    appendBackendTranscript,
    replyActs,
  )
where

import Circuit.Parser.Framing (Stored)
import Circuit.Post (Post, PostId)
import Circuit.Shared (stamp, stamped)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, diffUTCTime)
import Free.Agent.Agent (Name)
import Free.Agent.Backend
import Free.Agent.Cli (TranscriptOutcome (..), TranscriptRecord (..), encodeTranscriptLine)
import Free.Agent.Query (replyPosts)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Send one request and drain its outputs through the single terminal event.
runBackendTurn :: Session -> BackendRequest -> IO (Either SendError ([BackendOutput], BackendOutcome))
runBackendTurn session request = do
  accepted <- sendSession session request
  case accepted of
    Left err -> pure (Left err)
    Right () -> Right <$> drainBackend session

-- | Read output events until 'Settled'. Output is never treated as an outcome.
drainBackend :: Session -> IO ([BackendOutput], BackendOutcome)
drainBackend session = go []
  where
    go outputs = do
      event <- nextEvent session
      case event of
        Output output -> go (output : outputs)
        Settled outcome -> pure (reverse outputs, outcome)

-- | Append one backend-neutral transcript row. Evidence absent from the
-- backend stays absent; only the historical integer exit field needs a
-- fallback for protocols that have no process exit code.
appendBackendTranscript :: FilePath -> PostId -> UTCTime -> UTCTime -> BackendOutcome -> IO ()
appendBackendTranscript path postId started ended outcome = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.appendFile path (encodeTranscriptLine record <> "\n")
  where
    evidence = case outcome of
      Completed value -> Just value
      Cancelled value -> Just value
      Failed _ _ value -> Just value
      Stale _ value -> Just value
      Exhausted _ value -> Just value
      Interrupted _ value -> Just value
      Ceiling _ value -> Just value
      Unavailable _ -> Nothing
    transcriptOutcome = case outcome of
      Completed _ -> TranscriptSettled
      Cancelled _ -> TranscriptCancelled
      _ -> TranscriptFailed
    fallbackExit = case outcome of
      Completed _ -> 0
      Cancelled _ -> 130
      _ -> 1
    record =
      TranscriptRecord
        { trPostId = fromIntegral postId,
          trTimestamp = started,
          trExitCode = fromMaybe fallbackExit (evidence >>= evidenceExitCode),
          trElapsedMs = floor (realToFrac (diffUTCTime ended started) * (1000 :: Double)),
          trOutcome = transcriptOutcome,
          trAttempt = Nothing,
          trSessionId = evidence >>= evidenceSessionId,
          trRaw = maybe "" evidenceRawOutput evidence
        }

-- | Each reply-channel output is one explicit post act. Thoughts, deltas,
-- usage, stdout, stderr, and raw evidence remain off the bus. Reply audience
-- and ancestry come from the committed input, never from response prose.
replyActs :: Name -> [Stored] -> [BackendOutput] -> [Post Text Text]
replyActs who inputs outputs =
  concatMap (replyPosts who (map stamped inputs) (map (snd . stamp) inputs)) replies
  where
    replies = [text | BackendOutput ReplyOutput text <- outputs]
