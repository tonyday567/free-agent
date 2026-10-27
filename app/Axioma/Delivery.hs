{-# LANGUAGE OverloadedStrings #-}

-- | Delivery algebra: quiet settlement, exactly-once response, and explicit
-- publication as either a root post or a reply.
module Axioma.Delivery
  ( deliveryTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (Post (..), replyTo)
import Circuit.Agent.Framing (stamp, stamped, unframeStored)
import Control.Concurrent (threadDelay)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliDeliverTo, parseSessionId)
import Free.Agent.Delivery
  ( DeliveryRefusal (..),
    Invocation (..),
    TurnMetrics (..),
    TurnResult (..),
    awaitTurn,
    cancelTurn,
    deliverTo,
    directPostTo,
    newDeliveryGrant,
    postTo,
    turnMetrics,
  )
import System.Directory (doesFileExist, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Timeout (timeout)

deliveryTopic :: IO ()
deliveryTopic = do
  putStrLn "delivery settlement and publication"
  root <- tempRoot "free-agent-delivery"
  let script = root </> "fake-cli.sh"
      calls = root </> "calls"
      sessionFile = root </> "session"
      transcriptLog = root </> "transcripts" </> "seat.jsonl"
  TIO.writeFile script $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "printf 'call\\n' >> \"$1\"",
          "echo 'session_id: delivery-session'",
          "printf 'response:'",
          "cat"
        ]
  let cli =
        Cli
          { cliCommand = "/bin/sh",
            cliArgv = \_ _ -> [script, calls],
            cliStdin = T.unpack,
            cliSessionFile = sessionFile,
            cliSessionId = parseSessionId,
            cliStale = \code _ -> code /= ExitSuccess,
            cliScrub = cleanCliOut,
            cliStderr = StderrDrop,
            cliStderrTee = Nothing
          }
  causing <- postTo root (mkPost "human" ["seat"] "quiet turn")
  invokedByPost <- doesFileExist calls
  assert "postTo publishes without invoking a backend" (not invokedByPost)

  let postId = snd (stamp causing)
  quietTurn <- cliDeliverTo cli transcriptLog postId (body (stamped causing))
  quietResult1 <- awaitTurn quietTurn
  quietResult2 <- awaitTurn quietTurn
  let response1 = case quietResult1 of
        TurnResponse response -> response
        TurnCancelled -> ""
  assert "a delivered response is ordinary scrubbed text" $
    quietResult1 == TurnResponse "response:quiet turn" && quietResult2 == quietResult1
  callLog <- TIO.readFile calls
  assert "repeated awaits settle on one backend invocation" $
    length (T.lines callLog) == 1

  quietLog <- TIO.readFile (root </> "log.jsonl")
  let quietPosts = mapMaybe (unframeStored @Text) (T.lines quietLog)
  assert "a settled response publishes zero posts by itself" $
    length quietPosts == 1
  transcript <- TIO.readFile transcriptLog
  let records = filter (not . T.null) (T.lines transcript)
  assert "quiet settlement leaves one existing transcript record" $
    length records == 1
  assert "the transcript carries causation, settled outcome, and raw response" $
    case records of
      [record] ->
        "\"post_id\":0" `T.isInfixOf` record
          && "\"outcome\":\"settled\"" `T.isInfixOf` record
          && "response:quiet turn" `T.isInfixOf` record
      _ -> False

  unrelated <- postTo root (mkPost "seat" ["archive"] response1)
  assert "explicit publication may be an unrelated root post" $
    to (stamped unrelated) == ["archive"] && thread (stamped unrelated) == []
  replied <- postTo root (replyTo "seat" postId (stamped causing) response1)
  assert "explicit reply publication preserves audience and ancestry" $
    to (stamped replied) == ["human"] && thread (stamped replied) == [postId]
  finalLog <- TIO.readFile (root </> "log.jsonl")
  let finalPosts = mapMaybe (unframeStored @Text) (T.lines finalLog)
  assert "the public tape contains only the cause and two explicit post acts" $
    length finalPosts == 3
  finalCalls <- TIO.readFile calls
  assert "neither publication shape invokes another turn" $
    length (T.lines finalCalls) == 1
  removePathForcibly root

  putStrLn "delivery cancellation and live metrics"
  cancelRoot <- tempRoot "free-agent-delivery-cancel"
  let cancelScript = cancelRoot </> "blocking-cli.sh"
      started = cancelRoot </> "started"
      cancelTranscript = cancelRoot </> "transcripts" </> "seat.jsonl"
  TIO.writeFile cancelScript $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "printf 'started\\n' > \"$1\"",
          "echo 'session_id: cancel-session'",
          "printf 'partial-response'",
          "exec sleep 30"
        ]
  let cancelCli =
        Cli
          { cliCommand = "/bin/sh",
            cliArgv = \_ _ -> [cancelScript, started],
            cliStdin = const "",
            cliSessionFile = cancelRoot </> "session",
            cliSessionId = parseSessionId,
            cliStale = \code _ -> code /= ExitSuccess,
            cliScrub = cleanCliOut,
            cliStderr = StderrMerge,
            cliStderrTee = Nothing
          }
  cancelCause <- postTo cancelRoot (mkPost "human" ["seat"] "interrupt")
  interrupted <- cliDeliverTo cancelCli cancelTranscript (snd (stamp cancelCause)) "interrupt"
  let waitForStart attempts = do
        exists <- doesFileExist started
        if exists || attempts <= (0 :: Int)
          then pure exists
          else threadDelay 10_000 >> waitForStart (attempts - 1)
  startedOk <- waitForStart 100
  assert "the blocking child started before cancellation" startedOk
  liveMetrics <- turnMetrics interrupted
  assert "live metrics expose an in-flight turn and elapsed cost" $
    tmInFlight liveMetrics && not (tmCancelled liveMetrics) && tmElapsedMs liveMetrics >= 0
  cancelledPromptly <- timeout 2_000_000 (cancelTurn interrupted)
  assert "cancellation stops the child and retains evidence promptly" $
    isJust cancelledPromptly
  cancelledResult1 <- awaitTurn interrupted
  cancelledResult2 <- awaitTurn interrupted
  assert "cancelled turns settle explicitly and exactly once" $
    cancelledResult1 == TurnCancelled && cancelledResult2 == TurnCancelled
  finalMetrics <- turnMetrics interrupted
  assert "final metrics expose cancellation and no in-flight turn" $
    not (tmInFlight finalMetrics) && tmCancelled finalMetrics
  cancelTurn interrupted
  cancelEvidence <- TIO.readFile cancelTranscript
  let cancelRecords = filter (not . T.null) (T.lines cancelEvidence)
  assert "cancellation and repeated cancellation leave one transcript record" $
    length cancelRecords == 1
  assert "cancelled transcript retains outcome, cause, session, and partial raw output" $
    case cancelRecords of
      [record] ->
        "\"outcome\":\"cancelled\"" `T.isInfixOf` record
          && "\"post_id\":0" `T.isInfixOf` record
          && "\"session_id\":\"cancel-session\"" `T.isInfixOf` record
          && "partial-response" `T.isInfixOf` record
      _ -> False
  cancelLog <- TIO.readFile (cancelRoot </> "log.jsonl")
  let cancelPosts = mapMaybe (unframeStored @Text) (T.lines cancelLog)
  assert "a cancelled response publishes nothing and starts no retry" $
    length cancelPosts == 1 && length cancelRecords == 1
  removePathForcibly cancelRoot

  putStrLn "bounded direct delivery"
  grantRoot <- tempRoot "free-agent-delivery-grant"
  invocations <- newIORef (0 :: Int)
  let deliver stored =
        deliverTo
          ( \cause input ->
              pure
                Invocation
                  { runInvocation = do
                      modifyIORef' invocations (+ 1)
                      pure (TurnResponse (body (stamped cause) <> ":" <> input)),
                    cancelInvocation = pure False
                  }
          )
          stored
          (body (stamped stored))
  grant <- newDeliveryGrant 1
  (grantedPost, granted) <- directPostTo grant deliver grantRoot (mkPost "human" ["seat"] "first")
  case granted of
    Left _ -> assert "a one-unit grant admits the first direct post" False
    Right grantedTurn -> do
      grantedResult <- awaitTurn grantedTurn
      assert "a granted direct post causes exactly one delivery" $
        grantedResult == TurnResponse "first:first" && snd (stamp grantedPost) == 0
  (refusedPost, refused) <- directPostTo grant deliver grantRoot (mkPost "human" ["seat"] "second")
  case refused of
    Right _ -> assert "an exhausted grant refuses the second delivery" False
    Left DeliveryBudgetExhausted ->
      assert "an exhausted grant returns an inspectable refusal with the post receipt" $
        snd (stamp refusedPost) == 1
  invocationCount <- readIORef invocations
  assert "the refused direct post does not reach the backend" $
    invocationCount == 1
  removePathForcibly grantRoot
