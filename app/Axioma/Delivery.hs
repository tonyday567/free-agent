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
import Circuit.Agent.Mark (Mark (..), markGlyph)
import Control.Concurrent (threadDelay)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File (writeCursor)
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
    replyAct,
    replyWarning,
    turnMetrics,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removePathForcibly)
import System.Environment (getEnv, getExecutablePath, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
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
            cliExhausted = \_ _ -> False,
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
            cliExhausted = \_ _ -> False,
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

  -------------------------------------------------------------------------
  -- Kimi publication through the wired binary: direct prose replies to the
  -- sender, while one addressed presentation bullet is removed before replyAct.
  -- The transcript retains raw CLI output; delivery receives the scrubbed form.
  --
  -- Mutation guards: removing defaultReply loses the plain, bullet, and
  -- preambled replies; removing the guarded kimiText strip loses the explicitly
  -- addressed reply's body; dropping replyAct's channel or ancestry fails the
  -- reply assertions; replacing raw with scrubbed output fails the raw assertion.
  -------------------------------------------------------------------------
  putStrLn "Kimi reply-act publication"
  channelRoot <- tempRoot "free-agent-kimi-channel"
  channelCause <- postTo channelRoot ((mkPost "yang" ["kimi1", "ops"] "cause") {channel = Just "ops"})
  publicCause <- postTo channelRoot (mkPost "yang" [] "public")
  selfCause <- postTo channelRoot (mkPost "kimi1" ["kimi1"] "self")
  assert "replyAct publishes an unaddressed direct answer to the sender" $
    case replyAct "kimi1" channelCause "ordinary direct answer" of
      [r] ->
        body r == "ordinary direct answer"
          && to r == ["yang"]
          && channel r == Just "ops"
          && thread r == [snd (stamp channelCause)]
          && replyWarning "kimi1" "ordinary direct answer" == Nothing
      _ -> False
  assert "LOCAL suppresses the directed default, including leading whitespace" $
    null (replyAct "kimi1" channelCause "  LOCAL\nordinary local prose")
      && null (replyAct "kimi1" publicCause "public prose")
      && null (replyAct "kimi1" selfCause "self prose")
      && null (replyAct "kimi1" channelCause " \n")
  assert "a prefix-first address publishes without a warning" $
    replyWarning "kimi1" "@yang: explicit report" == Nothing
  assert "a later line-leading address publishes its suffix and warns about the dropped prefix" $
    case replyAct "kimi1" channelCause "progress\n@yang: DONE\nsecond line" of
      [r] ->
        body r == "DONE\nsecond line"
          && to r == ["yang"]
          && channel r == Just "ops"
          && thread r == [snd (stamp channelCause)]
          && replyWarning "kimi1" "progress\n@yang: DONE\nsecond line"
            == Just "🟡 kimi1: @yang: begins at character offset 9, not 0; publishing addressed suffix"
      _ -> False
  assert "a bar-prefixed diff above the address publishes only the reply" $
    case replyAct "kimi1" channelCause "│ +;; first\n│ +;; second\n@yang: DONE" of
      [r] -> to r == ["yang"] && body r == "DONE"
      _ -> False
  assert "an address in a raw diff body does not displace the final reply" $
    case replyAct "kimi1" channelCause "@@ -0,0 +1 @@\n@alice: diff body\n@bob: reply" of
      [r] -> to r == ["bob"] && body r == "reply"
      _ -> False
  assert "a later self-address is not a publication target" $
    case replyAct "kimi1" channelCause "tool\n@bob: reply\n@kimi1: self" of
      [r] -> to r == ["bob"] && body r == "reply\n@kimi1: self"
      _ -> False
  assert "a Markdown blockquote address is not an explicit recipient" $
    case replyAct "kimi1" channelCause "> @alice: quoted\nordinary direct prose" of
      [r] -> to r == ["yang"] && body r == "> @alice: quoted\nordinary direct prose"
      _ -> False
  assert "a fenced address is ignored in favour of the real reply" $
    case replyAct "kimi1" channelCause "```text\n@alice: hi\n```\n@bob: reply" of
      [r] -> to r == ["bob"] && body r == "reply"
      _ -> False
  assert "a real address is not displaced by a later fenced quote" $
    case replyAct "kimi1" channelCause "@bob: reply\n```text\n@carol: quoted\n```" of
      [r] ->
        to r == ["bob"]
          && body r == "reply\n```text\n@carol: quoted\n```"
      _ -> False
  assert "replyAct preserves an addressed reply's channel and ancestry" $
    case replyAct "kimi1" channelCause "@yang: explicit report" of
      [r] ->
        body r == "explicit report"
          && to r == ["yang"]
          && channel r == Just "ops"
          && thread r == [snd (stamp channelCause)]
      _ -> False
  assert "an inline address does not displace the directed default" $
    case replyAct "kimi1" channelCause "• working\n\n• @yang: DONE not-earned" of
      [r] -> to r == ["yang"] && body r == "• working\n\n• @yang: DONE not-earned"
      _ -> False
  removePathForcibly channelRoot

  kimiRoot <- tempRoot "free-agent-kimi-publication"
  let fakeBin = kimiRoot </> "bin"
      fakeKimi = fakeBin </> "kimi"
      promptFile = kimiRoot </> "prompt.md"
      kimiSessionFile = kimiRoot </> "session"
      kimiTranscriptLog = kimiRoot </> ".transcripts" </> "kimi1.jsonl"
  TIO.writeFile promptFile "You are a test seat."
  createDirectoryIfMissing True fakeBin
  TIO.writeFile fakeKimi $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "input=\"$*\"",
          "case \"$input\" in",
          "  *PLAIN-CASE*) printf 'ordinary local prose\\n' ;;",
          "  *EXPLICIT-CASE*) printf '• @yang: explicit report\\n' ;;",
          "  *DIFF-CASE*) printf '│ tool diff\\n@yang: recovered report\\n' ;;",
          "  *PREAMBLE-CASE*) printf '• working\\n\\n• @yang: DONE not-earned\\n' ;;",
          "  *BULLET-CASE*) printf '• ordinary bullet\\n' ;;",
          "esac",
          "printf 'To resume this session: kimi -r fake-kimi-integration\\n'"
        ]
  setFileMode fakeKimi 0o755
  writeCursor kimiRoot "kimi1" 0
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "PLAIN-CASE")
  explicitCause <- postTo kimiRoot (mkPost "yang" ["kimi1"] "EXPLICIT-CASE")
  diffCause <- postTo kimiRoot (mkPost "yang" ["kimi1"] "DIFF-CASE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "PREAMBLE-CASE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "BULLET-CASE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] (markGlyph Landed))
  myPath <- takeDirectory <$> getExecutablePath
  pathBin <- findExecutable "free-agent"
  let bin =
        maybe
          (myPath </> ".." </> ".." </> ".." </> "free-agent" </> "build" </> "free-agent" </> "free-agent")
          id
          pathBin
  oldPath <- getEnv "PATH"
  setEnv "PATH" (fakeBin <> ":" <> oldPath)
  (code, _out, err) <-
    readProcessWithExitCode
      bin
      [ "--root",
        kimiRoot,
        "--name",
        "kimi1",
        "--backend",
        "kimi",
        "--prompt",
        promptFile,
        "--session",
        kimiSessionFile
      ]
      ""
  setEnv "PATH" oldPath
  assert ("the wired Kimi seat completes its scripted turns" <> if code == ExitSuccess then "" else ": " <> err) $
    code == ExitSuccess
  let adapterWarnings = filter ("🟡 kimi1: @yang: begins at character offset " `T.isPrefixOf`) (T.lines (T.pack err))
  assert ("the wired adapter warns once for prefix-later, not for prefix-first or local prose" <> if length adapterWarnings == 1 then "" else ": " <> err) $
    length adapterWarnings == 1
  content <- TIO.readFile (kimiRoot </> "log.jsonl")
  let parsed = mapMaybe (unframeStored @Text) (T.lines content)
      replies = [stamped s | s <- parsed, from (stamped s) == "kimi1"]
  assert ("plain, preambled, and ordinary bullet prose use the directed default: " <> show (map body replies)) $
    map body replies
      == [ "ordinary local prose",
           "explicit report",
           "recovered report",
           "• working\n\n• @yang: DONE not-earned",
           "• ordinary bullet"
         ]
  assert "the addressed bullet publishes with ancestry" $
    case [r | r <- replies, body r == "explicit report"] of
      [r] ->
        to r == ["yang"]
          && channel r == Nothing
          && thread r == [snd (stamp explicitCause)]
      _ -> False
  assert "the tool-prefixed reply publishes with ancestry" $
    case [r | r <- replies, body r == "recovered report"] of
      [r] ->
        to r == ["yang"]
          && channel r == Nothing
          && thread r == [snd (stamp diffCause)]
      _ -> False
  kimiTranscript <- TIO.readFile kimiTranscriptLog
  assert "Kimi raw output preserves the addressed presentation bullet" $
    "• @yang: explicit report" `T.isInfixOf` kimiTranscript
  removePathForcibly kimiRoot
