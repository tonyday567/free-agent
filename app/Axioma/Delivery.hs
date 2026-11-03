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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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
    noteStrand,
    postTo,
    replyAct,
    replyWarning,
    strandBlock,
    strandNotePath,
    strandedNote,
    takeStrandNotes,
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
            cliStderrTee = Nothing,
            cliSessionReport = Nothing
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
            cliStderrTee = Nothing,
            cliSessionReport = Nothing
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
  assert "an address behind leading whitespace is an explicit recipient" $
    case replyAct "kimi1" channelCause "  @yang: front-line report" of
      [r] ->
        to r == ["yang"]
          && body r == "front-line report"
          && thread r == [snd (stamp channelCause)]
          && replyWarning "kimi1" "  @yang: front-line report" == Nothing
      _ -> False
  assert "a blank leading line before the address is prefix-first, not a recovery" $
    case replyAct "kimi1" channelCause "\n@yang: after a blank line" of
      [r] ->
        to r == ["yang"]
          && body r == "after a blank line"
          && replyWarning "kimi1" "\n@yang: after a blank line" == Nothing
      _ -> False
  assert "an indented address on a later line recovers its suffix with the offset" $
    case replyAct "kimi1" channelCause "notes\n  @yang: recovered report" of
      [r] ->
        to r == ["yang"]
          && body r == "recovered report"
          && replyWarning "kimi1" "notes\n  @yang: recovered report"
            == Just "🟡 kimi1: @yang: begins at character offset 8, not 0; publishing addressed suffix"
      _ -> False
  removePathForcibly channelRoot

  -------------------------------------------------------------------------
  -- Stranded-reply notes: a reply published only from its later-line address
  -- leaves a note for the seat's next delivery; reading the note clears it;
  -- clean, prefix-first, and local replies leave none.
  -------------------------------------------------------------------------
  putStrLn "stranded-reply notes"
  strandRoot <- tempRoot "free-agent-strand-note"
  strandCause <- postTo strandRoot (mkPost "yang" ["kimi1"] "STRAND")
  assert "a stranded reply renders one note naming delivery, recipient, and loss" $
    case strandedNote "kimi1" strandCause "notes\n@yang: stranded report" of
      Just n ->
        ("🟡 your reply for delivery " <> T.pack (show (snd (stamp strandCause)))) `T.isPrefixOf` n
          && "`@yang:`" `T.isInfixOf` n
          && "did not reach the bus" `T.isInfixOf` n
      Nothing -> False
  assert "notes are silent for clean, prefix-first, and self-addressed replies" $
    all
      (== Nothing)
      [ strandedNote "kimi1" strandCause "ordinary direct answer",
        strandedNote "kimi1" strandCause "@yang: prefix-first report",
        strandedNote "kimi1" strandCause "  LOCAL\nthinking",
        strandedNote "kimi1" strandCause "tool\n@kimi1: self only"
      ]
  noteStrand strandRoot "kimi1" strandCause "notes\n@yang: stranded report"
  strandFile <- doesFileExist (strandNotePath strandRoot "kimi1")
  assert "the note lands under .strand-<seat>" strandFile
  notesRead <- takeStrandNotes strandRoot "kimi1"
  assert "the next delivery reads the note" $
    case notesRead of
      [n] -> "did not reach the bus" `T.isInfixOf` n
      _ -> False
  assert "the block renders under a Harness notes header" $
    strandBlock notesRead == "\n\nHarness notes:\n" <> T.intercalate "\n" notesRead
  strandGone <- doesFileExist (strandNotePath strandRoot "kimi1")
  secondRead <- takeStrandNotes strandRoot "kimi1"
  assert "reading the notes clears them" $ not strandGone && null secondRead
  noteStrand strandRoot "kimi1" strandCause "ordinary direct answer"
  quietRead <- takeStrandNotes strandRoot "kimi1"
  assert "a clean reply leaves no note" (null quietRead)
  removePathForcibly strandRoot

  kimiRoot <- tempRoot "free-agent-kimi-publication"
  let fakeBin = kimiRoot </> "bin"
      fakeKimi = fakeBin </> "kimi"
      promptFile = kimiRoot </> "prompt.md"
      kimiSessionFile = kimiRoot </> "session"
      kimiTranscriptLog = kimiRoot </> ".transcripts" </> "kimi1.jsonl"
  TIO.writeFile promptFile "You are a test seat."
  createDirectoryIfMissing True fakeBin
  let kimiDossier = kimiRoot </> "dossier"
      kimiNotes = kimiRoot </> "notes.md"
  TIO.writeFile fakeKimi $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "input=\"$*\"",
          "printf '\\n===CALL===\\n%s\\n' \"$input\" >> \"" <> kimiDossier <> "\"",
          "case \"$input\" in",
          "  *PLAIN-CASE*) printf 'ordinary local prose\\n' ;;",
          "  *EXPLICIT-CASE*) printf '• @yang: explicit report\\n' ;;",
          "  *DIFF-CASE*) printf '│ tool diff\\n@yang: recovered report\\n' ;;",
          "  *PREAMBLE-CASE*) printf '• working\\n\\n• @yang: DONE not-earned\\n' ;;",
          "  *BULLET-CASE*) printf '• ordinary bullet\\n' ;;",
          "  *WRITE-CASE*) printf '│ notes.md\\n│ +hello\\n' >&2; printf 'hello\\n' >> \"" <> kimiNotes <> "\"; printf '@yang: DONE — notes.md written\\n' ;;",
          "  *STRAND-CASE*) printf 'planning notes\\n@yang: stranded report\\n' ;;",
          "  *CHECK-CASE*) printf 'checked\\n' ;;",
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
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "WRITE-CASE")
  kimiStrandCause <- postTo kimiRoot (mkPost "yang" ["kimi1"] "STRAND-CASE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "CHECK-CASE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] (markGlyph Landed))
  myPath <- takeDirectory <$> getExecutablePath
  pathBin <- findExecutable "free-agent"
  let bin =
        fromMaybe
          (myPath </> ".." </> ".." </> ".." </> "free-agent" </> "build" </> "free-agent" </> "free-agent")
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
  assert (("the wired adapter warns for each recovered prefix-later reply (diff, strand), not for prefix-first or local prose" <> if length adapterWarnings == 2 then "" else ": " <> err)) $
    length adapterWarnings == 2
  content <- TIO.readFile (kimiRoot </> "log.jsonl")
  let parsed = mapMaybe (unframeStored @Text) (T.lines content)
      replies = [stamped s | s <- parsed, from (stamped s) == "kimi1"]
  assert ("plain, preambled, and ordinary bullet prose use the directed default: " <> show (map body replies)) $
    map body replies
      == [ "ordinary local prose",
           "explicit report",
           "recovered report",
           "• working\n\n• @yang: DONE not-earned",
           "• ordinary bullet",
           "DONE — notes.md written",
           "stranded report",
           "checked"
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
  assert "the writing turn's render stays in the transcript while its reply is clean" $
    "│ notes.md" `T.isInfixOf` kimiTranscript
      && "@yang: DONE — notes.md written" `T.isInfixOf` kimiTranscript
  kimiNotesText <- TIO.readFile kimiNotes
  assert "the writing turn wrote its file" $ kimiNotesText == "hello\n"
  kimiDossierText <- TIO.readFile kimiDossier
  let kimiCalls = filter (not . T.null . T.strip) (T.splitOn "===CALL===" kimiDossierText)
  assert "the seat ran one scripted turn per cause" $ length kimiCalls == 8
  assert "each stranded note rides exactly the next delivery, once" $
    [i | (i, c) <- zip [0 :: Int ..] kimiCalls, "Harness notes:" `T.isInfixOf` c] == [3, 7]
  assert "the diff and strand notes name their deliveries, recipient, and loss" $
    case (kimiCalls !! 3, kimiCalls !! 7) of
      (preambleCall, checkCall) ->
        ("for delivery " <> T.pack (show (snd (stamp diffCause))) <> " published from its `@yang:` address onward only; the text above the address did not reach the bus.") `T.isInfixOf` preambleCall
          && ("for delivery " <> T.pack (show (snd (stamp kimiStrandCause))) <> " published from its `@yang:` address onward only; the text above the address did not reach the bus.") `T.isInfixOf` checkCall
  kimiStrandGone <- doesFileExist (strandNotePath kimiRoot "kimi1")
  assert "the stranded note is cleared once delivered" $ not kimiStrandGone
  removePathForcibly kimiRoot

  -------------------------------------------------------------------------
  -- Hermes capture through the wired binary: the oneshot backend (-z)
  -- prints only the final message, so an addressed reply publishes at the
  -- agent's own character zero with no render above it; the usage-file
  -- carries the session id into the raw evidence and the stored session
  -- file. A stranded turn still delivers its note on the next delivery.
  -------------------------------------------------------------------------
  putStrLn "Hermes capture and stranded notes through the wired binary"
  hermesRoot <- tempRoot "free-agent-hermes-capture"
  let hermesBin = hermesRoot </> "bin"
      fakeHermes = hermesBin </> "hermes"
      hermesPromptFile = hermesRoot </> "prompt.md"
      hermesSessionFile = hermesRoot </> ".sessions" </> "hermes1.sid"
      hermesTranscriptLog = hermesRoot </> ".transcripts" </> "hermes1.jsonl"
      hermesDossier = hermesRoot </> "dossier"
      hermesNotes = hermesRoot </> "notes.md"
  TIO.writeFile hermesPromptFile "You are a test seat."
  createDirectoryIfMissing True hermesBin
  TIO.writeFile fakeHermes $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "# hermes -z shape: stdout carries only the final message; the",
          "# --usage-file report carries the session id, as the real tool does.",
          "input=\"$*\"",
          "printf '\\n===CALL===\\n%s\\n' \"$input\" >> \"" <> hermesDossier <> "\"",
          "usage=\"\"",
          "prev=\"\"",
          "for a in \"$@\"; do",
          "  if [ \"$prev\" = \"--usage-file\" ]; then usage=\"$a\"; fi",
          "  prev=\"$a\"",
          "done",
          "if [ -n \"$usage\" ]; then printf '{\"session_id\": \"fake-hermes-integration\", \"completed\": true}\\n' > \"$usage\"; fi",
          "case \"$input\" in",
          "  *WRITE-CASE*) printf 'hello\\n' >> \"" <> hermesNotes <> "\"; printf '@yang: DONE — wrote notes.md\\n' ;;",
          "  *SPACES-CASE*) printf '  @yang: front-line spaces address\\n' ;;",
          "  *LATER-CASE*) printf 'tool notes\\n  @yang: recovered suffix\\n' ;;",
          "  *STRAND-CASE*) printf 'planning notes\\n@yang: hermes stranded report\\n' ;;",
          "  *CHECK-CASE*) printf 'checked\\n' ;;",
          "esac"
        ]
  setFileMode fakeHermes 0o755
  writeCursor hermesRoot "hermes1" 0
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] "WRITE-CASE")
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] "SPACES-CASE")
  hermesLaterCause <- postTo hermesRoot (mkPost "yang" ["hermes1"] "LATER-CASE")
  hermesStrandCause <- postTo hermesRoot (mkPost "yang" ["hermes1"] "STRAND-CASE")
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] "CHECK-CASE")
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] (markGlyph Landed))
  myPath2 <- takeDirectory <$> getExecutablePath
  pathBin2 <- findExecutable "free-agent"
  let bin2 =
        fromMaybe
          (myPath2 </> ".." </> ".." </> ".." </> "free-agent" </> "build" </> "free-agent" </> "free-agent")
          pathBin2
  oldPath2 <- getEnv "PATH"
  setEnv "PATH" (hermesBin <> ":" <> oldPath2)
  (code2, _out2, err2) <-
    readProcessWithExitCode
      bin2
      [ "--root",
        hermesRoot,
        "--name",
        "hermes1",
        "--backend",
        "hermes",
        "--prompt",
        hermesPromptFile,
        "--session",
        hermesSessionFile
      ]
      ""
  setEnv "PATH" oldPath2
  assert (("the wired Hermes seat completes its scripted turns" <> if code2 == ExitSuccess then "" else ": " <> err2)) $ code2 == ExitSuccess
  hermesContent <- TIO.readFile (hermesRoot </> "log.jsonl")
  let hermesParsed = mapMaybe (unframeStored @Text) (T.lines hermesContent)
      hermesReplies = [stamped s | s <- hermesParsed, from (stamped s) == "hermes1"]
  assert ("the final message publishes at the agent's own character zero: " <> show (map body hermesReplies)) $
    map body hermesReplies
      == [ "DONE — wrote notes.md",
           "front-line spaces address",
           "recovered suffix",
           "hermes stranded report",
           "checked"
         ]
  assert "every Hermes reply answers the sender with the cause's ancestry" $
    map to hermesReplies == replicate 5 ["yang"]
      && case hermesReplies of
        (r0 : _) -> thread r0 == [0]
        _ -> False
  let hermesWarnings = filter ("🟡 hermes1: @yang: begins at character offset " `T.isPrefixOf`) (T.lines (T.pack err2))
  assert (("exactly two warnings: the recovered later-line replies at their offsets, not the prefix-first or whitespace-front ones" <> if length hermesWarnings == 2 then "" else ": " <> err2)) $
    case hermesWarnings of
      [w1, w2] -> "offset 11, not 0" `T.isInfixOf` w1 && "offset 15, not 0" `T.isInfixOf` w2
      _ -> False
  hermesTranscript <- TIO.readFile hermesTranscriptLog
  assert "the transcript keeps the whitespace raw while the reply act sees it stripped" $
    "  @yang: front-line spaces address" `T.isInfixOf` hermesTranscript
  assert "the transcript keeps the captured final message and the usage-file session id" $
    "@yang: DONE — wrote notes.md" `T.isInfixOf` hermesTranscript
      && "session_id:fake-hermes-integration" `T.isInfixOf` hermesTranscript
      && "\"session_id\":\"fake-hermes-integration\"" `T.isInfixOf` hermesTranscript
  hermesSid <- TIO.readFile hermesSessionFile
  assert "the usage-file id reaches the stored session file" $ T.strip hermesSid == "fake-hermes-integration"
  hermesNotesText <- TIO.readFile hermesNotes
  assert "the writing turn wrote its file" $ hermesNotesText == "hello\n"
  hermesDossierText <- TIO.readFile hermesDossier
  let hermesCalls = filter (not . T.null . T.strip) (T.splitOn "===CALL===" hermesDossierText)
  assert "the seat ran one scripted turn per cause" $ length hermesCalls == 5
  assert "each stranded Hermes note rides exactly the next delivery, once" $
    [i | (i, c) <- zip [0 :: Int ..] hermesCalls, "Harness notes:" `T.isInfixOf` c] == [3, 4]
  assert "the later-line and strand notes name their deliveries, recipient, and loss" $
    case (hermesCalls !! 3, hermesCalls !! 4) of
      (strandCall, checkCall) ->
        ("for delivery " <> T.pack (show (snd (stamp hermesLaterCause))) <> " published from its `@yang:` address onward only; the text above the address did not reach the bus.") `T.isInfixOf` strandCall
          && ("for delivery " <> T.pack (show (snd (stamp hermesStrandCause))) <> " published from its `@yang:` address onward only; the text above the address did not reach the bus.") `T.isInfixOf` checkCall
  hermesStrandGone <- doesFileExist (strandNotePath hermesRoot "hermes1")
  assert "the Hermes stranded note is cleared once delivered" $ not hermesStrandGone
  removePathForcibly hermesRoot
