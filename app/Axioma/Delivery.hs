{-# LANGUAGE OverloadedStrings #-}

-- | Delivery algebra: quiet settlement, exactly-once response, and explicit
-- publication as either a root post or a reply.
module Axioma.Delivery
  ( deliveryTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Parser.Framing (decodeRow)
import Circuit.Post (Post (..), replyTo)
import Circuit.Shared (stamp, stamped)
import Control.Concurrent (threadDelay)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File (readCursor, readTransaction, writeCursor)
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliDeliverTo, kimiTextWith, parseSessionId)
import Free.Agent.Delivery
  ( DeliveryRefusal (..),
    Invocation (..),
    TurnMetrics (..),
    TurnResult (..),
    awaitTurn,
    cancelTurn,
    deliverTo,
    directPostTo,
    mapTurnIO,
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
import Free.Agent.Mark (Mark (..), markGlyph)
import Free.Agent.Protocol (Protocol (..), defaultProtocol, isSuppressedReply, protocolSummary)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removePathForcibly)
import System.Environment (getEnv, getExecutablePath, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

seatBinary :: IO FilePath
seatBinary = do
  axiomaPath <- getExecutablePath
  let built =
        takeDirectory axiomaPath
          </> ".."
          </> ".."
          </> ".."
          </> "free-agent"
          </> "build"
          </> "free-agent"
          </> "free-agent"
  builtExists <- doesFileExist built
  if builtExists
    then pure built
    else fromMaybe (takeDirectory axiomaPath </> "free-agent") <$> findExecutable "free-agent"

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
  let quietPosts = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines quietLog)
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
  let finalPosts = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines finalLog)
  assert "the public tape contains only the cause and two explicit post acts" $
    length finalPosts == 3
  finalCalls <- TIO.readFile calls
  assert "neither publication shape invokes another turn" $
    length (T.lines finalCalls) == 1

  putStrLn "mapped settlement is shared"
  mappedEffects <- newIORef []
  let mappedTurn label = do
        base <-
          deliverTo
            ( \() _ ->
                pure
                  Invocation
                    { runInvocation = pure (TurnResponse label),
                      cancelInvocation = pure False
                    }
            )
            ()
            ""
        pure $
          mapTurnIO
            (\response -> modifyIORef' mappedEffects (<> [response]) >> pure response)
            base
  firstMapped <- mappedTurn ("first" :: Text)
  secondMapped <- mappedTurn "second"
  firstResult1 <- awaitTurn firstMapped
  firstResult2 <- awaitTurn firstMapped
  secondResult1 <- awaitTurn secondMapped
  secondResult2 <- awaitTurn secondMapped
  effects <- readIORef mappedEffects
  assert "B22 repeated awaits run one mapped effect and never share another turn's result" $
    firstResult1 == TurnResponse "first"
      && firstResult2 == firstResult1
      && secondResult1 == TurnResponse "second"
      && secondResult2 == secondResult1
      && effects == ["first", "second"]
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
  let cancelPosts = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines cancelLog)
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
  -- Intentional publication: only an explicit address publishes. Plain prose,
  -- LOCAL, public-looking text, and self commentary all stay local. Addressed
  -- publication carries no inferred channel or ancestry.
  -------------------------------------------------------------------------
  putStrLn "intentional reply-act publication"
  assert "unaddressed direct prose stays local" $
    null (replyAct defaultProtocol "kimi1" "ordinary direct answer")
      && replyWarning defaultProtocol "kimi1" "ordinary direct answer" == Nothing
  assert "LOCAL, public-looking, self, and empty prose stay local" $
    null (replyAct defaultProtocol "kimi1" "  LOCAL\nordinary local prose")
      && null (replyAct defaultProtocol "kimi1" "public prose")
      && null (replyAct defaultProtocol "kimi1" "self prose")
      && null (replyAct defaultProtocol "kimi1" " \n")
  assert "a prefix-first address publishes without a warning" $
    replyWarning defaultProtocol "kimi1" "@yang: explicit report" == Nothing
  assert "a later line-leading address publishes its suffix and warns about the dropped prefix" $
    case replyAct defaultProtocol "kimi1" "progress\n@yang: DONE\nsecond line" of
      [r] ->
        body r == "DONE\nsecond line"
          && to r == ["yang"]
          && channel r == Nothing
          && thread r == []
          && replyWarning defaultProtocol "kimi1" "progress\n@yang: DONE\nsecond line"
            == Just "🟡 kimi1: @yang: begins at character offset 9, not 0; publishing addressed suffix"
      _ -> False
  assert "a bar-prefixed diff above the address publishes only the reply" $
    case replyAct defaultProtocol "kimi1" "│ +;; first\n│ +;; second\n@yang: DONE" of
      [r] -> to r == ["yang"] && body r == "DONE"
      _ -> False
  assert "an address in a raw diff body does not displace the final reply" $
    case replyAct defaultProtocol "kimi1" "@@ -0,0 +1 @@\n@alice: diff body\n@bob: reply" of
      [r] -> to r == ["bob"] && body r == "reply"
      _ -> False
  assert "a later self-address is not a publication target" $
    case replyAct defaultProtocol "kimi1" "tool\n@bob: reply\n@kimi1: self" of
      [r] -> to r == ["bob"] && body r == "reply\n@kimi1: self"
      _ -> False
  assert "a Markdown blockquote address stays local with its prose" $
    null (replyAct defaultProtocol "kimi1" "> @alice: quoted\nordinary direct prose")
  assert "a fenced address is ignored in favour of the real reply" $
    case replyAct defaultProtocol "kimi1" "```text\n@alice: hi\n```\n@bob: reply" of
      [r] -> to r == ["bob"] && body r == "reply"
      _ -> False
  assert "a real address is not displaced by a later fenced quote" $
    case replyAct defaultProtocol "kimi1" "@bob: reply\n```text\n@carol: quoted\n```" of
      [r] ->
        to r == ["bob"]
          && body r == "reply\n```text\n@carol: quoted\n```"
      _ -> False
  assert "replyAct leaves channel and ancestry to deliberate citation" $
    case replyAct defaultProtocol "kimi1" "@yang: explicit report" of
      [r] ->
        body r == "explicit report"
          && to r == ["yang"]
          && channel r == Nothing
          && thread r == []
      _ -> False
  assert "an inline address does not manufacture a recipient" $
    null (replyAct defaultProtocol "kimi1" "• working\n\n• @yang: DONE not-earned")
  assert "an address behind leading whitespace is an explicit recipient" $
    case replyAct defaultProtocol "kimi1" "  @yang: front-line report" of
      [r] ->
        to r == ["yang"]
          && body r == "front-line report"
          && thread r == []
          && replyWarning defaultProtocol "kimi1" "  @yang: front-line report" == Nothing
      _ -> False
  assert "a blank leading line before the address is prefix-first, not a recovery" $
    case replyAct defaultProtocol "kimi1" "\n@yang: after a blank line" of
      [r] ->
        to r == ["yang"]
          && body r == "after a blank line"
          && replyWarning defaultProtocol "kimi1" "\n@yang: after a blank line" == Nothing
      _ -> False
  assert "an indented address on a later line recovers its suffix with the offset" $
    case replyAct defaultProtocol "kimi1" "notes\n  @yang: recovered report" of
      [r] ->
        to r == ["yang"]
          && body r == "recovered report"
          && replyWarning defaultProtocol "kimi1" "notes\n  @yang: recovered report"
            == Just "🟡 kimi1: @yang: begins at character offset 8, not 0; publishing addressed suffix"
      _ -> False

  putStrLn "configurable reply and presentation protocol"
  let custom =
        defaultProtocol
          { protocolAddressPrefix = "%",
            protocolAddressTerminator = "=",
            protocolPresentationBullet = "* ",
            protocolEmptyReply = "EMPTY"
          }
  assert "a supplied address grammar drives publication and warnings" $
    case replyAct custom "kimi1" "notes\n  %yang= custom report" of
      [r] ->
        to r == ["yang"]
          && body r == "custom report"
          && replyWarning custom "kimi1" "notes\n  %yang= custom report"
            == Just "🟡 kimi1: %yang= begins at character offset 8, not 0; publishing addressed suffix"
      _ -> False
  assert "neither protocol implicitly reads the other's address" $
    null (replyAct custom "kimi1" "@yang: legacy")
      && null (replyAct defaultProtocol "kimi1" "%yang= custom")
  assert "the Kimi presentation scrub uses the same custom address prefix" $
    kimiTextWith custom "* %yang= custom report" == "%yang= custom report"
  assert "empty-reply suppression is configured, not a private runner literal" $
    isSuppressedReply custom "  EMPTY\n" && not (isSuppressedReply defaultProtocol "EMPTY")
  assert "help derives the address convention from the supplied protocol" $
    "%name=" `T.isInfixOf` protocolSummary custom

  -------------------------------------------------------------------------
  -- Stranded-reply notes: a reply published only from its later-line address
  -- leaves a note for the seat's next delivery; reading the note clears it;
  -- clean, prefix-first, and local replies leave none.
  -------------------------------------------------------------------------
  putStrLn "stranded-reply notes"
  strandRoot <- tempRoot "free-agent-strand-note"
  strandCause <- postTo strandRoot (mkPost "yang" ["kimi1"] "STRAND")
  assert "a stranded reply renders one note naming delivery, recipient, and loss" $
    case strandedNote defaultProtocol "kimi1" strandCause "notes\n@yang: stranded report" of
      Just n ->
        ("🟡 your reply for delivery " <> T.pack (show (snd (stamp strandCause)))) `T.isPrefixOf` n
          && "`@yang:`" `T.isInfixOf` n
          && "did not reach the bus" `T.isInfixOf` n
      Nothing -> False
  assert "notes are silent for clean, prefix-first, and self-addressed replies" $
    all
      (== Nothing)
      [ strandedNote defaultProtocol "kimi1" strandCause "ordinary direct answer",
        strandedNote defaultProtocol "kimi1" strandCause "@yang: prefix-first report",
        strandedNote defaultProtocol "kimi1" strandCause "  LOCAL\nthinking",
        strandedNote defaultProtocol "kimi1" strandCause "tool\n@kimi1: self only"
      ]
  noteStrand defaultProtocol strandRoot "kimi1" strandCause "notes\n@yang: stranded report"
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
  noteStrand defaultProtocol strandRoot "kimi1" strandCause "ordinary direct answer"
  quietRead <- takeStrandNotes strandRoot "kimi1"
  assert "a clean reply leaves no note" (null quietRead)
  removePathForcibly strandRoot

  -------------------------------------------------------------------------
  -- Wired Kimi batch delivery: three posts cause one Backend invocation.
  -- Reply output is content verbatim; audience and ancestry come from input.
  -------------------------------------------------------------------------
  putStrLn "wired Kimi batch publication"
  kimiRoot <- tempRoot "free-agent-kimi-batch"
  let fakeBin = kimiRoot </> "bin"
      fakeKimi = fakeBin </> "kimi"
      promptFile = kimiRoot </> "prompt.md"
      kimiSessionFile = kimiRoot </> "session"
      kimiTranscriptLog = kimiRoot </> ".transcripts" </> "kimi1.jsonl"
      kimiDossier = kimiRoot </> "dossier"
      kimiArgs =
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
  TIO.writeFile promptFile "You are a test seat."
  createDirectoryIfMissing True fakeBin
  TIO.writeFile fakeKimi $
    T.pack $
      unlines
        [ "#!/bin/sh",
          "input=\"$*\"",
          "printf '\\n===CALL===\\n%s\\n' \"$input\" >> \"" <> kimiDossier <> "\"",
          "case \"$input\" in",
          "  *CHECK-CASE*) printf 'checked\\n' ;;",
          "  *BATCH-THREE*) printf 'local batch notes\\n@yang: kimi batch report\\n' ;;",
          "esac",
          "printf 'To resume this session: kimi -r fake-kimi-integration\\n'"
        ]
  setFileMode fakeKimi 0o755
  writeCursor kimiRoot "kimi1" 0
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "BATCH-ONE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "BATCH-TWO")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "BATCH-THREE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] (markGlyph Landed))
  bin <- seatBinary
  oldPath <- getEnv "PATH"
  setEnv "PATH" (fakeBin <> ":" <> oldPath)
  (code, _out, err) <- readProcessWithExitCode bin kimiArgs ""
  setEnv "PATH" oldPath
  assert ("the wired Kimi batch completes" <> if code == ExitSuccess then "" else ": " <> err) $
    code == ExitSuccess
  content <- TIO.readFile (kimiRoot </> "log.jsonl")
  let parsed = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines content)
      replies = [stamped s | s <- parsed, from (stamped s) == "kimi1"]
  assert "the complete reply output publishes with explicit input ancestry" $
    case replies of
      [r] ->
        to r == ["yang"]
          && body r == "local batch notes\n@yang: kimi batch report"
          && channel r == Nothing
          && thread r == [0, 1, 2]
      _ -> False
  firstCursor <- readCursor kimiRoot "kimi1"
  firstTransaction <- readTransaction kimiRoot "kimi1"
  assert "the wired quiet-capable batch commits before its halt" $
    firstCursor == 4 && firstTransaction == Just 3
  kimiDossierText <- TIO.readFile kimiDossier
  let kimiCalls = filter (not . T.null . T.strip) (T.splitOn "===CALL===" kimiDossierText)
  assert "three addressed posts cause exactly one wired CLI call" $
    length kimiCalls == 1
  assert "the wired prompt preserves every post id, sender, body, and order" $
    case kimiCalls of
      [call] ->
        "[post 0] yang: BATCH-ONE\n\n[post 1] yang: BATCH-TWO\n\n[post 2] yang: BATCH-THREE"
          `T.isInfixOf` call
      _ -> False
  let adapterWarnings = filter ("🟡 kimi1: @yang: begins at character offset " `T.isPrefixOf`) (T.lines (T.pack err))
  stranded <- doesFileExist (strandNotePath kimiRoot "kimi1")
  assert "response scraping warnings and strand state stay out of the live Backend path" $
    null adapterWarnings && not stranded
  kimiTranscript <- TIO.readFile kimiTranscriptLog
  assert "the transcript retains the local prefix and explicit response" $
    "local batch notes" `T.isInfixOf` kimiTranscript
      && "@yang: kimi batch report" `T.isInfixOf` kimiTranscript

  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] "CHECK-CASE")
  _ <- postTo kimiRoot (mkPost "yang" ["kimi1"] (markGlyph Landed))
  setEnv "PATH" (fakeBin <> ":" <> oldPath)
  (checkCode, _checkOut, checkErr) <- readProcessWithExitCode bin kimiArgs ""
  setEnv "PATH" oldPath
  assert ("the wired Kimi singleton completes" <> if checkCode == ExitSuccess then "" else ": " <> checkErr) $
    checkCode == ExitSuccess
  kimiDossierAfter <- TIO.readFile kimiDossier
  let callsAfter = filter (not . T.null . T.strip) (T.splitOn "===CALL===" kimiDossierAfter)
  assert "the next live batch is one singleton Backend call without harness notes" $
    length callsAfter == 2
      && case callsAfter of
        [_, checkCall] ->
          "[post 5] yang: CHECK-CASE" `T.isInfixOf` checkCall
            && not ("Harness notes:" `T.isInfixOf` checkCall)
        _ -> False
  checkContent <- TIO.readFile (kimiRoot </> "log.jsonl")
  let checkParsed = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines checkContent)
      checkReplies = [stamped s | s <- checkParsed, from (stamped s) == "kimi1"]
  assert "plain singleton output is also an explicit reply act" $
    case checkReplies of
      [_, r] -> to r == ["yang"] && body r == "checked" && thread r == [5]
      _ -> False
  kimiStrandGone <- doesFileExist (strandNotePath kimiRoot "kimi1")
  assert "another Backend turn leaves no stranded note" $ not kimiStrandGone
  removePathForcibly kimiRoot

  -------------------------------------------------------------------------
  -- Wired Hermes batch capture: the Backend receives the whole mail batch
  -- once. Reply text is content verbatim; audience and ancestry come from
  -- the committed input. Usage, transcript, and file evidence remain intact.
  -------------------------------------------------------------------------
  putStrLn "wired Hermes batch capture"
  hermesRoot <- tempRoot "free-agent-hermes-batch"
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
          "  *BATCH-THREE*) printf 'hello\\n' >> \"" <> hermesNotes <> "\"; printf '@yang: hermes batch report\\n' ;;",
          "esac"
        ]
  setFileMode fakeHermes 0o755
  writeCursor hermesRoot "hermes1" 0
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] "BATCH-ONE")
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] "BATCH-TWO")
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] "BATCH-THREE")
  _ <- postTo hermesRoot (mkPost "yang" ["hermes1"] (markGlyph Landed))
  bin2 <- seatBinary
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
  assert ("the wired Hermes batch completes" <> if code2 == ExitSuccess then "" else ": " <> err2) $
    code2 == ExitSuccess
  hermesContent <- TIO.readFile (hermesRoot </> "log.jsonl")
  let hermesParsed = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines hermesContent)
      hermesReplies = [stamped s | s <- hermesParsed, from (stamped s) == "hermes1"]
  assert "the complete Hermes reply publishes with explicit input ancestry" $
    case hermesReplies of
      [r] ->
        to r == ["yang"]
          && body r == "@yang: hermes batch report"
          && channel r == Nothing
          && thread r == [0, 1, 2]
      _ -> False
  hermesCursor <- readCursor hermesRoot "hermes1"
  hermesTransaction <- readTransaction hermesRoot "hermes1"
  assert "the wired Hermes batch writes its transaction before the halt" $
    hermesCursor == 4 && hermesTransaction == Just 3
  hermesDossierText <- TIO.readFile hermesDossier
  let hermesCalls = filter (not . T.null . T.strip) (T.splitOn "===CALL===" hermesDossierText)
  assert "three Hermes posts cause one CLI call containing the whole ordered batch" $
    case hermesCalls of
      [call] ->
        "[post 0] yang: BATCH-ONE\n\n[post 1] yang: BATCH-TWO\n\n[post 2] yang: BATCH-THREE"
          `T.isInfixOf` call
      _ -> False
  hermesTranscript <- TIO.readFile hermesTranscriptLog
  assert "the transcript keeps the final message and raw usage report" $
    "@yang: hermes batch report" `T.isInfixOf` hermesTranscript
      && "\"session_id\":\"fake-hermes-integration\"" `T.isInfixOf` hermesTranscript
  hermesSid <- TIO.readFile hermesSessionFile
  assert "the usage-file id reaches the stored session file" $
    T.strip hermesSid == "fake-hermes-integration"
  hermesNotesText <- TIO.readFile hermesNotes
  assert "the batch turn retained its file side effect" $
    hermesNotesText == "hello\n"
  removePathForcibly hermesRoot
