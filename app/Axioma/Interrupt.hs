{-# LANGUAGE OverloadedStrings #-}

-- | The interrupted handler (Z1) and the failed delivery (Z2).
--
-- The census of 5-6 Nov 2026 (coffee\/loom\/scully-agent-deaths.md) measured one
-- class repeatedly: macOS idle sleep outlives the host's 300s session turn
-- lease, the host interrupts the turn, the backend exits 130, and free-agent
-- read that as a seat death.  Seat @yang@ on 31 Oct 2026 reads the other half:
-- four resumed attempts of the same session, 1,196s to 5,550s, every one ending
-- in the same interruption, because a resumed session inherits the cut-off turn.
--
-- Z1 — an interruption is the host's, not the turn's work: the batch is retried
-- once in a FRESH session (no resume, the stored id dropped first) and never in
-- the session that was cut off.  Two invocations, not four.
--
-- Z2 — a delivery that fails is retried once, then relayed and skipped, so a
-- backend failure cannot end a seat.  Explicit cancellation and a decided quiet
-- still do, and a halt decided by one batch admits no work drained behind it.
--
-- The witnesses run the real delivery and seat paths against stub backends,
-- because the claims are about how many times the loop invokes and what it
-- commits: a shell script counts its own invocations, the cursor is read back
-- from the bus, and the assertions are those counts.
module Axioma.Interrupt
  ( interruptTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (Post (..), body)
import Circuit.Agent.Framing (stamped)
import Circuit.Agent.Mark (Mark (..), markGlyph)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (IOException, SomeException, fromException, throwIO, try)
import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File (readCursor, writeCursor)
import Free.Agent.Cli
  ( Cli (..),
    StderrPolicy (StderrMerge),
    cliCeiling,
    cliDeliverTo,
    cliInterrupted,
  )
import Free.Agent.Delivery
  ( Turn (..),
    TurnCeiling (..),
    TurnMetrics (..),
    TurnResult (..),
    awaitTurn,
    newDeliveryGrant,
  )
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Timeout (timeout)

-- ---------------------------------------------------------------------------
-- Z1 stubs
-- ---------------------------------------------------------------------------

-- | A CLI whose single invocation is the given shell script.  Every invocation
-- logs the mode the delivery layer asked for (@resumed@ or @fresh@) and appends
-- a line to the counter, so a witness can count invocations and name the
-- session each one used.
stubCli :: FilePath -> FilePath -> String -> Cli
stubCli sessionFile modeLog = stubCliWith sessionFile modeLog Nothing

-- | The same stub, optionally pointable at a CLI run report.  The report is
-- where the host states its own verdict (@partial@, @turn_exit_reason@), so a
-- witness that classifies a stopped turn needs one.
stubCliWith :: FilePath -> FilePath -> Maybe FilePath -> String -> Cli
stubCliWith sessionFile modeLog report script =
  Cli
    { cliCommand = "/bin/sh",
      cliArgv = \_ mSid ->
        [ "-c",
          "echo "
            <> show (maybe "fresh" (const "resumed") mSid :: String)
            <> " >> "
            <> modeLog
            <> "; "
            <> script
        ],
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = const Nothing,
      cliStale = \_ _ -> False,
      cliExhausted = \_ _ -> False,
      cliScrub = id,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing,
      cliSessionReport = report
    }

linesOf :: FilePath -> IO [Text]
linesOf path = do
  exists <- doesFileExist path
  if exists then filter (not . T.null) . T.lines <$> TIO.readFile path else pure []

-- | Run one delivery against a stub and report (result, invocations, modes).
oneTurn :: FilePath -> String -> IO (Either IOException (TurnResult Text), Int, [Text])
oneTurn root script = do
  let counter = root </> "invocations"
      modes = root </> "modes"
      session = root </> "session"
      transcript = root </> "transcript.jsonl"
  TIO.writeFile session "stub-session\n"
  turn <- cliDeliverTo (stubCli session modes script) transcript 0 "prompt"
  result <- try (awaitTurn turn)
  seen <- linesOf counter
  used <- linesOf modes
  pure (result, length seen, used)

-- | Counts its own invocations and fails the way the interrupted hermes does.
interruptedScript :: FilePath -> String
interruptedScript counter =
  "echo invoked >> "
    <> counter
    <> "; echo 'Operation interrupted: waiting for model response (4.8s elapsed).'; exit 130"

-- | Interrupted on the first invocation, settled on the next: the recovery.
interruptedOnceScript :: FilePath -> String
interruptedOnceScript counter =
  "n=$(cat "
    <> counter
    <> " 2>/dev/null | wc -l); echo invoked >> "
    <> counter
    <> "; if [ \"$n\" -eq 0 ]; then echo 'Operation interrupted: waiting for model response (4.8s elapsed).'; exit 130;"
    <> " else echo recovered; exit 0; fi"

-- | Run one delivery with no stored session (the cold path), then report the
-- same triple as 'oneTurn'.
oneTurnCold :: FilePath -> String -> IO (Either IOException (TurnResult Text), Int, [Text])
oneTurnCold root script = do
  let counter = root </> "invocations"
      modes = root </> "modes"
      session = root </> "session"
      transcript = root </> "transcript.jsonl"
  turn <- cliDeliverTo (stubCli session modes script) transcript 0 "prompt"
  result <- try (awaitTurn turn)
  seen <- linesOf counter
  used <- linesOf modes
  pure (result, length seen, used)

-- | A transient failure: the retry ladder's own case, unchanged by this rule.
transientScript :: FilePath -> String
transientScript counter = "echo invoked >> " <> counter <> "; echo 'transient backend error'; exit 1"

message :: Either IOException a -> Text
message = either (T.pack . show) (const "")

-- ---------------------------------------------------------------------------
-- Z3 helpers · the tool-iteration ceiling
-- ---------------------------------------------------------------------------

-- | The verdict a budget stop leaves in the CLI's run report: partial work,
-- not interrupted, and a reason that names the ceiling.
ceilingReport :: String
ceilingReport =
  "{\"session_id\":\"stub-session\",\"completed\":false,\"partial\":true,"
    <> "\"interrupted\":false,\"turn_exit_reason\":\"max_iterations_reached(500)\"}"

-- | The same exit 2 for a turn that failed outright (a provider error): the
-- transient ladder's case, which a ceiling rule must not swallow.
failedReport :: String
failedReport =
  "{\"session_id\":\"stub-session\",\"completed\":false,\"partial\":false,"
    <> "\"interrupted\":false,\"turn_exit_reason\":\"provider_error\"}"

-- | Runs out of tool iterations: partial text on stdout, a report that says so,
-- exit 2 — the shape net#76 recorded.
ceilingScript :: FilePath -> FilePath -> String
ceilingScript counter report =
  "echo invoked >> "
    <> counter
    <> "; printf '%s' "
    <> show ceilingReport
    <> " > "
    <> report
    <> "; echo 'LOCAL — I ran out of tool iterations with the work staged'; exit 2"

-- | Exit 2 with no partial verdict: a failed turn, still retried.
failedTurnScript :: FilePath -> FilePath -> String
failedTurnScript counter report =
  "echo invoked >> "
    <> counter
    <> "; printf '%s' "
    <> show failedReport
    <> " > "
    <> report
    <> "; echo 'provider error'; exit 2"

-- | Run one delivery against a stub and report the exception's class, not only
-- its text: a ceiling is a 'TurnCeiling', not an 'IOException'.
oneTurnClassified :: FilePath -> Cli -> IO (Either SomeException (TurnResult Text), Int, [Text])
oneTurnClassified root cli = do
  TIO.writeFile (root </> "session") "stub-session\n"
  turn <- cliDeliverTo cli (root </> "transcript.jsonl") 0 "prompt"
  result <- try (awaitTurn turn)
  seen <- linesOf (root </> "invocations")
  used <- linesOf (root </> "modes")
  pure (result, length seen, used)

-- | A handler whose turn was stopped at the host budget.
ceilingTurn :: String -> Turn [Post Text]
ceilingTurn why =
  Turn
    { awaitTurn = throwIO (TurnCeiling (T.pack why)),
      cancelTurn = pure (),
      turnMetrics = pure (TurnMetrics False False 0)
    }

isCeiling :: Either SomeException (TurnResult Text) -> Bool
isCeiling = either (isJust . (fromException :: SomeException -> Maybe TurnCeiling)) (const False)

-- ---------------------------------------------------------------------------
-- Z2 helpers
-- ---------------------------------------------------------------------------

settledTurn :: [Post Text] -> Turn [Post Text]
settledTurn ps =
  Turn
    { awaitTurn = pure (TurnResponse ps),
      cancelTurn = pure (),
      turnMetrics = pure (TurnMetrics False False 0)
    }

failingTurn :: String -> Turn [Post Text]
failingTurn why =
  Turn
    { awaitTurn = ioError (userError why),
      cancelTurn = pure (),
      turnMetrics = pure (TurnMetrics False False 0)
    }

cancelledTurn :: Turn [Post Text]
cancelledTurn =
  Turn
    { awaitTurn = pure TurnCancelled,
      cancelTurn = pure (),
      turnMetrics = pure (TurnMetrics False False 0)
    }

-- | Bounded wait for the cursor to reach n.
awaitCursor :: FilePath -> Text -> Int -> IO Bool
awaitCursor root name n = go (500 :: Int)
  where
    go 0 = pure False
    go k = do
      c <- readCursor root name
      if c >= fromIntegral n then pure True else threadDelay 20000 >> go (k - 1)

-- | Bounded wait for the seat to have relayed n skipped batches.
awaitRelays :: FilePath -> Int -> IO Bool
awaitRelays root n = go (500 :: Int)
  where
    go 0 = pure False
    go k = do
      text <- TIO.readFile (root </> "log.jsonl")
      if T.count "batch skipped" text >= n then pure True else threadDelay 20000 >> go (k - 1)

interruptTopic :: IO ()
interruptTopic = do
  -------------------------------------------------------------------------
  -- Z1 · the interrupted handler
  -------------------------------------------------------------------------
  putStrLn "the interrupted handler (Z1)"
  assert "Z1a exit 130 and the interruption words are both interruptions" $
    cliInterrupted (ExitFailure 130) ""
      && cliInterrupted ExitSuccess "Operation interrupted: waiting for model response (4.8s elapsed)."
  assert "Z1b an ordinary failure is not an interruption" $
    not (cliInterrupted (ExitFailure 1) "transient backend error")
      && not (cliInterrupted (ExitFailure 143) "killed")
  -- Control: a transient failure still makes 1 + 3 invocations, all resuming.
  rootA <- tempRoot "free-agent-interrupt-transient"
  (resA, nA, modesA) <- oneTurn rootA (transientScript (rootA </> "invocations"))
  let msgA = message resA
  assert ("Z1c a transient failure still retries 1 + 3 times (invocations " <> show nA <> ")") (nA == 4)
  assert ("Z1d and reports the session as failed, as before: " <> T.unpack (T.take 60 msgA)) $
    "resumed session failed after 4 attempts" `T.isInfixOf` msgA
  assert "Z1e every transient attempt resumed the stored session" $ modesA == replicate 4 "resumed"
  -- The rule: an interruption is retried once, in a fresh session.
  rootB <- tempRoot "free-agent-interrupt-recover"
  (resB, nB, modesB) <- oneTurn rootB (interruptedOnceScript (rootB </> "invocations"))
  assert ("Z1f an interrupted batch is retried once, not four times (invocations " <> show nB <> ")") (nB == 2)
  assert "Z1g the recovery does not resume: the second invocation is fresh" $ modesB == ["resumed", "fresh"]
  assert "Z1h the interrupted turn settles from the recovery" $
    case resB of
      Right (TurnResponse r) -> "recovered" `T.isInfixOf` r
      _ -> False
  sessionGone <- fmap not (doesFileExist (rootB </> "session"))
  assert "Z1i the poisoned session is dropped, so no later turn resumes it" sessionGone
  -- The cold path: no stored session to resume, so the fresh ladder is the
  -- only recovery there is. This was fatal on the first exit 130.
  rootI <- tempRoot "free-agent-interrupt-cold"
  (resI, nI, modesI) <- oneTurnCold rootI (interruptedOnceScript (rootI </> "invocations"))
  assert ("Z1m a cold turn interrupted once recovers in a fresh session (invocations " <> show nI <> ")") (nI == 2)
  assert "Z1n and both invocations were fresh, never a resume" $ modesI == ["fresh", "fresh"]
  assert "Z1o the cold recovery settles the turn" $
    case resI of
      Right (TurnResponse r) -> "recovered" `T.isInfixOf` r
      _ -> False
  rootJ <- tempRoot "free-agent-interrupt-cold-twice"
  (resJ, nJ, modesJ) <- oneTurnCold rootJ (interruptedScript (rootJ </> "invocations"))
  let msgJ = message resJ
  assert ("Z1p a cold turn interrupted twice stops at two fresh invocations (invocations " <> show nJ <> ")") (nJ == 2)
  assert "Z1q both were fresh, and it never spun" $ modesJ == ["fresh", "fresh"]
  assert ("Z1r and the error names the interruption, not the session: " <> T.unpack (T.take 100 msgJ)) $
    "was interrupted on a fresh session" `T.isInfixOf` msgJ
      && not ("resumed session failed" `T.isInfixOf` msgJ)
  -- Bounded: an interruption that repeats is not retried again.
  rootC <- tempRoot "free-agent-interrupt-twice"
  (resC, nC, modesC) <- oneTurn rootC (interruptedScript (rootC </> "invocations"))
  let msgC = message resC
  assert ("Z1j a repeated interruption stops after one resumed and two fresh invocations (invocations " <> show nC <> ")") (nC == 3)
  assert "Z1k and the recovery never resumes" $ take 2 modesC == ["resumed", "fresh"] && last modesC == "fresh"
  assert ("Z1l the error names the interruption and the failed recovery: " <> T.unpack (T.take 110 msgC)) $
    "was interrupted on a fresh session" `T.isInfixOf` msgC
      && "not resuming" `T.isInfixOf` msgC
      && not ("resumed session failed" `T.isInfixOf` msgC)
  -------------------------------------------------------------------------
  -- Z2 · the failed delivery
  -------------------------------------------------------------------------
  putStrLn "the failed delivery (Z2)"
  -- A batch that fails twice is relayed and skipped; new mail is still served.
  rootD <- tempRoot "free-agent-skip-batch"
  writeCursor rootD "seat" 0
  _ <- postLocal rootD (mkPost "human" ["seat"] "first")
  seenD <- newIORef ([] :: [Text])
  attemptsD <- newIORef (0 :: Int)
  doneD <- newEmptyMVar
  grantD <- newDeliveryGrant 100
  let handlerD storedBatch = do
        modifyIORef' seenD (++ map (body . stamped) storedBatch)
        n <- readIORef attemptsD
        writeIORef attemptsD (n + 1)
        if n < 2 then pure (failingTurn "backend down") else pure (settledTurn [])
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootD Nothing grantD handlerD >> putMVar doneD ())
  skippedD <- awaitRelays rootD 1
  _ <- postLocal rootD (mkPost "human" ["seat"] "second")
  answeredD <- awaitCursor rootD "seat" 3
  aliveD <- fmap (isNothing) (timeout 2000000 (takeMVar doneD))
  attempts <- readIORef attemptsD
  seen <- readIORef seenD
  assert ("Z2a a failing batch costs two attempts (attempts " <> show attempts <> ")") (attempts == 3)
  assert "Z2b the failed batch is relayed as a skip" skippedD
  assert "Z2c the seat survives a failed delivery" aliveD
  assert ("Z2d later mail is still answered after the skip (seen " <> show seen <> ")") $
    answeredD && "second" `elem` seen && length (filter (== "first") seen) == 2
  -- An interruption cannot duplicate publication: a delivery that fails once
  -- and settles on the retry publishes one reply for the batch, once.
  rootK <- tempRoot "free-agent-retry-single-publish"
  writeCursor rootK "seat" 0
  _ <- postLocal rootK (mkPost "human" ["seat"] "work")
  attemptsK <- newIORef (0 :: Int)
  doneK <- newEmptyMVar
  grantK <- newDeliveryGrant 100
  let handlerK _ = do
        n <- readIORef attemptsK
        writeIORef attemptsK (n + 1)
        if n == 0
          then pure (failingTurn "backend down")
          else pure (settledTurn [mkPost "seat" ["human"] "one reply"])
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootK Nothing grantK handlerK >> putMVar doneK ())
  committed <- awaitCursor rootK "seat" 1
  logK <- linesOf (rootK </> "log.jsonl")
  assert "Z2m a batch that fails once and settles publishes exactly one reply" $
    committed && length (filter (T.isInfixOf "one reply") logK) == 1
  -- Explicit cancellation still halts, with the mail left behind the cursor.
  rootE <- tempRoot "free-agent-cancel-halts"
  writeCursor rootE "seat" 0
  _ <- postLocal rootE (mkPost "human" ["seat"] "go")
  doneE <- newEmptyMVar
  grantE <- newDeliveryGrant 100
  callsE <- newIORef (0 :: Int)
  let handlerE _ = modifyIORef' callsE (+ 1) >> pure cancelledTurn
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootE Nothing grantE handlerE >> putMVar doneE ())
  exitedE <- fmap (isJust) (timeout 5000000 (takeMVar doneE))
  cursorE <- readCursor rootE "seat"
  assert "Z2e explicit cancellation still halts the loop" exitedE
  assert ("Z2f and leaves the mail behind the cursor (cursor " <> show cursorE <> ")") (cursorE == 0)
  -- A decided quiet still halts, and commits the batch it decided on.
  rootF <- tempRoot "free-agent-standdown-halts"
  writeCursor rootF "seat" 0
  _ <- postLocal rootF (mkPost "human" ["seat"] "go")
  doneF <- newEmptyMVar
  grantF <- newDeliveryGrant 100
  let handlerF _ = pure (settledTurn [mkPost "seat" [] (markGlyph StandDown)])
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootF Nothing grantF handlerF >> putMVar doneF ())
  exitedF <- fmap (isJust) (timeout 5000000 (takeMVar doneF))
  cursorF <- readCursor rootF "seat"
  assert "Z2g a StandDown reply still halts the loop" exitedF
  assert ("Z2h and commits the batch it decided on (cursor " <> show cursorF <> ")") (cursorF == 1)
  -- Three consecutive skips mean a broken seat, not broken mail: it stands down.
  rootG <- tempRoot "free-agent-skip-three"
  writeCursor rootG "seat" 0
  doneG <- newEmptyMVar
  grantG <- newDeliveryGrant 100
  let handlerG _ = pure (failingTurn "backend down")
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootG Nothing grantG handlerG >> putMVar doneG ())
  okA <- postThenSkip rootG "one" 1
  okB <- postThenSkip rootG "two" 2
  okC <- postThenSkip rootG "three" 3
  let threeSkips = okA && okB && okC
  exitedG <- fmap (isJust) (timeout 5000000 (takeMVar doneG))
  assert "Z2i three consecutive skips stand the seat down" (threeSkips && exitedG)
  -- The already-drained batch: a halt decided by one batch admits no work from
  -- a batch drained behind it (the ~/net race: 103s and 768s of post-halt work).
  rootH <- tempRoot "free-agent-halt-fence"
  writeCursor rootH "seat" 0
  _ <- postLocal rootH (mkPost "human" ["seat"] "first")
  seenH <- newIORef ([] :: [Text])
  doneH <- newEmptyMVar
  grantH <- newDeliveryGrant 100
  let handlerH storedBatch = do
        modifyIORef' seenH (++ map (body . stamped) storedBatch)
        -- Append the next batch while this one still holds the drain lock.
        void (postLocal rootH (mkPost "human" ["seat"] "late"))
        threadDelay 400000
        pure (settledTurn [mkPost "seat" [] (markGlyph StandDown)])
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootH Nothing grantH handlerH >> putMVar doneH ())
  exitedH <- fmap (isJust) (timeout 6000000 (takeMVar doneH))
  threadDelay 500000
  seenH' <- readIORef seenH
  cursorH <- readCursor rootH "seat"
  assert ("Z2j the batch drained behind the halt never reaches the handler (seen " <> show seenH' <> ")") $
    seenH' == ["first"]
  assert "Z2k the halt decided by one batch stops the loop" exitedH
  assert ("Z2l and it leaves the cursor at the decided batch (cursor " <> show cursorH <> ")") (cursorH == 1)
  -------------------------------------------------------------------------
  -- Z3 · the tool-iteration ceiling
  -------------------------------------------------------------------------
  putStrLn "the tool-iteration ceiling (Z3)"
  -- The classification is the CLI's own verdict, not its prose.
  assert "Z3a exit 2 with a partial verdict is a ceiling" $
    cliCeiling (ExitFailure 2) ("partial work\nturn_partial:true\nturn_exit_reason:max_iterations_reached(500)")
  assert "Z3b exit 2 without the partial verdict is an ordinary failure" $
    not (cliCeiling (ExitFailure 2) "provider error\nturn_partial:false\nturn_exit_reason:provider_error")
  assert "Z3c an interruption, a plain failure and a success are never ceilings" $
    not (cliCeiling (ExitFailure 130) "turn_partial:true")
      && not (cliCeiling (ExitFailure 1) "turn_partial:true")
      && not (cliCeiling ExitSuccess "turn_partial:true")
  -- A stopped turn is not replayed: one invocation, and the stop is typed.
  rootL <- tempRoot "free-agent-ceiling-one"
  let reportL = rootL </> "usage.json"
      cliL = stubCliWith (rootL </> "session") (rootL </> "modes") (Just reportL) (ceilingScript (rootL </> "invocations") reportL)
  (resL, nL, modesL) <- oneTurnClassified rootL cliL
  assert ("Z3d a ceiling costs one invocation, not four (invocations " <> show nL <> ")") (nL == 1)
  assert "Z3e and it never retries: the one invocation was the resumed session" (modesL == ["resumed"])
  assert "Z3f the stop reaches the runner as a typed ceiling, not an IO failure" (isCeiling resL)
  -- The same exit code from a failed turn keeps the transient ladder.
  rootM <- tempRoot "free-agent-ceiling-failed-turn"
  let reportM = rootM </> "usage.json"
      cliM = stubCliWith (rootM </> "session") (rootM </> "modes") (Just reportM) (failedTurnScript (rootM </> "invocations") reportM)
  (resM, nM, _) <- oneTurnClassified rootM cliM
  assert ("Z3g a failed exit-2 turn still retries 1 + 3 times (invocations " <> show nM <> ")") (nM == 4)
  assert "Z3h and it stays an ordinary failure, not a ceiling" (not (isCeiling resM))
  -- The batch: committed once, never replayed, and the seat lives.
  rootN <- tempRoot "free-agent-ceiling-commit"
  writeCursor rootN "seat" 0
  _ <- postLocal rootN (mkPost "human" ["seat"] "first")
  callsN <- newIORef (0 :: Int)
  doneN <- newEmptyMVar
  grantN <- newDeliveryGrant 100
  let handlerN _ = do
        n <- readIORef callsN
        writeIORef callsN (n + 1)
        if n == 0 then pure (ceilingTurn "max_iterations_reached(500)") else pure (settledTurn [])
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootN Nothing grantN handlerN >> putMVar doneN ())
  committedN <- awaitCursor rootN "seat" 1
  _ <- postLocal rootN (mkPost "human" ["seat"] "second")
  answeredN <- awaitCursor rootN "seat" 3
  aliveN <- fmap isNothing (timeout 2000000 (takeMVar doneN))
  calls <- readIORef callsN
  logN <- linesOf (rootN </> "log.jsonl")
  assert ("Z3i the ceiling batch is committed and only later mail is presented (handler calls " <> show calls <> ")") (committedN && calls == 2)
  assert "Z3j its senders are told the batch closed without a reply" (any (T.isInfixOf "tool-iteration ceiling") logN)
  assert "Z3k and the seat answers the next batch" (answeredN && aliveN)
  -- Publication that happened outside the runner is not duplicated by a replay.
  rootO <- tempRoot "free-agent-ceiling-single-publish"
  writeCursor rootO "seat" 0
  _ <- postLocal rootO (mkPost "human" ["seat"] "work")
  callsO <- newIORef (0 :: Int)
  doneO <- newEmptyMVar
  grantO <- newDeliveryGrant 100
  let handlerO _ = do
        modifyIORef' callsO (+ 1)
        void (postLocal rootO (mkPost "seat" ["human"] "published outside the runner"))
        pure (ceilingTurn "max_iterations_reached(500)")
  _ <- forkIO (runAgentLoop "seat" ["seat"] rootO Nothing grantO handlerO >> putMVar doneO ())
  committedO <- awaitCursor rootO "seat" 1
  callsO' <- readIORef callsO
  logO <- linesOf (rootO </> "log.jsonl")
  assert "Z3l a reply published outside the runner is not duplicated on a ceiling" $
    committedO && callsO' == 1 && length (filter (T.isInfixOf "published outside the runner") logO) == 1

-- | Post one more batch and wait until the seat has relayed n skips in total.
postThenSkip :: FilePath -> Text -> Int -> IO Bool
postThenSkip root label n = do
  _ <- postLocal root (mkPost "human" ["seat"] label)
  awaitRelays root n
