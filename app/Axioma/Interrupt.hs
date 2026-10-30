{-# LANGUAGE OverloadedStrings #-}

-- | The interrupted handler (Z1): an interrupted turn is not retried.
--
-- Measured on seat @yang@, 31 Oct 2026: the handler made four invocations of
-- the same session — 1,196s, 3,431s, 4,400s and 5,550s — every one resuming,
-- every one ending @Operation interrupted: waiting for model response (N.Ns
-- elapsed)@ and exiting 130, and the public error reported the session as the
-- fault.  The interruption was the first answer repeated four times.
--
-- The witness runs the real delivery path against a stub CLI, because the
-- claim is about how many times the handler invokes: a shell script counts its
-- own invocations and the assertion is the count.
module Axioma.Interrupt
  ( interruptTopic,
  )
where

import Axioma.Common (assert, tempRoot)
import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Cli
  ( Cli (..),
    StderrPolicy (StderrMerge),
    cliDeliverTo,
    cliInterrupted,
  )
import Free.Agent.Delivery (Turn (..), TurnResult)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

-- | A CLI whose single invocation is the given shell script.
stubCli :: FilePath -> String -> Cli
stubCli sessionFile script =
  Cli
    { cliCommand = "/bin/sh",
      cliArgv = \_ _ -> ["-c", script],
      cliStdin = const "",
      cliSessionFile = sessionFile,
      cliSessionId = const Nothing,
      cliStale = \_ _ -> False,
      cliExhausted = \_ _ -> False,
      cliScrub = id,
      cliStderr = StderrMerge,
      cliStderrTee = Nothing
    }

-- | Run one delivery against a stub and report (result, invocations counted).
oneTurn :: FilePath -> String -> IO (Either IOException (TurnResult Text), Int)
oneTurn root script = do
  let counter = root </> "invocations"
      session = root </> "session"
      transcript = root </> "transcript.jsonl"
  TIO.writeFile session "stub-session\n"
  turn <- cliDeliverTo (stubCli session script) transcript 0 "prompt"
  result <- try (awaitTurn turn)
  seen <- doesFileExist counter
  n <- if seen then length . filter (not . T.null) . T.lines <$> TIO.readFile counter else pure 0
  pure (result, n)

-- | The measurement, as shell: append one line to the counter, then fail the
-- way the interrupted hermes does.
interruptedScript :: FilePath -> String
interruptedScript counter =
  "echo invoked >> "
    <> counter
    <> "; echo 'Operation interrupted: waiting for model response (4.8s elapsed).'; exit 130"

-- | A transient failure: the retry loop's own case, unchanged by this rule.
transientScript :: FilePath -> String
transientScript counter = "echo invoked >> " <> counter <> "; echo 'transient backend error'; exit 1"

interruptTopic :: IO ()
interruptTopic = do
  putStrLn "the interrupted handler (Z1)"
  assert "Z1a exit 130 and the interruption words are both interruptions" $
    cliInterrupted (ExitFailure 130) ""
      && cliInterrupted ExitSuccess "Operation interrupted: waiting for model response (4.8s elapsed)."
  assert "Z1b an ordinary failure is not an interruption" $
    not (cliInterrupted (ExitFailure 1) "transient backend error")
      && not (cliInterrupted (ExitFailure 143) "killed")
  -- The control: a transient failure still makes 1 + 3 invocations, which is
  -- also the proof that the resumed-session path was taken below.
  rootA <- tempRoot "free-agent-interrupt-transient"
  (resA, nA) <- oneTurn rootA (transientScript (rootA </> "invocations"))
  let msgA = case resA of
        Left e -> T.pack (show e)
        Right _ -> ""
  assert ("Z1c a transient failure still retries 1 + 3 times (invocations " <> show nA <> ")") (nA == 4)
  assert ("Z1d and reports the session as failed, as before: " <> T.unpack (T.take 60 msgA)) $
    "resumed session failed after 4 attempts" `T.isInfixOf` msgA
  -- The rule: the same stub, interrupted instead, is invoked once.
  rootB <- tempRoot "free-agent-interrupt-once"
  (resB, nB) <- oneTurn rootB (interruptedScript (rootB </> "invocations"))
  let msgB = case resB of
        Left e -> T.pack (show e)
        Right _ -> ""
  assert ("Z1e an interrupted turn is invoked once, not four times (invocations " <> show nB <> ")") (nB == 1)
  assert ("Z1f and the error names the interruption: " <> T.unpack (T.take 90 msgB)) $
    "was interrupted mid-turn" `T.isInfixOf` msgB && "exit ExitFailure 130" `T.isInfixOf` msgB
  assert "Z1g and the error no longer blames the session" $
    not ("resumed session failed" `T.isInfixOf` msgB)
