{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Process ports: framing, death-as-content, merged stdio, and doubles.
module Axioma.Ports
  ( portsTopic,
  )
where

import Axioma.Common (assert)
import Circuit.Category (K (..))
import Circuit.Machine (Body (Body), Moore, Poles (commit, emit))
import Circuit.Syntax (eval)
import Circuit.Tensor (unConst)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.StdPorts
import Free.Agent.StdPorts qualified as Ports
import System.Exit (ExitCode (..))
import System.Process (getProcessExitCode)
import System.Timeout (timeout)

default (Text)

portsTopic :: IO ()
portsTopic = do
  putStrLn "death honesty"

  -- S1: frame extraction is invariant under byte-chunk boundaries.
  do
    let runFrames :: ProcMarks -> [Maybe BS.ByteString] -> [BS.ByteString]
        runFrames marks inputs = concat (go (BS.empty, []) inputs)
          where
            sys = frameAgent marks id
            go _ [] = []
            go st (i : is) =
              let (o, st') = runCell sys st i
               in o : go st' is
        whole marks s = runFrames marks [Just s, Nothing]
        cuts2 s = [[BS.take i s, BS.drop i s] | i <- [0 .. BS.length s]]
        cuts3 s =
          [ [a, b, c]
          | i <- [0 .. BS.length s],
            let (a, bc) = BS.splitAt i s,
            j <- [i .. BS.length s],
            let (b, c) = BS.splitAt (j - i) bc
          ]
        samples =
          [ (lineMarks, "a\nb\n"),
            (hermesMarks, "output\n❯ rest"),
            (sseMarks, "event: e\ndata: d\n\nevent: e2\n")
          ]
        properPrefixes (ProcMarks ms) =
          [(m1, m2) | m1 <- ms, m2 <- ms, m1 /= m2, m1 `T.isPrefixOf` m2]
        colliding = ProcMarks ["\n\n", "\n"]
    assert "S1 shipped grammars have no proper-prefix mark pairs" $
      all ((null . properPrefixes) . fst) samples
    assert "S1 frames are invariant under every 2- and 3-cut" $
      all
        (\(marks, s) -> all (\cs -> runFrames marks (map Just cs ++ [Nothing]) == whole marks s) (cuts2 s ++ cuts3 s))
        samples
    assert "S1 the prefix collision breaks the cut (the guard is load-bearing)" $
      runFrames colliding [Just "a\n", Just "\nb", Nothing]
        /= runFrames colliding [Just "a\n\nb", Nothing]

  -- S2: death is content — a dead child delivers its exit code.
  do
    pp <- runK (eval (openStdPorts id id (shellConfig "exit 3"))) ()
    m <- timeout 5_000_000 (runK (emit (stdOut pp)) ())
    assert "S2 a dead child delivers the terminal frame with exit 3" $
      m == Just (Exited (ExitFailure 3))
    stdClose pp

  -- S3: close reaps — terminate, wait, no zombie.
  do
    pp <- runK (eval (openStdPorts id id (shellConfig "sleep 30"))) ()
    mClose <- timeout 5_000_000 (void (stdClose pp))
    assert "S3 stdClose terminates and reaps within the bound" $ isJust mClose
    case stdProcess pp of
      Just ph -> do
        mc <- getProcessExitCode ph
        assert "S3 the child is reaped: getProcessExitCode returns Just" $ isJust mc
      Nothing -> assert "S3 openStdPorts carries the process handle" False

  -- S5: a dribbled frame arrives whole, exactly once.
  do
    pp <- runK (eval (openStdPorts id id (shellConfig "printf 'par'; sleep 0.2; printf 'tial\\n'; exec sleep 5"))) ()
    f1 <- timeout 5_000_000 (runK (emit (stdOut pp)) ())
    assert "S5 a dribbled frame arrives whole, exactly once" $
      f1 == Just (Payload "partial")
    stdClose pp

  -- S6: honest doubles — block, die, zero frames, two frames.
  do
    pp <- Ports.echo pure :: IO (StdPorts String String ())
    mBlocked <- timeout 200_000 (runK (emit (stdOut pp)) ())
    assert "S6 a read before any write blocks" $ mBlocked == Nothing
    runK (commit (stdIn pp)) "hi"
    f <- runK (emit (stdOut pp)) ()
    assert "S6 the echo replies after the write" $ f == Payload "hi"

    sp <- scriptedDouble [Quiet, Reply2 (++ "1") (++ "2"), Die (ExitFailure 7)]
    runK (commit (stdIn sp)) "a"
    mq <- timeout 200_000 (runK (emit (stdOut sp)) ())
    assert "S6 Quiet answers nothing (the read blocks)" $ mq == Nothing
    runK (commit (stdIn sp)) "b"
    g1 <- runK (emit (stdOut sp)) ()
    g2 <- runK (emit (stdOut sp)) ()
    assert "S6 Reply2 emits two frames for one token" $
      [g1, g2] == [Payload "b1", Payload "b2"]
    runK (commit (stdIn sp)) "c"
    g3 <- runK (emit (stdOut sp)) ()
    assert "S6 Die delivers the terminal frame once" $ g3 == Exited (ExitFailure 7)
    runK (commit (stdIn sp)) "d"
    mAfter <- timeout 200_000 (runK (emit (stdOut sp)) ())
    assert "S6 reads block after death" $ mAfter == Nothing

  putStrLn "merged stdio (S4)"
  do
    pp <-
      runK
        ( eval
            ( openStdPorts
                id
                id
                (shellConfig "printf 'b\\n' >&2; printf 'a\\n'; sleep 1") {procMergeStderr = True}
            )
        )
        ()
    f1 <- timeout 5_000_000 (runK (emit (stdOut pp)) ())
    assert "S4 merged: stderr rides the same pipe, ordered before stdout" $
      f1 == Just (Payload "b")
    f2 <- timeout 5_000_000 (runK (emit (stdOut pp)) ())
    assert "S4 merged: the stdout follow-up arrives second" $
      f2 == Just (Payload "a")
    mErr <- timeout 200_000 (runK (emit (stdErr pp)) ())
    assert "S4 merged: the stderr pole has nothing of its own" $ mErr == Nothing
    stdClose pp

shellConfig :: String -> ProcConfig
shellConfig script =
  ProcConfig
    { procCommand = "sh",
      procArgs = ["-c", script],
      procWorkingDir = ".",
      procEnv = Nothing,
      procMarks = lineMarks,
      procMergeStderr = False
    }

runCell :: Moore (,) s (->) a b -> s -> a -> (b, s)
runCell (Body _ step output) state input =
  let next = step (state, input)
   in (unConst (output next), next)
