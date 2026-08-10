{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Circuit (Ends (..), close)
import Circuit.Agent (Agent, AgentSeat (..), Bag, Post (..), Shard, awaitA, branchesByIndex, coneByIndex, raceA, replyTo, runAgentShard, sortNub, synthesis, tape, toBag)
import Circuit.Agent.Framing (Stamped (..), frameStored, parseLine, parseTimeText, stamp, stamped)
import Circuit.Agent.Mark (Mark (..), isEscalate, markGlyph, markOf)
import Circuit.Agent.Tensor
  ( awaitShard,
    fanInShard,
    fanOutShard,
    raceShard,
    silentShard,
  )
import Circuit.Category (Category (id, (.)), ObDict (..))
import Circuit.Channel (Strength (..), Traced (..))
import Circuit.Layer ((:~>))
import Circuit.Poly (Mono, System (..), monoDir)
import Circuit.Poly.Process (iterateSystem, runSystem)
import Circuit.Poly.StringDiagram (SDiagram (..))
import Circuit.Poly.StringDiagram.Hyper (BoundaryEnd (..), HyperGraph (..), PortDir (..), PortEnd (..), Wire (..), hyperEquiv, normalise)
import Circuit.Process (delay, register, scan)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (MVar, forkIO, killThread, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Monad (when)
import Control.Monad.State (State, StateT, runState, runStateT)
import Data.IORef (newIORef, writeIORef)
import Data.List (inits)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (closeBus, openBus, postLocal, runSeatBus)
import Free.Agent.Bus.File (Flow (..), tailLog)
import Free.Agent.BusStats (Classification (..), Rules (..), SliceMode (..), Stats (..), classify, computeStats, defaultRules, isDoneClaim, slicePosts)
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliQuery, parseSessionId)
import Free.Agent.Derivation (chaseLog, dParents, derivation, valid)
import Free.Agent.Diagram (diagramStep, diagramSteps, liftProcess, meetingSkeleton, mooreProcess, skeletonLabels)
import Free.Agent.Host (BodyMode (..), Host (..), cliHost, hostShard, mkHost, processHost)
import Free.Agent.Hyper (both, braidP, copy2, copyP, merge2, mergeP, silent)
import Free.Agent.Layer (bindFreeAgent, runFreeAgent)
import Free.Agent.Meeting (AgentBox (..), meetLog, quoter, unchanged)
import Free.Agent.Pipeline
  ( Pipeline,
    broadcast,
    filterP,
    forName,
    fromName,
    mapP,
    pipelineShard,
    routeBy,
    routeP,
    routeTo,
    runPipeline,
  )
import Free.Agent.Pipeline qualified as P
import Free.Agent.Seat
  ( FreeSeat (..),
    SeatBehaviour,
    awaitSeat,
    bundleSeat,
    fanInSeat,
    fanOutSeat,
    forkSeat,
    hostSeat,
    interpretSeat,
    interpretSeatA,
    interpretSeatS,
    pipelineSeat,
    raceSeat,
    runAgentSBox,
    silentSeat,
  )
import Free.Agent.Syntax (FreeAgent (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (getExecutablePath, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import Prelude hiding (id, (.))

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

mkPost :: Text -> [Text] -> Text -> Post Text
mkPost a ds = Post a ds []

-- | The 'SBox' labels of a meeting skeleton, in composition order (meeting
-- skeletons contain only boxes, spiders and swaps besides wires).
boxLabels :: SDiagram -> [String]
boxLabels = filter (`notElem` ["spider", "swap"]) . skeletonLabels

-- | Count the spiders of a given arity in a diagram tree.
countSpiders :: (Int, Int) -> SDiagram -> Int
countSpiders (m, n) (SSpider m' n') = if m == m' && n == n' then 1 else 0
countSpiders mn (SBeside f g) = countSpiders mn f + countSpiders mn g
countSpiders mn (SThenD f g) = countSpiders mn f + countSpiders mn g
countSpiders _ _ = 0

-- | Close a same-type shard once under State.
closeShard :: Shard (State s) a a -> a -> s -> (a, s)
closeShard sh x s0 =
  runState (runKleisli (close (conjoint sh) (companion sh)) x) s0

-- | Close a same-type shard once under StateT IO.
closeShardIO :: Shard (StateT s IO) a a -> a -> s -> IO (a, s)
closeShardIO sh x s0 =
  runStateT (runKleisli (close (conjoint sh) (companion sh)) x) s0

main :: IO ()
main = do
  putStrLn "free-agent oracle tests"

  -------------------------------------------------------------------------
  -- Layer laws
  -------------------------------------------------------------------------
  putStrLn "Layer laws"

  do
    let f :: Int -> Int
        f = (+ 1)
        g :: Int -> Int
        g = (* 2)
        h :: Int -> Int
        h = subtract 3
        freeF = Lift f :: FreeAgent (->) Int Int
        freeG = Lift g :: FreeAgent (->) Int Int
        freeH = Lift h :: FreeAgent (->) Int Int
        freeGF = freeG `Compose` freeF
        freeHG = freeH `Compose` freeG
        leftAssoc = freeH `Compose` freeGF
        rightAssoc = freeHG `Compose` freeF
        freeId = Lift id :: FreeAgent (->) Int Int
    assert "run (Lift f) == f" $ runFreeAgent freeF 5 == 6
    assert "run (Compose (Lift g) (Lift f)) == g . f" $ runFreeAgent freeGF 5 == 12
    assert "left unit: run (Compose f id) == run f" $
      runFreeAgent (freeF `Compose` freeId) 5 == runFreeAgent freeF 5
    assert "right unit: run (Compose id f) == run f" $
      runFreeAgent (freeId `Compose` freeF) 5 == runFreeAgent freeF 5
    assert "Compose associative under run" $
      runFreeAgent leftAssoc 5 == runFreeAgent rightAssoc 5

  do
    let f :: Int -> Int
        f = (+ 1)
        g :: Int -> Int
        g = (* 2)
        freeF = Lift f :: FreeAgent (->) Int Int
        freeG = Lift g :: FreeAgent (->) Int Int
        freeGF = freeG `Compose` freeF
        target = bindFreeAgent id id freeF :: (->) Int Int
        targetGF = bindFreeAgent id id freeGF :: (->) Int Int
    assert "bind/unit: bind id (Lift f) == f" $ target 5 == 6
    assert "bind preserves Compose" $ targetGF 5 == 12

  do
    let f :: Int -> Int
        f = (+ 1)
        g :: Int -> Int
        g = (* 2)
        freeF = Lift f :: FreeAgent (->) Int Int
        freeG = Lift g :: FreeAgent (->) Int Int
        freeGF = freeG `Compose` freeF
        toMaybe :: (->) :~> Kleisli Maybe
        toMaybe h = Kleisli (Just . h)
        targetF = bindFreeAgent (\_ -> ObDict) toMaybe freeF :: Kleisli Maybe Int Int
        targetGF = bindFreeAgent (\_ -> ObDict) toMaybe freeGF :: Kleisli Maybe Int Int
    assert "bindFreeAgent folds into a discrete target (Kleisli Maybe)" $
      runKleisli targetF 5 == Just 6
    assert "bindFreeAgent preserves Compose into Kleisli Maybe" $
      runKleisli targetGF 5 == Just 12

  -------------------------------------------------------------------------
  -- Pipeline laws (on the pure fold)
  -------------------------------------------------------------------------
  putStrLn "Pipeline laws"

  do
    let f = filterP even :: Pipeline Int Int
        g = mapP (* 3) :: Pipeline Int Int
        h = routeP (\n -> [n, n + 1]) :: Pipeline Int Int
        leftA = h . (g . f)
        rightA = (h . g) . f
        xs = [1 .. 6 :: Int]
        pid = id :: Pipeline Int Int
    assert "pipeline Compose associative under runPipeline" $
      runPipeline leftA xs == runPipeline rightA xs
    assert "filter then map" $
      runPipeline (g . f) xs == [6, 12, 18]
    assert "pipeline left unit: runPipeline (f . id) == runPipeline f" $
      runPipeline (f . pid) xs == runPipeline f xs
    assert "pipeline right unit: runPipeline (id . f) == runPipeline f" $
      runPipeline (pid . f) xs == runPipeline f xs
    assert "pipeline id is identity" $
      runPipeline pid xs == xs

  -------------------------------------------------------------------------
  -- Pipeline round-trip into Shard (State [Post Text]) [Post Text] [Post Text]
  -------------------------------------------------------------------------
  putStrLn "Pipeline round-trip"

  do
    let p :: Pipeline (Post Text) (Post Text)
        -- Category-style: map after filter
        p = mapP (\x -> x {body = "map:" <> body x}) `P.Compose` filterP (\x -> body x /= "noise")
        posts =
          [ mkPost "human" ["bot"] "hello",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "world"
          ]
        sh :: Shard (State [Post Text]) [Post Text] [Post Text]
        sh = pipelineShard p
        (outs, st) = closeShard sh posts []
    assert "runPipeline agrees with shard emit" $
      map body outs == map body (runPipeline p posts)
    assert "noise filtered and prefix added" $
      map body outs == ["map:hello", "map:world"]
    assert "shard buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Host shard round-trip
  -------------------------------------------------------------------------
  putStrLn "Host shard round-trip"

  do
    let h = mkHost "echo" (pure . map ("echo:" <>))
        sh :: Shard (State [Post Text]) [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi there"
        (outs, st) = closeShard sh [p] []
    assert "host replies to sender" $
      map body outs == ["echo:hi", "echo:there"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
        && all (\x -> thread x == []) outs
    assert "host buffer cleared after emit" $ st == []

  do
    let h = mkHost "echo" (pure . map ("echo:" <>))
        sh :: Shard (State [Post Text]) [Post Text] [Post Text]
        sh = hostShard h
        posts =
          [ mkPost "human" ["echo"] "one two",
            mkPost "human" ["echo"] "three"
          ]
        (outs, st) = closeShard sh posts []
    assert "host emits one reply batch per committed post" $
      map body outs == ["echo:one", "echo:two", "echo:three"]
        && length outs == 3
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
    assert "multi-post host buffer cleared after emit" $ st == []

  do
    let h = (mkHost "echo" (pure . map ("echo:" <>))) {hostBodyMode = BodyLines}
        sh :: Shard (State [Post Text]) [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi\nthere"
        (outs, st) = closeShard sh [p] []
    assert "host BodyLines splits on newlines" $
      map body outs == ["echo:hi", "echo:there"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
    assert "BodyLines host buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- FreeSeat multi-stage close: pipeline + host composed as seat terms
  -------------------------------------------------------------------------
  putStrLn "FreeSeat multi-stage close"

  do
    let p :: Pipeline (Post Text) (Post Text)
        p = mapP (\x -> x {body = "map:" <> body x}) `P.Compose` filterP (\x -> body x /= "noise")
        h = mkHost "echo" (pure . map ("echo:" <>))
        seat :: FreeSeat
        seat = SeatCompose (hostSeat h) (pipelineSeat p)
        sh :: Shard (StateT [Post Text] IO) [Post Text] [Post Text]
        sh = interpretSeat seat
        posts =
          [ mkPost "human" ["bot"] "hello world",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "again"
          ]
    (outs, st) <- closeShardIO sh posts []
    assert "free seat composed pipeline then host" $
      map body outs == ["echo:map:hello", "echo:world", "echo:map:again"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
        && all (\x -> thread x == []) outs
    assert "free seat buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Route-by-name helpers
  -------------------------------------------------------------------------
  putStrLn "Route-by-name helpers"

  do
    let posts =
          [ mkPost "human" ["bot"] "one",
            mkPost "human" ["bot"] "two"
          ]
        p = routeTo "router" :: Pipeline (Post Text) (Post Text)
    assert "routeTo sets single recipient" $
      all (\x -> to x == ["router"]) (runPipeline p posts)

  do
    let posts =
          [ mkPost "human" [] "one",
            mkPost "human" [] "two"
          ]
        p = broadcast ["a", "b"] :: Pipeline (Post Text) (Post Text)
        routed = runPipeline p posts
    assert "broadcast sets multiple recipients" $
      all (\x -> to x == ["a", "b"]) routed && length routed == 2

  do
    let p = routeBy (\x -> if body x == "ops" then ["ops"] else ["general"])
        posts =
          [ mkPost "human" [] "ops",
            mkPost "human" [] "chat"
          ]
        routed = runPipeline p posts
    assert "routeBy routes by predicate" $
      case routed of
        [opsPost, generalPost] -> to opsPost == ["ops"] && to generalPost == ["general"]
        _ -> False

  -------------------------------------------------------------------------
  -- Real-host sketch: external process
  -------------------------------------------------------------------------
  putStrLn "Real-host sketch"

  do
    let h = processHost "shell" "echo" ["hello"]
        sh :: Shard (StateT [Post Text] IO) [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["shell"] "ignored"
    (outs, st) <- closeShardIO sh [p] []
    assert "process host runs echo and replies" $
      map body outs == ["hello ignored"] && all (\x -> to x == ["human"]) outs
    assert "process host buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Addressed filters
  -------------------------------------------------------------------------
  putStrLn "Addressed filters"

  do
    let posts =
          [ mkPost "alice" ["bot"] "a",
            mkPost "bob" ["other"] "b",
            mkPost "carol" ["bot", "x"] "c"
          ]
    assert "forName keeps deliversTo name" $
      map body (runPipeline (forName "bot") posts) == ["a", "c"]
    assert "fromName keeps sender" $
      map body (runPipeline (fromName "bob") posts) == ["b"]

  -------------------------------------------------------------------------
  -- FreeSeat Compose associative under interpretSeat close
  -------------------------------------------------------------------------
  putStrLn "FreeSeat Compose assoc"

  do
    let p1 = forName "bot" :: Pipeline (Post Text) (Post Text)
        p2 = mapP (\x -> x {body = "m:" <> body x}) :: Pipeline (Post Text) (Post Text)
        p3 = routeTo "out" :: Pipeline (Post Text) (Post Text)
        leftA = SeatCompose (pipelineSeat p3) (SeatCompose (pipelineSeat p2) (pipelineSeat p1))
        rightA = SeatCompose (SeatCompose (pipelineSeat p3) (pipelineSeat p2)) (pipelineSeat p1)
        posts =
          [ mkPost "human" ["bot"] "hi",
            mkPost "human" ["skip"] "no"
          ]
    (outsL, _) <- closeShardIO (interpretSeat leftA) posts []
    (outsR, _) <- closeShardIO (interpretSeat rightA) posts []
    assert "SeatCompose associative under close" $
      outsL == outsR
        && map body outsL == ["m:hi"]
        && all (\x -> to x == ["out"]) outsL

  -------------------------------------------------------------------------
  -- Real process host sharing StateT IO buffer with pipeline stages
  -------------------------------------------------------------------------
  putStrLn "Real host in shared StateT IO buffer"

  do
    let p = forName "shell" :: Pipeline (Post Text) (Post Text)
        h = processHost "shell" "echo" ["ok"]
        seat :: FreeSeat
        seat = SeatCompose (hostSeat h) (pipelineSeat p)
        sh :: Shard (StateT [Post Text] IO) [Post Text] [Post Text]
        sh = interpretSeat seat
        posts =
          [ mkPost "human" ["shell"] "world",
            mkPost "human" ["skip"] "no"
          ]
    (outs, st) <- closeShardIO sh posts []
    assert "process host shares StateT IO buffer with pipeline" $
      map body outs == ["ok world"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "shell") outs
    assert "shared buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Real-host sketch: live CLI seat (fake binary, exact oracle)
  -------------------------------------------------------------------------
  putStrLn "CLI host sketch"

  do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "free-agent-cli-axioma"
        script = dir </> "fake-cli.sh"
        sf = dir </> "session"
    createDirectoryIfMissing True dir
    se <- doesFileExist sf
    when se (removeFile sf)
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo \"session_id: free-fake\"",
            "echo \"argv:$*\"",
            "printf 'stdin:'",
            "cat"
          ]
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliTranscript = Nothing
            }
        h = cliHost "fake" cli
        sh :: Shard (StateT [Post Text] IO) [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["fake"] "hello\nworld"
    (outs, st) <- closeShardIO sh [p] []
    assert "cli host preserves multi-line body verbatim" $
      case outs of
        [o] -> "stdin:hello\nworld" `T.isInfixOf` body o && to o == ["human"]
        _ -> False
    assert "cli host buffer cleared after emit" $ st == []
    stored <- TIO.readFile sf
    assert "cli host session id persisted" $ T.strip stored == "free-fake"
    (outs2, _) <- closeShardIO sh [p] []
    assert "cli host second close resumes stored session" $
      case outs2 of
        [o] -> "argv:--resume free-fake" `T.isInfixOf` body o
        _ -> False

  -------------------------------------------------------------------------
  -- Transcript oracle (B1): cliQuery appends a JSONL record when
  -- cliTranscript is set, and scrubbing the recorded raw reproduces the
  -- posted reply body.  Failure is silent — a missing directory does not
  -- prevent the seat from working.
  -------------------------------------------------------------------------
  putStrLn "Transcript oracle"

  do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "free-agent-transcript-axioma"
        script = dir </> "fake-cli.sh"
        sf = dir </> "session"
        transcriptLog = dir </> "transcripts" </> "fake.jsonl"
        badTranscriptLog = script </> "transcripts" </> "fake.jsonl"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-tid'",
            "echo 'hello from stderr' >&2",
            "printf 'stdin:'",
            "cat"
          ]
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliTranscript = Nothing
            }
    -- Part 1: transcript disabled → no file written, cliQuery still works
    result1 <- cliQuery cli "hello"
    exists1 <- doesFileExist transcriptLog
    assert "transcript not written when cliTranscript is Nothing" (not exists1)
    assert "cliQuery still works without transcript" $ "stdin:hello" `T.isInfixOf` result1

    -- Part 2: transcript enabled → one record written per cliQuery call
    postRef <- newIORef 42
    let cliEnabled = cli {cliTranscript = Just (postRef, transcriptLog)}
    result2 <- cliQuery cliEnabled "hi there"
    assert "cliQuery with transcript still produces reply" $ "stdin:hi there" `T.isInfixOf` result2
    -- Check transcript file exists and has one valid JSONL record
    tlog <- TIO.readFile transcriptLog
    let tlines = filter (not . T.null) (T.lines tlog)
    assert "transcript has exactly one record" $ length tlines == 1
    let line = head tlines
    assert "transcript record has post_id:42" $ "\"post_id\":42" `T.isInfixOf` line
    assert "transcript record has exit_code:0" $ "\"exit_code\":0" `T.isInfixOf` line
    assert "transcript record has session_id" $ "\"session_id\":\"fake-tid\"" `T.isInfixOf` line
    assert "transcript record contains raw" $ "\"raw\":\"" `T.isInfixOf` line
    assert "transcript raw has stdout content" $ "stdin:hi there" `T.isInfixOf` line
    assert "transcript raw has stderr content" $ "hello from stderr" `T.isInfixOf` line

    -- Part 3: silent failure — write to an impossible path
    let cliBad = cli {cliTranscript = Just (postRef, badTranscriptLog)}
    result3 <- cliQuery cliBad "still works"
    assert "cliQuery works despite broken transcript path" $ "stdin:still works" `T.isInfixOf` result3

  -- Part 4: full-path test through cliHost + hostShard with transcript
  -- enabled.  Two posts → two cliQuery invocations → two transcript records.
  -- Verify cliScrub(raw) reproduces the reply body.
  --
  -- Mutation guard: if someone drops writeTranscript from cliQuery, the
  -- transcript log stays empty and this test fails at the length check.

  do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "free-agent-transcript-host-axioma"
        script = dir </> "fake-echo.sh"
        sf = dir </> "session"
        transcriptLog = dir </> "transcripts" </> "fake.jsonl"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-host'",
            "echo 'diagnostic noise' >&2",
            "printf 'echo:'",
            "cat"
          ]
    let p1 = mkPost "human" ["fake"] "hello"
        p2 = mkPost "human" ["fake"] "world"
    postRef <- newIORef 0
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = cleanCliOut,
              cliStderr = StderrDrop,
              cliStderrTee = Nothing,
              cliTranscript = Just (postRef, transcriptLog)
            }
        h = cliHost "fake" cli
        sh :: Shard (StateT [Post Text] IO) [Post Text] [Post Text]
        sh = hostShard h
    -- Post 1
    writeIORef postRef 1
    (outs1, st1) <- closeShardIO sh [p1] []
    assert "hostShard single post emits 1 reply" $ length outs1 == 1
    assert "hostShard buffer cleared after single" $ st1 == []
    -- Post 2
    writeIORef postRef 2
    (outs2, st2) <- closeShardIO sh [p2] []
    assert "hostShard second post emits 1 reply" $ length outs2 == 1
    assert "hostShard buffer cleared after second" $ st2 == []
    -- Transcript: 2 records, one per post
    tlog <- TIO.readFile transcriptLog
    let tlines = filter (not . T.null) (T.lines tlog)
    assert "transcript has 2 records (one per post)" $ length tlines == 2
    let line1 = tlines !! 0
        line2 = tlines !! 1
    assert "record 1 has post_id:1" $ "\"post_id\":1" `T.isInfixOf` line1
    assert "record 2 has post_id:2" $ "\"post_id\":2" `T.isInfixOf` line2
    assert "record 1 raw has reply content" $ "echo:hello" `T.isInfixOf` line1
    assert "record 2 raw has reply content" $ "echo:world" `T.isInfixOf` line2
    -- raw contains what cleanCliOut strips (session_id line, stderr noise)
    assert "record 1 raw has session_id line" $ "session_id:" `T.isInfixOf` line1
    assert "record 1 raw has stderr noise" $ "diagnostic noise" `T.isInfixOf` line1
    -- replies are scrubbed: no session_id, no stderr noise
    case (outs1, outs2) of
      ([reply1], [reply2]) -> do
        assert "reply 1 body is echo:hello" $ body reply1 == "echo:hello"
        assert "reply 2 body is echo:world" $ body reply2 == "echo:world"
        assert "reply 1 has no session_id line" $
          not ("session_id:" `T.isInfixOf` body reply1)
        assert "reply 1 has no stderr noise" $
          not ("diagnostic noise" `T.isInfixOf` body reply1)
      _ -> assert "expected one reply per post" False

  -------------------------------------------------------------------------
  -- Bundle primitives (FreeSeat fold / agreement)
  --
  -- The shard-level tensor laws live in circuits-agent-axioma.  Here we only
  -- witness that the free syntax folds to those citizens and agrees with the
  -- agent-level semantics where applicable.
  -------------------------------------------------------------------------
  putStrLn "Bundle primitives"

  putStrLn "silent seat folds to silentShard"
  do
    let posts = [mkPost "human" ["bot"] "x"]
    (outsFree, _) <- closeShardIO (interpretSeat silentSeat) posts []
    (outsShard, _) <- closeShardIO silentShard posts []
    assert "silent seat folds to silentShard" $ outsFree == outsShard

  putStrLn "fork is id"
  do
    let p :: Pipeline (Post Text) (Post Text)
        p = mapP (\x -> x {body = "m:" <> body x})
        posts = [mkPost "human" ["bot"] "hi"]
    (outs1, _) <- closeShardIO (interpretSeat (pipelineSeat p)) posts []
    (outs2, _) <- closeShardIO (interpretSeat (forkSeat (pipelineSeat p))) posts []
    assert "fork seat agrees with plain seat" $ outs1 == outs2

  putStrLn "awaitSeat folds to awaitShard"
  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        posts = [mkPost "human" [] "one"]
    (outsFree, _) <- closeShardIO (interpretSeat (awaitSeat a b)) posts []
    (outsShard, _) <-
      closeShardIO
        (awaitShard (pipelineShard (routeP (const [p]))) (pipelineShard (routeP (const [q]))))
        posts
        []
    assert "awaitSeat folds to awaitShard" $ outsFree == outsShard

  putStrLn "raceSeat folds to raceShard and agrees with raceA"
  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        posts = [mkPost "human" [] "one"]
        (outsPure, _) =
          runAgentShard
            (raceA (tape (const [p])) (tape (const [q])))
            (AgentSeat ([], []) [])
            posts
    (outsFree, _) <- closeShardIO (interpretSeat (raceSeat a b)) posts []
    (outsShard, _) <-
      closeShardIO
        (raceShard (pipelineShard (routeP (const [p]))) (pipelineShard (routeP (const [q]))))
        posts
        []
    assert "raceSeat folds to raceShard" $ outsFree == outsShard
    assert "raceSeat agrees with raceA" $ outsFree == outsPure

  putStrLn "fanOutSeat folds to fanOutShard"
  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        posts = [mkPost "human" [] "one"]
    (outsFree, _) <- closeShardIO (interpretSeat (fanOutSeat [a, b])) posts []
    (outsShard, _) <-
      closeShardIO
        (fanOutShard [pipelineShard (routeP (const [p])), pipelineShard (routeP (const [q]))])
        posts
        []
    assert "fanOutSeat folds to fanOutShard" $ outsFree == outsShard

  putStrLn "fanInSeat folds to fanInShard"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "sum" [] (T.intercalate "+" (map body (concat pss)))]
        posts = [mkPost "human" [] "one"]
    (outsFree, _) <- closeShardIO (interpretSeat (fanInSeat summary [a, b])) posts []
    (outsShard, _) <-
      closeShardIO
        ( fanInShard
            summary
            [ pipelineShard (routeP (\x -> [x {body = body x <> "-a"}])),
              pipelineShard (routeP (\x -> [x {body = body x <> "-b"}]))
            ]
        )
        posts
        []
    assert "fanInSeat folds to fanInShard" $ outsFree == outsShard

  putStrLn "bundle is fan-in over forks"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "bundle" [] (T.intercalate "|" (map body (concat pss)))]
        posts = [mkPost "human" [] "one"]
    (outs, _) <- closeShardIO (interpretSeat (bundleSeat summary [forkSeat a, forkSeat b])) posts []
    assert "bundle: summary is one post" $ length outs == 1
    assert "bundle: summary body collects fork outputs" $
      case outs of
        [o] -> body o == "one-a|one-b"
        _ -> False

  -------------------------------------------------------------------------
  -- FreeSeat tensor laws (syntax-level) and agent-level agreement
  -------------------------------------------------------------------------
  putStrLn "FreeSeat tensor laws"

  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        r = mkPost "test" [] "r"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        c = pipelineSeat (routeP (const [r]))
        posts = [mkPost "human" [] "one"]
        leftA = awaitSeat a (awaitSeat b c)
        rightA = awaitSeat (awaitSeat a b) c
    (outsL, _) <- closeShardIO (interpretSeat leftA) posts []
    (outsR, _) <- closeShardIO (interpretSeat rightA) posts []
    assert "SeatAwait associative under interpretSeat" $ outsL == outsR
    assert "SeatAwait interpretSeatA agrees with interpretSeat" $
      interpretSeatA leftA posts == outsL

  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        r = mkPost "test" [] "r"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        c = pipelineSeat (routeP (const [r]))
        posts = [mkPost "human" [] "one"]
        leftR = raceSeat a (raceSeat b c)
        rightR = raceSeat (raceSeat a b) c
    (outsL, _) <- closeShardIO (interpretSeat leftR) posts []
    (outsR, _) <- closeShardIO (interpretSeat rightR) posts []
    assert "SeatRace associative under interpretSeat" $ outsL == outsR
    assert "SeatRace interpretSeatA agrees with interpretSeat" $
      interpretSeatA leftR posts == outsL

  do
    let p = mkPost "test" [] "p"
        a = pipelineSeat (routeP (const [p]))
        posts = [mkPost "human" [] "one"]
    (outsA, _) <- closeShardIO (interpretSeat a) posts []
    (outsAwaitRight, _) <- closeShardIO (interpretSeat (awaitSeat a silentSeat)) posts []
    (outsAwaitLeft, _) <- closeShardIO (interpretSeat (awaitSeat silentSeat a)) posts []
    (outsRaceRight, _) <- closeShardIO (interpretSeat (raceSeat a silentSeat)) posts []
    (outsRaceLeft, _) <- closeShardIO (interpretSeat (raceSeat silentSeat a)) posts []
    assert "silentSeat is right unit for SeatAwait" $ outsAwaitRight == outsA
    assert "silentSeat is left unit for SeatAwait" $ outsAwaitLeft == outsA
    assert "silentSeat is right unit for SeatRace (left bias)" $ outsRaceRight == outsA
    assert "silentSeat is left unit for SeatRace (right bias)" $ outsRaceLeft == outsA

  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "sum" [] (T.intercalate "+" (map body (concat pss)))]
        fan = fanInSeat summary [a, b]
        posts = [mkPost "human" [] "one"]
    (outsShard, _) <- closeShardIO (interpretSeat fan) posts []
    assert "SeatFanIn interpretSeatA agrees with interpretSeat" $
      interpretSeatA fan posts == outsShard

  -------------------------------------------------------------------------
  -- STM seat interpretation (interpretSeatS)
  -------------------------------------------------------------------------
  putStrLn "STM seat interpretation"

  putStrLn "STM pipeline agrees with pure behaviour"
  do
    let p = mapP (\x -> x {body = "s:" <> body x}) `P.Compose` filterP (\x -> body x /= "noise")
        posts =
          [ mkPost "human" ["bot"] "hello",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "world"
          ]
    outsS <- runAgentSBox (interpretSeatS (pipelineSeat p)) posts
    let outsA = interpretSeatA (pipelineSeat p) posts
    assert "interpretSeatS pipeline equals interpretSeatA" $ outsS == outsA
    assert "interpretSeatS pipeline equals runPipeline" $ outsS == runPipeline p posts

  putStrLn "STM silent seat emits nothing"
  do
    let posts = [mkPost "human" ["bot"] "x"]
    outsS <- runAgentSBox (interpretSeatS silentSeat) posts
    assert "silent seat STM output is empty" $ null outsS

  putStrLn "STM composition agrees with pure behaviour"
  do
    let p1 = filterP (\x -> body x /= "noise") :: Pipeline (Post Text) (Post Text)
        p2 = mapP (\x -> x {body = "m:" <> body x}) :: Pipeline (Post Text) (Post Text)
        seat = SeatCompose (pipelineSeat p2) (pipelineSeat p1)
        posts =
          [ mkPost "human" ["bot"] "hello",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "world"
          ]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    assert "interpretSeatS composition equals interpretSeatA" $ outsS == interpretSeatA seat posts

  putStrLn "STM await agrees with pure behaviour and shard fold"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        seat = awaitSeat a b
        posts = [mkPost "human" [] "one"]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    (outsShard, _) <- closeShardIO (interpretSeat seat) posts []
    assert "interpretSeatS await equals interpretSeatA" $ outsS == interpretSeatA seat posts
    assert "interpretSeatS await equals interpretSeat shard" $ outsS == outsShard

  putStrLn "STM race is left-biased like pure behaviour"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        seat = raceSeat a b
        posts = [mkPost "human" [] "one"]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    assert "interpretSeatS race equals interpretSeatA" $ outsS == interpretSeatA seat posts

  putStrLn "STM fan-out/fan-in agrees with pure behaviour"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "sum" [] (T.intercalate "+" (map body (concat pss)))]
        seat = fanInSeat summary [a, b]
        posts = [mkPost "human" [] "one", mkPost "human" [] "two"]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    assert "interpretSeatS fanIn equals interpretSeatA" $ outsS == interpretSeatA seat posts

  putStrLn "STM bag invariant under input permutation"
  do
    let p = mapP (\x -> x {body = "p:" <> body x}) :: Pipeline (Post Text) (Post Text)
        posts =
          [ mkPost "human" ["bot"] "one",
            mkPost "human" ["bot"] "two",
            mkPost "human" ["bot"] "three"
          ]
        perm = reverse posts
    outs1 <- runAgentSBox (interpretSeatS (pipelineSeat p)) posts
    outs2 <- runAgentSBox (interpretSeatS (pipelineSeat p)) perm
    assert "bag of outputs is invariant under input permutation" $
      toBag outs1 == toBag outs2

  -------------------------------------------------------------------------
  -- Diagram bridge (endgame stage 1): an agent is a box; one diagram run
  -- is one Moore step.  Feedback wiring (bend + delay) is not yet
  -- expressible in the string-diagram surface — see Free.Agent.Diagram.
  -------------------------------------------------------------------------
  putStrLn "diagram bridge"
  do
    let sysN = System (\(s, d) -> (s + monoDir d, (s + 1, ()))) :: System (->) Int (Mono Int Int)
    assert "diagram step is runSystem's (put, get)" $
      diagramStep sysN 5 3 == (snd (runSystem sysN 5) 3, fst (runSystem sysN 5))
    assert "diagram steps mirror iterateSystem" $
      diagramSteps sysN 0 [1 .. 5] == iterateSystem sysN 0 [1 .. 5]

  do
    let echo =
          tape (\hist -> [mkPost "bot" ["human"] ("n:" <> T.pack (show (length hist)))]) ::
            Agent (->) [Post Text] (Post Text) [Post Text]
        p0 = mkPost "human" ["bot"] "one"
        ins =
          [ p0,
            mkPost "human" ["bot"] "two",
            mkPost "human" ["bot"] "three"
          ]
    assert "tape agent: diagram step is runSystem's (put, get)" $
      diagramStep echo [] p0
        == (snd (runSystem echo []) p0, fst (runSystem echo []))
    assert "tape agent: diagram steps mirror iterateSystem" $
      diagramSteps echo [] ins == iterateSystem echo [] ins

  -------------------------------------------------------------------------
  -- Bend with delay (endgame stage 1b): a stateful agent decomposes as a
  -- stateless body plus cross-tick feedback; 'register' closes the state
  -- wire with the one-tick 'delay' explicit in the wiring.
  -------------------------------------------------------------------------
  putStrLn "bend with delay"
  do
    -- scan's inject consumes the first input as state initialisation, so a
    -- standalone delay emits the initial value, then echoes the tail.
    -- (Inside register, where the feedback wire carries the body's own
    -- state output, this is exactly "observable one tick late".)
    assert "delay emits initial value, then the tail one tick late" $
      scan (delay 0) [1, 2, 3, 4 :: Int] == [0, 2, 3, 4]

  do
    let sysN = System (\(s, d) -> (s + monoDir d, (s + 1, ()))) :: System (->) Int (Mono Int Int)
    assert "register body with delay mirrors iterateSystem" $
      scan (mooreProcess sysN 0) [1 .. 5] == iterateSystem sysN 0 [1 .. 5]

  do
    let echo =
          tape (\hist -> [mkPost "bot" ["human"] ("n:" <> T.pack (show (length hist)))]) ::
            Agent (->) [Post Text] (Post Text) [Post Text]
        ins = [mkPost "human" ["bot"] "one", mkPost "human" ["bot"] "two"]
    assert "tape agent: register body with delay mirrors iterateSystem" $
      scan (mooreProcess echo []) ins == iterateSystem echo [] ins

  -- The bend-and-delay identity from Circuit.Process: for bodies whose
  -- fixed point is independent of the initial feedback value, register's
  -- wiring equals the cartesian trace with delay on the feedback wire.
  do
    let body = liftProcess (\(i, _s) -> (i * 2, 0 :: Int))
        swapP = liftProcess (\(a, b) -> (b, a))
        ins = [1, 2, 3, 4 :: Int]
        viaRegister = scan (register 99 body) ins
        viaTrace = scan (trace (swapP . (body . strength (delay 99)) . swapP)) ins
    assert "register ≡ trace with delay on the feedback wire" $
      viaRegister == viaTrace && viaRegister == [2, 4, 6, 8]

  -------------------------------------------------------------------------
  -- Meeting skeleton (endgame stage 2/2b): a conversation reads back as a
  -- drawing of the full post-DAG — chains, forks (visible copy spiders)
  -- and syntheses (visible merge spiders), routed with SSwap permutations.
  -------------------------------------------------------------------------
  putStrLn "meeting skeleton"
  do
    let seed = mkPost "human" ["a"] "start"
        a1 = replyTo "a" 0 seed "ack"
        b1 = replyTo "b" 1 a1 "ack"
        a2 = replyTo "a" 2 b1 "ack"
        convo = [seed, a1, b1, a2]
    assert "skeleton of a ping-pong is the speaker chain (hyper-normal form)" $
      hyperEquiv (meetingSkeleton convo) $
        SThenD (SBox "human" 0 1) (SThenD (SBox "a" 1 1) (SThenD (SBox "b" 1 1) (SBox "a" 1 1)))
    assert "single-thread ancestry is the skeleton's label sequence" $
      case branchesByIndex [seed, a1, b1] a2 of
        [path] -> skeletonLabels (meetingSkeleton convo) == reverse (map T.unpack path)
        _ -> False

  putStrLn "meeting skeleton: forks, syntheses, dangling parents"
  do
    let p0 = mkPost "a" [] "seed"
        r1 = replyTo "b" 0 p0 "r1"
        r2 = replyTo "c" 0 p0 "r2"
        forkSkel = meetingSkeleton [p0, r1, r2]
        hgFork = normalise forkSkel
    assert "fork: one wire joins the parent's output to both replies' inputs" $
      Wire [] [PortEnd "a" Out 0, PortEnd "b" In 0, PortEnd "c" In 0] `elem` hgWires hgFork
    assert "fork: no free inputs, both replies reach the boundary" $
      hgInArity hgFork == 0 && hgOutArity hgFork == 2
    assert "fork: the copy spider is visible in the drawing" $
      "spider" `elem` skeletonLabels forkSkel

  do
    let pA = mkPost "a" [] "1"
        pB = mkPost "b" [] "2"
        pC = mkPost "c" [] "3"
        s = synthesis "s" [] [0, 1, 2] "sum"
        synSkel = meetingSkeleton [pA, pB, pC, s]
        hgSyn = normalise synSkel
    assert "synthesis: the box input descends from a merge of exactly the three parents' outputs" $
      Wire [] [PortEnd "a" Out 0, PortEnd "b" Out 0, PortEnd "c" Out 0, PortEnd "s" In 0] `elem` hgWires hgSyn
    assert "synthesis: no free inputs, one boundary output" $
      hgInArity hgSyn == 0 && hgOutArity hgSyn == 1
    assert "synthesis: the merge spider is visible in the drawing" $
      "spider" `elem` skeletonLabels synSkel

  do
    let d = (mkPost "a" [] "x") {thread = [99]}
        hgD = normalise (meetingSkeleton [d])
    assert "dangling parent: a free input wire feeds the box" $
      hgInArity hgD == 1 && Wire [InB 0] [PortEnd "a" In 0] `elem` hgWires hgD

  -------------------------------------------------------------------------
  -- Panel meeting (stage L2d): the shape of the committed SVG artifact —
  -- one seed fork, crossing round-2 wires, one synthesis merge.
  -------------------------------------------------------------------------
  putStrLn "panel meeting"
  do
    let seed = mkPost "human" ["agent-1", "agent-2", "agent-3"] "Q"
        a1 = replyTo "agent-1" 0 seed "A1"
        b1 = replyTo "agent-2" 0 seed "B1"
        c1 = replyTo "agent-3" 0 seed "C1"
        a2 = replyTo "agent-1" 2 b1 "A2"
        b2 = replyTo "agent-2" 3 c1 "B2"
        c2 = replyTo "agent-3" 1 a1 "C2"
        synth = synthesis "synth" ["human"] [4, 5, 6] "S"
        skel = meetingSkeleton [seed, a1, b1, c1, a2, b2, c2, synth]
    assert "panel skeleton has the 8 boxes in label order" $
      boxLabels skel
        == ["human", "agent-1", "agent-2", "agent-3", "agent-1", "agent-2", "agent-3", "synth"]
    assert "the seed's fork spider is visible (SSpider 1 3)" $
      countSpiders (1, 3) skel == 1
    assert "the synthesis merge spider is visible (SSpider 3 1)" $
      countSpiders (3, 1) skel == 1

  -------------------------------------------------------------------------
  -- Hyper generators (endgame stage 3): copy/merge/braid for streams, and
  -- 'both' — the merge of two agents.  Semantics on record: bag at the
  -- wire, set at the name.
  -------------------------------------------------------------------------
  putStrLn "hyper generators"
  do
    let xs = [mkPost "a" [] "a1", mkPost "a" [] "a2"]
        ys = [mkPost "b" [] "b1"]
        lhs = copyP (mergeP (xs, ys))
        rhs = merge2 (braidP (copy2 (xs, ys)))
    assert "bialgebra holds exactly for singleton blocks" $
      copyP (mergeP ([mkPost "a" [] "a1"], [mkPost "b" [] "b1"]))
        == merge2 (braidP (copy2 ([mkPost "a" [] "a1"], [mkPost "b" [] "b1"])))
    assert "bialgebra holds exactly for general streams" $
      lhs == rhs

  do
    let agA = tape (\hist -> [mkPost "a" [] ("a:" <> T.pack (show (length hist)))]) :: Agent (->) [Post Text] (Post Text) [Post Text]
        agB = tape (\hist -> [mkPost "b" [] ("b:" <> T.pack (show (length hist)))]) :: Agent (->) [Post Text] (Post Text) [Post Text]
        ins = [mkPost "human" ["x"] "one", mkPost "human" ["x"] "two"]
        outsAB = concat (iterateSystem (both agA agB) ([], []) ins)
        outsBA = concat (iterateSystem (both agB agA) ([], []) ins)
    assert "both commutes as a bag of outputs" $
      toBag outsAB == toBag outsBA
    assert "both emits left then right per step" $
      map from outsAB == ["a", "b", "a", "b"]
    assert "silent is a left zero for both" $
      iterateSystem (both silent agA) ((), []) ins == iterateSystem agA [] ins
    assert "silent is a right zero for both" $
      iterateSystem (both agA silent) ([], ()) ins == iterateSystem agA [] ins
    assert "both is not idempotent: bag at the wire (double-post)" $
      length (concat (iterateSystem (both agA agA) ([], []) ins))
        == 2 * length (concat (iterateSystem agA [] ins))

  -- Replay (endgame stage 5): swap one box, re-derive the meeting.
  -- Deterministic quoters stand in for models; the tag is the "model".
  putStrLn "replay"
  do
    let box who tg = AgentBox ([], [], [], []) (quoter who tg)
        roster tg = [box "a" "A", box "b" tg, box "c" "C"]
        seed = [mkPost "human" ["panel"] "Q"]
        log1 = meetLog 2 (roster "B") seed
        log2 = meetLog 2 (roster "B-ALT") seed
    assert "replay with a fresh identical roster reproduces the log" $
      log1 == meetLog 2 (roster "B") seed
    assert "the swap takes effect in round 1" $
      log1 !! 2 /= log2 !! 2
    assert "unchanged subtrees reproduce identical posts" $
      unchanged ["b"] log1 == unchanged ["b"] log2
    assert "unchanged subtree is seed plus the unswapped round-1 posts" $
      length (unchanged ["b"] log1) == 3
    assert "posts downstream of the swap differ" $
      drop 4 log1 /= drop 4 log2
    assert "thread ids reproduce exactly between identical replays" $
      map thread log1 == map thread log2

  -- Derivations as 2-cells (endgame stage 6): the squares are recoverable
  -- from the log; pasting is roster merge (horizontal) and time (vertical).
  putStrLn "derivations as 2-cells"
  do
    let box who tg = AgentBox ([], [], [], []) (quoter who tg)
        seed = [mkPost "human" ["panel"] "Q"]
        log3 = meetLog 2 [box "a" "A", box "b" "B", box "c" "C"] seed
    assert "every square in a meeting log is valid" $
      and [valid prior p | (prior, p) <- zip (inits log3) log3]
    assert "vertical pasting: the chase from the last post covers its cone" $
      sortNub (map from (chaseLog log3)) == coneByIndex (init log3) (last log3)
    assert "vertical pasting: the chase visits exactly the cone names" $
      sortNub (map from (chaseLog (take 4 log3))) == coneByIndex (take 3 log3) (log3 !! 3)
    assert "a square's vertical sources are the resolved parents" $
      case drop 5 log3 of
        (p : _) ->
          let prior = take 5 log3
           in map from (dParents (derivation prior p))
                == map (\i -> from (prior !! fromIntegral i)) (thread p)
        _ -> False
    let boxA = AgentBox ([], [], [], []) (quoter "a" "A")
        boxB = AgentBox ([], [], [], []) (quoter "b" "B")
        merged = AgentBox (([], [], [], []), ([], [], [], [])) (both (quoter "a" "A") (quoter "b" "B"))
    assert "horizontal pasting: loop [both a b] ≡ loop [a, b]" $
      meetLog 2 [merged] seed == meetLog 2 [boxA, boxB] seed

  -------------------------------------------------------------------------
  -- Bus stats (flow metrics)
  -------------------------------------------------------------------------
  putStrLn "bus stats"

  do
    let p = mkPost "a" ["b"] "standing by"
    assert "plain status ping is noise" $ classify defaultRules p == Noise

  do
    let p = mkPost "a" ["b"] "standing by with loom/foo.md"
    assert "noise carrying a path is signal" $ classify defaultRules p == Signal

  do
    let p = mkPost "a" ["b"] "decided to keep the current design"
    assert "decision word is signal" $ classify defaultRules p == Signal

  do
    let p = mkPost "a" ["b"] "DONE: stuff/foo.md"
    assert "DONE plus path is a done claim" $ isDoneClaim p

  do
    let p = mkPost "a" ["b"] "done with the task"
    assert "DONE without path is not a done claim" $ not (isDoneClaim p)

  do
    let mkStored i ts f t body = case parseTimeText ts of
          Just ts' -> Stamped {stamp = i, timeStamp = ts', stamped = Post f t [] body}
          Nothing -> error $ "bad timestamp: " <> show ts
        posts =
          [ mkStored 0 "2026-08-05T00:00:00" "a" ["b"] "hello",
            mkStored 1 "2026-08-05T00:10:00" "b" ["a"] "ack",
            mkStored 2 "2026-08-05T00:20:00" "a" ["b"] "standing by",
            mkStored 3 "2026-08-05T00:30:00" "a" ["b"] "🟢 card done"
          ]
        slices = slicePosts WholeLog posts
        stats = case map (uncurry (computeStats defaultRules 1)) slices of
          [s] -> s
          _ -> error "expected exactly one slice"
    assert "whole log is one slice" $ length slices == 1
    assert "counts all posts" $ statPosts stats == 4
    assert "counts agents" $ statAgents stats == 2
    assert "signal = deliverable mark" $ statSignal stats == 1
    assert "noise = status pings" $ statNoise stats == 2
    assert "unclassified = neutral posts" $ statUnclassified stats == 1
    assert "deliverables = conductor marks" $ statDeliverables stats == 1

  do
    let mkStored i ts f t body = case parseTimeText ts of
          Just ts' -> Stamped {stamp = i, timeStamp = ts', stamped = Post f t [] body}
          Nothing -> error $ "bad timestamp: " <> show ts
        posts =
          [ mkStored 0 "2026-08-05T00:00:00" "a" ["b"] "hello",
            mkStored 1 "2026-08-05T00:10:00" "b" ["a"] "ack",
            mkStored 2 "2026-08-05T01:05:00" "a" ["b"] "standing by",
            mkStored 3 "2026-08-05T01:20:00" "a" ["b"] "🟢 card done"
          ]
        slices = slicePosts (WindowMinutes 60) posts
    assert "60m window splits four posts into two slices" $ length slices == 2

  do
    let mkStored i ts f t body = case parseTimeText ts of
          Just ts' -> Stamped {stamp = i, timeStamp = ts', stamped = Post f t [] body}
          Nothing -> error $ "bad timestamp: " <> show ts
        posts =
          [ mkStored 0 "2026-08-05T00:00:00" "a" ["b"] "hello",
            mkStored 1 "2026-08-05T00:10:00" "b" ["a"] "ack",
            mkStored 2 "2026-08-05T00:20:00" "a" ["b"] "standing by",
            mkStored 3 "2026-08-05T00:30:00" "b" ["a"] "🟢 card done"
          ]
        slices = slicePosts ByAgent posts
    assert "by-agent splits into two author slices" $ length slices == 2
    assert "author a has one post" $
      case filter ((== "a") . fst) slices of
        [(_, ps)] -> length ps == 2
        _ -> False
    assert "author b has one signal and one noise post" $
      case filter ((== "b") . fst) slices of
        [(_, ps)] ->
          let stats = computeStats defaultRules 1 "b" ps
           in statSignal stats == 1 && statNoise stats == 1
        _ -> False

  -------------------------------------------------------------------------
  -- Bus seat: the in-process runner, decided quiet included
  --
  -- openBus + postLocal + runSeatBus on a tmpdir bus.  The halt mark is
  -- already on the log when the seat starts, so runSeatBus must process
  -- the work, skip the mark (control, not content), and return.
  -------------------------------------------------------------------------
  putStrLn "bus seat"
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-seat-axioma"
    removePathForcibly root
    createDirectoryIfMissing True root
    _ <- postLocal root (mkPost "human" ["echo"] "hello")
    _ <- postLocal root (mkPost "human" ["echo"] "🟢 landed")
    bus <- openBus root
    runSeatBus bus "echo" ["echo"] (hostSeat (mkHost "echo" (pure . map ("echo:" <>))))
    closeBus bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (parseLine @Text) (T.lines content)
        fromEcho = [stamped s | s <- parsed, from (stamped s) == "echo"]
    assert "seat replied to the work post with a thread edge" $
      case fromEcho of
        [p] -> body p == "echo:hello" && to p == ["human"] && thread p == [0]
        _ -> False
    assert "ids are coherent across postLocal and the seat scribe" $
      map stamp parsed == [0, 1, 2]
    assert "the halt mark got silence, not a reply" $
      length fromEcho == 1

  -------------------------------------------------------------------------
  -- Bus seat supervision: a handler exception becomes a 🔴 escalation,
  -- the cursor advances, and the seat keeps listening.
  -------------------------------------------------------------------------
  putStrLn "bus seat supervision"
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-supervision-axioma"
    removePathForcibly root
    createDirectoryIfMissing True root
    _ <- postLocal root (mkPost "human" ["fragile"] "hello")
    _ <- postLocal root (mkPost "human" ["fragile"] "boom")
    _ <- postLocal root (mkPost "human" ["fragile"] "world")
    _ <- postLocal root (mkPost "human" ["fragile"] "🟢 landed")
    bus <- openBus root
    let fragileHost = mkHost "fragile" $ \ws ->
          if ws == ["boom"]
            then error "boom"
            else pure (map ("fragile:" <>) ws)
    runSeatBus bus "fragile" ["fragile"] (hostSeat fragileHost)
    closeBus bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (parseLine @Text) (T.lines content)
        fromFragile = [stamped s | s <- parsed, from (stamped s) == "fragile"]
        escalations = filter (maybe False isEscalate . markOf) fromFragile
    assert "seat replies to the first post" $
      any (\p -> body p == "fragile:hello" && to p == ["human"] && thread p == [0]) fromFragile
    assert "seat posts exactly one escalation on handler failure" $
      case escalations of
        [p] -> to p == ["human"] && "boom" `T.isInfixOf` body p
        _ -> False
    assert "seat replies to the third post after a failure" $
      any (\p -> body p == "fragile:world" && to p == ["human"] && thread p == [2]) fromFragile
    assert "ids are coherent across postLocal, replies, and escalation" $
      map stamp parsed == [0, 1, 2, 3, 4, 5, 6]

  -------------------------------------------------------------------------
  -- Bus seat self-halt (tail-chatter, R3c): a seat's own 🔵 stops the loop.
  --
  -- 🟢 stays exchange-level: a seat may land one exchange and host more.
  -- 🔵 is seat-level: standing down is the seat deciding its own quiet.
  -- The F2 self-post skip means the seat never reads its own mark back,
  -- so the halt must be judged at commit time.  A post arriving after the
  -- stand-down must go unanswered — the trailing 🟢 is only a safety
  -- stopper for the buggy behaviour, where the seat lives on and answers.
  -------------------------------------------------------------------------
  putStrLn "bus seat self-halt"
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-selfhalt-axioma"
    removePathForcibly root
    createDirectoryIfMissing True root
    _ <- postLocal root (mkPost "human" ["echo"] "task")
    _ <- postLocal root (mkPost "human" ["echo"] "hello")
    _ <- postLocal root (mkPost "human" ["echo"] "finish")
    _ <- postLocal root (mkPost "human" ["echo"] "ping")
    _ <- postLocal root (mkPost "human" ["echo"] "🟢 landed")
    bus <- openBus root
    let host = mkHost "echo" $ \ws ->
          pure $
            if "task" `elem` ws
              then [markGlyph Landed <> " task done"]
              else
                if "finish" `elem` ws
                  then [markGlyph StandDown <> " standing down"]
                  else map ("echo:" <>) ws
    runSeatBus bus "echo" ["echo"] (hostSeat host)
    closeBus bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (parseLine @Text) (T.lines content)
        fromEcho = [stamped s | s <- parsed, from (stamped s) == "echo"]
    assert "a 🟢 reply lands the exchange but the seat hosts more" $
      any (\p -> body p == markGlyph Landed <> " task done" && thread p == [0]) fromEcho
        && any (\p -> body p == "echo:hello" && thread p == [1]) fromEcho
    assert "the seat posts its own stand-down" $
      any (\p -> markOf p == Just StandDown) fromEcho
    assert "no reply after the stand-down (tail-chatter)" $
      not (any (\p -> "ping" `T.isInfixOf` body p) fromEcho)
    assert "ids are coherent: 5 planted + 3 replies, then quiet" $
      map stamp parsed == [0, 1, 2, 3, 4, 5, 6, 7]

  -------------------------------------------------------------------------
  -- Multi-seat card meeting: two FreeSeats share a subscription on the bus
  --
  -- A seed addressed to a shared card wakes every seat; each replies to the
  -- original sender.  A pre-planted halt mark stops all seats once they have
  -- processed the seed.
  -------------------------------------------------------------------------
  putStrLn "multi-seat card meeting"
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-meeting-axioma"
        card = "panel"
    removePathForcibly root
    createDirectoryIfMissing True root
    _ <- postLocal root (mkPost "human" [card] "discuss")
    _ <- postLocal root (mkPost "human" [card] "🟢 landed")
    bus <- openBus root
    let alphaSeat = hostSeat (mkHost "alpha" (pure . map ("alpha:" <>)))
        betaSeat = hostSeat (mkHost "beta" (pure . map ("beta:" <>)))
    doneAlpha <- newEmptyMVar
    doneBeta <- newEmptyMVar
    _ <- forkIO $ runSeatBus bus "alpha" [card] alphaSeat >> putMVar doneAlpha ()
    _ <- forkIO $ runSeatBus bus "beta" [card] betaSeat >> putMVar doneBeta ()
    takeMVar doneAlpha
    takeMVar doneBeta
    closeBus bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (parseLine @Text) (T.lines content)
        fromAlpha = [stamped s | s <- parsed, from (stamped s) == "alpha"]
        fromBeta = [stamped s | s <- parsed, from (stamped s) == "beta"]
    assert "alpha replied to the seed" $
      case fromAlpha of
        [p] -> body p == "alpha:discuss" && to p == ["human"] && thread p == [0]
        _ -> False
    assert "beta replied to the seed" $
      case fromBeta of
        [p] -> body p == "beta:discuss" && to p == ["human"] && thread p == [0]
        _ -> False
    assert "ids are coherent across postLocal and seat scribes" $
      map stamp parsed == [0, 1, 2, 3]
    assert "the halt mark got silence from every seat" $
      length (fromAlpha ++ fromBeta) == 2

  -------------------------------------------------------------------------
  -- tailLog: offset draining, partial trailing line, halt mid-batch.
  --
  -- tailLog reads complete lines only; a writer mid-append is left for the
  -- next drain. A callback that returns 'Halt' stops further delivery.
  -------------------------------------------------------------------------
  putStrLn "tailLog"
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-tail-axioma"
        path = root </> "log.jsonl"
        mkStored i ts f t b = case parseTimeText ts of
          Just ts' -> Stamped {stamp = i, timeStamp = ts', stamped = Post f t [] b}
          Nothing -> error $ "bad timestamp: " <> show ts
        frame i ts f t b = frameStored @Text (mkStored i ts f t b)
    removePathForcibly root
    createDirectoryIfMissing True root
    -- Write one complete line and one partial line (no trailing newline).
    TIO.writeFile
      path
      ( frame 0 "2026-08-05T00:00:00" "human" ["test"] "one"
          <> "\n"
          <> frame 1 "2026-08-05T00:00:01" "human" ["test"] "two"
      )
    collected <- newMVar []
    tid <-
      forkIO $
        tailLog path ["test"] 0 Nothing $ \stored -> do
          modifyMVar_ collected (pure . (stored :))
          pure (if body (stamped stored) == "halt" then Halt else Continue)
    -- Wait for the initial drain to finish before appending.
    threadDelay 200000
    initial <- readMVar collected
    assert "offset drain: the complete line is delivered, the partial line is not" $
      map (body . stamped) (reverse initial) == ["one"]
    -- Complete the partial line and append a halt plus one more post.
    TIO.appendFile
      path
      ( "\n"
          <> frame 2 "2026-08-05T00:00:02" "human" ["test"] "halt"
          <> "\n"
          <> frame 3 "2026-08-05T00:00:03" "human" ["test"] "four"
          <> "\n"
      )
    -- Wait for fsnotify to wake the drain and for the halt to stop the loop.
    threadDelay 1000000
    killThread tid
    final <- readMVar collected
    let bodies = map (body . stamped) (reverse final)
    assert "partial trailing line is delivered once it is completed" $
      bodies == ["one", "two", "halt"]
    assert "halt mid-batch stops delivery" $
      not ("four" `elem` bodies)

  -------------------------------------------------------------------------
  -- bus post anti-pollution (B4)
  --
  -- FREE_AGENT_BUS_ROOT env var + refuse when no log.jsonl exists.
  -------------------------------------------------------------------------
  putStrLn "bus post anti-pollution"

  do
    tmp <- getTemporaryDirectory
    let busRoot = tmp </> "free-agent-b4-bus"
        badRoot = tmp </> "free-agent-b4-nobus"
    removePathForcibly busRoot
    removePathForcibly badRoot
    createDirectoryIfMissing True busRoot
    createDirectoryIfMissing True badRoot
    -- Create a real bus with a log.jsonl
    _ <- postLocal busRoot (mkPost "test" [] "seed")
    -- Test 1: postLocal on a real bus works
    stored <- postLocal busRoot (mkPost "verify" [] "works")
    assert "post to an existing bus succeeds" $
      stamp stored == 1
    -- Test 2: postLocal on a non-bus directory creates one (existing behavior)
    stored2 <- postLocal badRoot (mkPost "verify" [] "also-works")
    assert "postLocal creates bus when absent (library, not CLI)" $
      stamp stored2 == 0
    removePathForcibly busRoot
    removePathForcibly badRoot

  -- CLI-level test: the binary refuses when no log.jsonl and no env var
  do
    tmp <- getTemporaryDirectory
    let nobusDir = tmp </> "free-agent-b4-cli-nobus"
        busDir = tmp </> "free-agent-b4-cli-bus"
    removePathForcibly nobusDir
    removePathForcibly busDir
    createDirectoryIfMissing True nobusDir
    createDirectoryIfMissing True busDir
    -- Create a bus with log.jsonl for the env var test
    _ <- postLocal busDir (mkPost "seed" [] "init")
    -- Find the free-agent binary relative to the axioma binary
    myPath <- takeDirectory <$> getExecutablePath
    let freeAgentBin =
          myPath </> ".." </> ".." </> ".." </> "free-agent" </> "build" </> "free-agent" </> "free-agent"
    -- Test: no env var, no log.jsonl → exit 1
    (code1, out1, _) <-
      readProcessWithExitCode
        freeAgentBin
        ["bus", "post", "--from", "test", "--to", "test", "--body", "should-fail"]
        ""
    assert "CLI post from non-bus dir exits 1" $
      code1 == ExitFailure 1
    assert "CLI post error message names the dir" $
      T.isInfixOf "no log.jsonl" (T.pack out1)
    -- Test: FREE_AGENT_BUS_ROOT → succeeds
    setEnv "FREE_AGENT_BUS_ROOT" busDir
    (code2, out2, _) <-
      readProcessWithExitCode
        freeAgentBin
        ["bus", "post", "--from", "test", "--to", "test", "--body", "env-var-works"]
        ""
    unsetEnv "FREE_AGENT_BUS_ROOT"
    assert "CLI post with FREE_AGENT_BUS_ROOT exits 0" $
      code2 == ExitSuccess
    let parsed = parseLine @Text (T.strip (T.pack out2))
    assert "CLI post lands on the correct bus" $
      case parsed of
        Just s -> stamp s >= 0 && body (stamped s) == "env-var-works"
        Nothing -> False
    removePathForcibly nobusDir
    removePathForcibly busDir

  -------------------------------------------------------------------------
  -- Resume fix oracle (B6): transient failure retries with same session id;
  -- stale session falls back to fresh.  Regression guard: if someone
  -- restores `cliStale = \code _ -> code /= ExitSuccess`, test (a) fails
  -- because the transient failure triggers fresh instead of retry.
  -------------------------------------------------------------------------
  putStrLn "Resume fix (B6)"

  -- Test (a): transient failure → retry with same session id
  do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "free-agent-b6a"
        script = dir </> "fake.sh"
        sf = dir </> "session"
        attemptFile = dir </> "attempt"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    -- Seed the session file with a known id.
    TIO.writeFile sf "test-sid-b6a"
    -- Fake CLI: fails on first call (exit 1), succeeds on second,
    -- echoes argv so we can inspect the --resume flag.
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "ATTEMPT=$(cat " <> attemptFile <> " 2>/dev/null || echo 0)",
            "echo 'session_id: fake-b6a'",
            "if [ \"$ATTEMPT\" -eq 0 ]; then",
            "  echo 1 > " <> attemptFile,
            "  exit 1",
            "fi",
            "echo \"argv:$*\"",
            "printf 'stdin:'",
            "cat"
          ]
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \_ out ->
                "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliTranscript = Nothing
            }
    result <- cliQuery cli "hello-b6a"
    -- Second invocation must carry the --resume flag with our seeded id.
    assert "B6a: retry preserves session id in --resume" $
      "--resume test-sid-b6a" `T.isInfixOf` result
    -- Session file is re-persisted with the scraped id on success (standard).
    stored <- TIO.readFile sf
    assert "B6a: .sid updated with scraped id after retry success" $
      T.strip stored == "fake-b6a"
    removePathForcibly dir

  -- Test (b): stale session → fresh, new .sid written
  do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "free-agent-b6b"
        script = dir </> "fake.sh"
        sf = dir </> "session"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    TIO.writeFile sf "old-stale-sid"
    -- Fake CLI: returns a stale-session message, then a fresh id.
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'No session found matching old-stale-sid'",
            "echo 'session_id: fresh-b6b'",
            "echo \"argv:$*\"",
            "printf 'stdin:'",
            "cat"
          ]
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \_ out ->
                "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliTranscript = Nothing
            }
    result <- cliQuery cli "hello-b6b"
    -- Must NOT carry the old --resume flag (it was a fresh call).
    assert "B6b: stale session does NOT resume old sid" $
      not ("--resume old-stale-sid" `T.isInfixOf` result)
    -- Session file must be updated with the new id.
    stored <- TIO.readFile sf
    assert "B6b: .sid updated to fresh session id" $
      T.strip stored == "fresh-b6b"
    removePathForcibly dir

  -- Test (c): mutation guard — code-is-stale regression
  --
  -- Same pattern as (a) but with the OLD hermesCli cliStale that treats
  -- any non-zero exit as stale.  A transient error then triggers a fresh
  -- call instead of a retry, and the old session id is dropped.
  -- If someone regresses hermesCli back to code-is-stale, this test
  -- catches it by asserting no --resume on the second call.
  do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "free-agent-b6c"
        script = dir </> "fake.sh"
        sf = dir </> "session"
        attemptFile = dir </> "attempt"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    TIO.writeFile sf "should-not-survive"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "ATTEMPT=$(cat " <> attemptFile <> " 2>/dev/null || echo 0)",
            "echo 'session_id: fresh-b6c'",
            "if [ \"$ATTEMPT\" -eq 0 ]; then",
            "  echo 1 > " <> attemptFile,
            "  exit 1",
            "fi",
            "echo \"argv:$*\"",
            "printf 'stdin:'",
            "cat"
          ]
    -- DELIBERATE regression: code /= ExitSuccess makes all errors stale.
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code out ->
                code /= ExitSuccess
                  || "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliTranscript = Nothing
            }
    result <- cliQuery cli "hello-b6c"
    -- With code-is-stale, the first invocation's exit 1 triggers stale →
    -- fresh.  The second call (fresh) must NOT carry --resume.
    assert "B6c: code-is-stale triggers fresh, old sid dropped" $
      not ("--resume should-not-survive" `T.isInfixOf` result)
    -- Fresh call DID produce output (the reply).
    assert "B6c: fresh call produced reply" $
      "stdin:hello-b6c" `T.isInfixOf` result
    -- .sid was updated to the new session id from the fresh call.
    stored <- TIO.readFile sf
    assert "B6c: .sid updated to fresh session id" $
      T.strip stored == "fresh-b6c"
    removePathForcibly dir

  putStrLn "All tests passed"
