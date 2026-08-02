{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Circuit (Ends (..), close)
import Circuit.Agent (AgentSeat (..), Bag, Post (..), Shard, awaitA, raceA, runAgentShard, tape, toBag)
import Circuit.Agent.Tensor
  ( awaitShard,
    fanInShard,
    fanOutShard,
    raceShard,
    silentShard,
  )
import Circuit.Category (Category (id, (.)), ObDict (..))
import Circuit.Layer ((:~>))
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Monad (when)
import Control.Monad.State (State, StateT, runState, runStateT)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Host (BodyMode (..), Host (..), cliHost, mkHost, hostShard, processHost)
import Free.Agent.Layer (bindFreeAgent, runFreeAgent)
import Free.Agent.Pipeline qualified as P
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
import Circuit.Agent.Cli (Cli (..), parseSessionId)
import Free.Agent.Syntax (FreeAgent (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import Prelude hiding (id, (.))

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

mkPost :: Text -> [Text] -> Text -> Post Text
mkPost a ds = Post a ds Nothing

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
        && all (\x -> thread x == Just "human") outs
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
        && all (\x -> thread x == Just "human") outs
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
              cliScrub = id
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
        posts []
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
        posts []
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
        posts []
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
        posts []
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

  putStrLn "All tests passed"
