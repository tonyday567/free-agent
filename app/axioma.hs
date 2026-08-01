{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Circuit (Ends (..), close)
import Circuit.Agent (Post (..), Shard)
import Control.Arrow (runKleisli)
import Control.Monad.State (State, StateT, runState, runStateT)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Host (Host (..), hostShard, processHost)
import Free.Agent.Layer (bindFreeAgent, runFreeAgent)
import Free.Agent.Pipeline qualified as P
import Free.Agent.Pipeline (Pipeline, broadcast, filterP, mapP, pipelineShard, routeBy, routeP, routeTo, runPipeline)
import Free.Agent.Seat (FreeSeat (..), hostSeat, interpretSeat, pipelineSeat)
import Free.Agent.Syntax (FreeAgent (..))
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

mkPost :: Text -> [Text] -> Text -> Post
mkPost = Post

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

  -------------------------------------------------------------------------
  -- Pipeline laws (on the pure fold)
  -------------------------------------------------------------------------
  putStrLn "Pipeline laws"

  do
    let f = filterP even :: Pipeline Int Int
        g = mapP (* 3) :: Pipeline Int Int
        h = routeP (\n -> [n, n + 1]) :: Pipeline Int Int
        leftA = h `P.Compose` (g `P.Compose` f)
        rightA = (h `P.Compose` g) `P.Compose` f
        xs = [1 .. 6 :: Int]
    assert "pipeline Compose associative under runPipeline" $
      runPipeline leftA xs == runPipeline rightA xs
    assert "filter then map" $
      runPipeline (g `P.Compose` f) xs == [6, 12, 18]

  -------------------------------------------------------------------------
  -- Pipeline round-trip into Shard (State [Post]) [Post] [Post]
  -------------------------------------------------------------------------
  putStrLn "Pipeline round-trip"

  do
    let p :: Pipeline Post Post
        -- Category-style: map after filter
        p = mapP (\x -> x {body = "map:" <> body x}) `P.Compose` filterP (\x -> body x /= "noise")
        posts =
          [ mkPost "human" ["bot"] "hello",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "world"
          ]
        sh :: Shard (State [Post]) [Post] [Post]
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
    let h = Host {hostName = "echo", hostRun = pure . map ("echo:" <>)}
        sh :: Shard (State [Post]) [Post] [Post]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi there"
        (outs, st) = closeShard sh [p] []
    assert "host replies to sender" $
      map body outs == ["echo:hi", "echo:there"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
    assert "host buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- FreeSeat multi-stage close: pipeline + host composed as seat terms
  -------------------------------------------------------------------------
  putStrLn "FreeSeat multi-stage close"

  do
    let p :: Pipeline Post Post
        p = mapP (\x -> x {body = "map:" <> body x}) `P.Compose` filterP (\x -> body x /= "noise")
        h = Host {hostName = "echo", hostRun = pure . map ("echo:" <>)}
        seat :: FreeSeat
        seat = SeatCompose (hostSeat h) (pipelineSeat p)
        sh :: Shard (State [Post]) [Post] [Post]
        sh = interpretSeat seat
        posts =
          [ mkPost "human" ["bot"] "hello world",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "again"
          ]
        (outs, st) = closeShard sh posts []
    assert "free seat composed pipeline then host" $
      map body outs == ["echo:map:hello", "echo:world"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
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
        p = routeTo "router" :: Pipeline Post Post
    assert "routeTo sets single recipient" $
      all (\x -> to x == ["router"]) (runPipeline p posts)

  do
    let posts =
          [ mkPost "human" [] "one",
            mkPost "human" [] "two"
          ]
        p = broadcast ["a", "b"] :: Pipeline Post Post
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
        sh :: Shard (StateT [Post] IO) [Post] [Post]
        sh = hostShard h
        p = mkPost "human" ["shell"] "ignored"
    (outs, st) <- closeShardIO sh [p] []
    assert "process host runs echo and replies" $
      map body outs == ["hello ignored"] && all (\x -> to x == ["human"]) outs
    assert "process host buffer cleared after emit" $ st == []

  putStrLn "All tests passed"
