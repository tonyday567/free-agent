{-# LANGUAGE OverloadedStrings #-}

-- | pipeline + seat round-trips: pipeline laws, host/CLI seat sketches,
-- FreeSeat multi-stage close and Compose assoc.
module Axioma.Seat
  ( seatTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (Post (..))
import Circuit.Agent.Tensor (AgentShard, closeShardIO)
import Circuit.Category (Category (id, (.)))
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Cli (Cli (..), StderrPolicy (..), parseSessionId)
import Free.Agent.Host (BodyMode (..), Host (..), cliHost, hostShard, mkHost, processHost)
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
import Free.Agent.Seat (FreeSeat (..), hostSeat, interpretSeat, pipelineSeat)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Prelude hiding (id, (.))

seatTopic :: IO ()
seatTopic = do
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
  -- Pipeline round-trip into AgentShard [Post Text] [Post Text]
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
        sh :: AgentShard [Post Text] [Post Text]
        sh = pipelineShard p
    (outs, st) <- closeShardIO sh posts []
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
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi there"
    (outs, st) <- closeShardIO sh [p] []
    assert "host replies to sender" $
      map body outs == ["echo:hi", "echo:there"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
        && all (\x -> thread x == []) outs
    assert "host buffer cleared after emit" $ st == []

  do
    let h = mkHost "echo" (pure . map ("echo:" <>))
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        posts =
          [ mkPost "human" ["echo"] "one two",
            mkPost "human" ["echo"] "three"
          ]
    (outs, st) <- closeShardIO sh posts []
    assert "host emits one reply batch per committed post" $
      map body outs == ["echo:one", "echo:two", "echo:three"]
        && length outs == 3
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
    assert "multi-post host buffer cleared after emit" $ st == []

  do
    let h = (mkHost "echo" (pure . map ("echo:" <>))) {hostBodyMode = BodyLines}
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi\nthere"
    (outs, st) <- closeShardIO sh [p] []
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
        sh :: AgentShard [Post Text] [Post Text]
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
        sh :: AgentShard [Post Text] [Post Text]
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
  -- Real process host sharing a threaded IO buffer with pipeline stages
  -------------------------------------------------------------------------
  putStrLn "Real host in shared threaded IO buffer"

  do
    let p = forName "shell" :: Pipeline (Post Text) (Post Text)
        h = processHost "shell" "echo" ["ok"]
        seat :: FreeSeat
        seat = SeatCompose (hostSeat h) (pipelineSeat p)
        sh :: AgentShard [Post Text] [Post Text]
        sh = interpretSeat seat
        posts =
          [ mkPost "human" ["shell"] "world",
            mkPost "human" ["skip"] "no"
          ]
    (outs, st) <- closeShardIO sh posts []
    assert "process host shares threaded IO buffer with pipeline" $
      map body outs == ["ok world"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "shell") outs
    assert "shared buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Real-host sketch: live CLI seat (fake binary, exact oracle)
  -------------------------------------------------------------------------
  putStrLn "CLI host sketch"

  do
    dir <- tempRoot "free-agent-cli-axioma"
    let script = dir </> "fake-cli.sh"
        sf = dir </> "session"
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
        sh :: AgentShard [Post Text] [Post Text]
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
