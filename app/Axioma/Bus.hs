{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | bus stats + seats: supervision, self-halt, multi-seat card meetings,
-- reply audience (R2b), reply ancestry (M1a), the seam (Q7).
module Axioma.Bus
  ( busTopic,
  )
where

import Axioma.Common (assert, closeBusDurable, mkPost)
import Circuit.Agent (Agent, Post (..), PostId, cellToAgent, loop, tape, watch)
import Circuit.Agent.Framing (parseTimeText, stamp, stamped, unframeStored, pattern Stamped)
import Circuit.Agent.Mark (Mark (..), isEscalate, isHalt, markGlyph, markOf)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus (openBus, postLocal, runSeatBus)
import Free.Agent.Bus.File (QuiesceConfig (..), writeCursor)
import Free.Agent.BusStats
  ( Classification (..),
    SliceMode (..),
    Stats (..),
    classify,
    computeStats,
    defaultRules,
    isDoneClaim,
    slicePosts,
  )
import Free.Agent.Host (mkHost)
import Free.Agent.Seat (hostSeat)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

busTopic :: IO ()
busTopic = do
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
          Just ts' -> Stamped (ts', i) (Post f t [] body)
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
          Just ts' -> Stamped (ts', i) (Post f t [] body)
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
          Just ts' -> Stamped (ts', i) (Post f t [] body)
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
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        fromEcho = [stamped s | s <- parsed, from (stamped s) == "echo"]
    assert "seat replied to the work post with a thread edge" $
      case fromEcho of
        [p] -> body p == "echo:hello" && to p == ["human"] && thread p == [0]
        _ -> False
    assert "ids are coherent across postLocal and the seat scribe" $
      map (snd . stamp) parsed == [0, 1, 2]
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
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
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
      map (snd . stamp) parsed == [0, 1, 2, 3, 4, 5, 6]

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
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        fromEcho = [stamped s | s <- parsed, from (stamped s) == "echo"]
    assert "a 🟢 reply lands the exchange but the seat hosts more" $
      any (\p -> body p == markGlyph Landed <> " task done" && thread p == [0]) fromEcho
        && any (\p -> body p == "echo:hello" && thread p == [1]) fromEcho
    assert "the seat posts its own stand-down" $
      any (\p -> markOf p == Just StandDown) fromEcho
    assert "no reply after the stand-down (tail-chatter)" $
      not (any (\p -> "ping" `T.isInfixOf` body p) fromEcho)
    assert "ids are coherent: 5 planted + 3 replies, then quiet" $
      map (snd . stamp) parsed == [0, 1, 2, 3, 4, 5, 6, 7]

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
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
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
      map (snd . stamp) parsed == [0, 1, 2, 3]
    assert "the halt mark got silence from every seat" $
      length (fromAlpha ++ fromBeta) == 2

  -------------------------------------------------------------------------
  -- Reply audience: one spelling, branching factor 1 (R2b)
  --
  -- The card's default: a seat's reply addresses the sender only — the
  -- "all" magic top is localised by 'deliversTo', never propagated.  A
  -- reply carrying "all" forward would re-broadcast (the amplifier the
  -- design card flags), so a wire-preserving "convergence" of the
  -- audience rule goes red here.
  -------------------------------------------------------------------------
  putStrLn "reply audience"
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-r2b-audience"
    removePathForcibly root
    createDirectoryIfMissing True root
    _ <- postLocal root (mkPost "human" ["all"] "ping")
    _ <- postLocal root (mkPost "human" ["aud"] (markGlyph Landed <> " done"))
    bus <- openBus root
    runSeatBus bus "aud" ["aud"] (hostSeat (mkHost "aud" (pure . map ("ack: " <>))))
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "aud"]
    assert "R2b a reply to an all-post addresses the sender only (no amplifier)" $
      case replies of
        [r] -> body r == "ack: ping" && to r == ["human"]
        _ -> False
    removePathForcibly root

  -------------------------------------------------------------------------
  -- Reply ancestry (M1a): every post produced in response to a delivery
  -- cites that delivery. runAgentLoop posts handler replies as-is —
  -- these oracles pin the thread edge on the production path.
  -- Counterexample under the old code: thread == [].
  -------------------------------------------------------------------------
  putStrLn "reply ancestry (M1a)"

  -- A handler reply cites the delivered ask. The seat starts first; the
  -- asks are planted after (a cold-boot cursor would skip pre-planted
  -- history — production seats do not answer the past).
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-m1a-ancestry"
        path = root </> "log.jsonl"
        plant = do
          _ <- postLocal root (mkPost "human" ["m1a"] "ask")
          postLocal root (mkPost "human" ["m1a"] (markGlyph Landed <> " done"))
    removePathForcibly root
    createDirectoryIfMissing True root
    done <- newEmptyMVar
    _ <-
      forkIO $
        runAgentLoop
          "m1a"
          ["m1a"]
          root
          Nothing
          ( \stored ->
              pure [mkPost "m1a" [from (stamped stored)] ("ack: " <> body (stamped stored))]
          )
          >> putMVar done ()
    _ <- plant
    takeMVar done
    content <- TIO.readFile path
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "m1a"]
    assert "M1a a runAgentLoop reply cites the delivered ask's id" $
      case replies of
        [r] -> body r == "ack: ask" && thread r == [0]
        _ -> False
    removePathForcibly root

  -- A handler exception produces a 🔴 citing the ask.
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-m1a-escalation"
        path = root </> "log.jsonl"
        plant = do
          _ <- postLocal root (mkPost "human" ["m1x"] "boom-ask")
          postLocal root (mkPost "human" ["m1x"] (markGlyph Landed <> " done"))
    removePathForcibly root
    createDirectoryIfMissing True root
    done <- newEmptyMVar
    _ <-
      forkIO $
        runAgentLoop "m1x" ["m1x"] root Nothing (\_ -> error "handler exploded")
          >> putMVar done ()
    _ <- plant
    takeMVar done
    content <- TIO.readFile path
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        escalations =
          [ stamped s
          | s <- parsed,
            from (stamped s) == "m1x",
            maybe False isEscalate (markOf (stamped s))
          ]
    assert "M1a a handler failure produces a 🔴 citing the ask" $
      case escalations of
        [e] -> thread e == [0] && "handler exploded" `T.isInfixOf` body e
        _ -> False
    removePathForcibly root

  -- An incoming 🔴 relayed to the pitboss cites the escalation it answers.
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-m1a-relay"
        path = root </> "log.jsonl"
    removePathForcibly root
    createDirectoryIfMissing True root
    done <- newEmptyMVar
    -- The 🔴 is control: the handler must never run (it would error and
    -- post a second escalation, breaking the singleton assertion below).
    _ <-
      forkIO $
        runAgentLoop
          "m1r"
          ["m1r"]
          root
          (Just (QuiesceConfig 3 "boss" 1_000_000))
          ( \_ ->
              error "handler should never run on an escalation mark"
          )
          >> putMVar done ()
    _ <- postLocal root (mkPost "human" ["m1r"] (markGlyph Escalate <> " help"))
    takeMVar done
    content <- TIO.readFile path
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        relays =
          [ stamped s
          | s <- parsed,
            from (stamped s) == "m1r",
            to (stamped s) == ["boss"]
          ]
    assert "M1a the escalation relay to the pitboss cites the ask" $
      case relays of
        [r] -> thread r == [0] && "escalation received" `T.isInfixOf` body r
        _ -> False
    removePathForcibly root

  -------------------------------------------------------------------------
  -- The seam (Q7): one deterministic seat, two machines.
  --
  -- The same index-citing seat runs through the pure calculus 'loop'
  -- (circuits-agent) and through the operational 'runAgentLoop'
  -- (free-agent).  The pure seat cites its parent's log index; the
  -- runner prepends the parent's file id; the B3 id-agreement makes
  -- those the same number.  The final logs must be the SAME
  -- [Post Text] once stamps are stripped — no modulo left in the
  -- comparison.
  --
  -- Counterexamples this pins: a runner that drops the ancestry edge
  -- (pre-M1 shape, thread == []), a cold-boot cursor that skips the
  -- script (the seat answers nothing), a seat that answers the halt
  -- mark, and a silent pair of empty logs (the non-triviality clause).
  -------------------------------------------------------------------------
  putStrLn "the seam (Q7)"
  do
    let seatName = "q7" :: Text
        -- Pure side: the tape sees the whole addressed history
        -- (post-step observation), so the parent's log index is
        -- length hist - 1.  Halt marks are control: silence, and the
        -- loop quiesces.
        q7Seat :: [Post Text] -> [Post Text]
        q7Seat hist =
          let p = case hist of
                (x : _) -> x
                [] -> error "Q7: empty history"
              parentId = fromIntegral (length hist - 1) :: PostId
           in case markOf p of
                Just m
                  | isHalt m -> []
                _ -> [(mkPost seatName [from p] ("ack: " <> body p)) {thread = [parentId]}]
        agent = cellToAgent (tape q7Seat) :: Agent (->) [Post Text] (Post Text) [Post Text]
        a = mkPost "human" [seatName] "one"
        b = mkPost "human" [seatName] "two"
        c = mkPost "human" [seatName] "three"
        stopper = mkPost "human" [seatName] (markGlyph Landed <> " done")
        -- The pure Log is newest-first (post = cons); seeding it
        -- newest-first makes the whole lifecycle consistent, and
        -- 'watch' is the calculus's own oldest-first reading.
        (_, pureLog, _) = loop [(seatName, agent)] [stopper, c, b, a]
        pureTimeline = watch ["human", seatName] pureLog
    -- Operational side: the cursor is pinned to 0 up front — the
    -- cold-boot default (latestPostId) would skip the script, which is
    -- exactly the divergence this test exists to catch.
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-q7-seam"
    removePathForcibly root
    createDirectoryIfMissing True root
    writeCursor root seatName 0
    done <- newEmptyMVar
    _ <-
      forkIO $
        runAgentLoop
          seatName
          [seatName]
          root
          Nothing
          ( \stored ->
              pure [mkPost seatName [from (stamped stored)] ("ack: " <> body (stamped stored))]
          )
          >> putMVar done ()
    _ <- postLocal root a
    _ <- postLocal root b
    _ <- postLocal root c
    _ <- postLocal root stopper
    takeMVar done
    content <- TIO.readFile (root </> "log.jsonl")
    let fileLog = map stamped (mapMaybe (unframeStored @Text) (T.lines content))
    assert "Q7 the run produced the full script plus replies (no silent pair)" $
      length fileLog == 7 && length pureTimeline == 7
    assert "Q7 the seam: loop and runAgentLoop give the same final log modulo stamps" $
      pureTimeline == fileLog
    removePathForcibly root
