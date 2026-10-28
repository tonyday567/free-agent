{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | bus stats + seats: supervision, self-halt, multi-seat card meetings,
-- reply audience (R2b), reply ancestry (M1a), the seam (Q7).
module Axioma.Bus
  ( busTopic,
  )
where

import Axioma.Common (assert, closeBusDurable, mkPost, tempRoot)
import Circuit.Agent (Agent, Post (..), PostId, cellToAgent, loop, mkChannelPost, tape, watch)
import Circuit.Agent.Framing (Stamped, parseTimeText, stamp, stamped, unframeStored, pattern Stamped)
import Circuit.Agent.Mark (Mark (..), isEscalate, isHalt, markGlyph, markOf)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus (openBus, postLocal, readFileStrict, runSeatBus)
import Free.Agent.Bus.Cli (Depth (..), hbVerdict, readyPosts, readyWarnings, runStatus)
import Free.Agent.Bus.File (QuiesceConfig (..), readCursor, writeCursor)
import Free.Agent.Bus.Heartbeat (HbState (..), hbPath, parseHb, readHeartbeat, writeHeartbeat)
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
import Free.Agent.Delivery (Invocation (..), Turn, TurnResult (..), deliverTo, newDeliveryGrant, replyAct)
import Free.Agent.Host (hostShard, mkHost)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hFlush, stdout, withFile)
import System.Posix.Process (getProcessID)
import System.Process (createProcess, getPid, proc, waitForProcess)

ioTurn :: IO [Post Text] -> IO (Turn [Post Text])
ioTurn action =
  deliverTo
    (\_ _ -> pure Invocation {runInvocation = TurnResponse <$> action, cancelInvocation = pure False})
    ()
    ""

runTestAgent :: Text -> [Text] -> FilePath -> Maybe QuiesceConfig -> (Stamped Text -> IO (Turn [Post Text])) -> IO ()
runTestAgent agentName names root mQuiesce handle = do
  grant <- newDeliveryGrant 100
  runAgentLoop agentName names root mQuiesce grant handle

captureLiveStatus :: FilePath -> IO Text
captureLiveStatus root = do
  hFlush stdout
  bracket (hDuplicate stdout) restore $ \_ -> do
    withFile (root </> "status.out") WriteMode $ \h -> do
      hDuplicateTo h stdout
      runStatus root 0 True (Depth 1)
      hFlush stdout
    readFileStrict (root </> "status.out")
  where
    restore saved = do
      hFlush stdout
      hDuplicateTo saved stdout
      hClose saved

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
          Just ts' -> Stamped (ts', i) (Post f Nothing t [] body)
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
          Just ts' -> Stamped (ts', i) (Post f Nothing t [] body)
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
          Just ts' -> Stamped (ts', i) (Post f Nothing t [] body)
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

  do
    let stored i ancestors = case parseTimeText "2026-10-28T00:00:00" of
          Just ts -> Stamped (ts, i) (Post "moka" Nothing [] ancestors "cycle fixture")
          Nothing -> error "bad thread-cycle fixture timestamp"
        posts =
          [ stored 1 [2],
            stored 2 [3],
            stored 3 [2],
            stored 7 [],
            stored 8 [7],
            stored 9 [9]
          ]
        slices = slicePosts ByThread posts
        componentIds label = maybe [] (map (snd . stamp)) (lookup label slices)
    -- Mutation: removing cycle detection makes this oracle non-terminating.
    assert "a two-post ancestry cycle keeps its child in one component" $ componentIds "2" == [3, 2, 1]
    assert "ordinary ancestry still resolves to its root" $ componentIds "7" == [8, 7]
    assert "a self-ancestor is its own root" $ componentIds "9" == [9]
    assert "thread slicing accounts for every cyclic and ordinary post" $ length slices == 3

  -------------------------------------------------------------------------
  -- Idle age is not a crash signal; pid evidence separates parked from dead.
  -------------------------------------------------------------------------
  putStrLn "bus heartbeat liveness"
  do
    root <- tempRoot "free-agent-heartbeat"
    now <- getCurrentTime
    pid <- getProcessID
    let old = addUTCTime (-3600) now
    writeHeartbeat root "live" Idle
    current <- readHeartbeat root "live"
    assert "heartbeat writer records the seat pid" $
      case current of
        Just (Idle, _, Just writtenPid) -> writtenPid == pid
        _ -> False
    assert "legacy heartbeat files still parse without a pid" $
      case parseHb "idle 2026-10-28T00:00:00Z" of
        Just (Idle, _, Nothing) -> True
        _ -> False
    (idleDot, idleText) <- hbVerdict 900 now (Just (Idle, old, Just pid))
    assert "a live idle seat stays green beyond the staleness threshold" $
      idleDot == "🟢" && not ("crashed?" `T.isInfixOf` idleText)
    (legacyDot, _) <- hbVerdict 900 now (Just (Idle, old, Nothing))
    assert "an unverifiable stale legacy idle reads unknown, not live" $ legacyDot == "⚪"
    (workingDot, workingText) <- hbVerdict 900 now (Just (Working, old, Just pid))
    -- Mutation: restoring the stale-non-idle crash branch fails this oracle.
    assert "a stale working seat with a live pid reads stale, not crashed" $
      workingDot == "🟡" && "stale?" `T.isInfixOf` workingText && not ("crashed?" `T.isInfixOf` workingText)
    (legacyWorkingDot, legacyWorkingText) <- hbVerdict 900 now (Just (Working, old, Nothing))
    -- Mutation: treating missing pid evidence as a crash fails this oracle.
    assert "a stale legacy working seat keeps liveness unknown" $
      legacyWorkingDot == "⚪" && "stale?" `T.isInfixOf` legacyWorkingText && not ("crashed?" `T.isInfixOf` legacyWorkingText)
    (freshDot, freshText) <- hbVerdict 900 now (Just (Working, now, Just pid))
    -- Mutation: ignoring heartbeat age and warning on every live pid fails this.
    assert "a fresh working seat reads working without a staleness warning" $
      freshDot == "🟡" && not ("stale?" `T.isInfixOf` freshText) && not ("crashed?" `T.isInfixOf` freshText)
    (_, _, _, ph) <- createProcess (proc "sleep" ["0.01"])
    mpid <- getPid ph
    _ <- waitForProcess ph
    case mpid of
      Nothing -> assert "dead-pid oracle acquired a child pid" False
      Just deadPid -> do
        (deadDot, deadText) <- hbVerdict 900 now (Just (Working, old, Just deadPid))
        -- Mutation: dropping dead-pid precedence turns this into a stale warning.
        assert "a stale working heartbeat with a dead pid reads crashed" $
          deadDot == "🔴" && "crashed?" `T.isInfixOf` deadText
    (retiredDot, retiredText) <- hbVerdict 900 now (Just (Dead, old, Just pid))
    -- Mutation: treating explicit dead as an ordinary stale live state fails this.
    assert "an explicit dead marker reads retired" $
      retiredDot == "🔴" && "retired" `T.isInfixOf` retiredText
    removePathForcibly root

  -------------------------------------------------------------------------
  -- Active live status: only explicit retirement leaves the projection.
  -------------------------------------------------------------------------
  putStrLn "bus active live status"
  do
    root <- tempRoot "free-agent-active-status"
    _ <- postLocal root (mkPost "retired" [] "retired historical evidence")
    _ <- postLocal root (mkPost "participant" [] "participant historical evidence")
    writeHeartbeat root "idle" Idle
    writeHeartbeat root "stale" Working
    writeHeartbeat root "retired" Dead
    writeCursor root "unknown" 0
    now <- getCurrentTime
    (_, _, _, ph) <- createProcess (proc "sleep" ["0.01"])
    mpid <- getPid ph
    _ <- waitForProcess ph
    case mpid of
      Nothing -> assert "active-status oracle acquired a child pid" False
      Just deadPid ->
        TIO.writeFile
          (hbPath root "crashed")
          ("working " <> T.pack (iso8601Show now) <> " " <> T.pack (show deadPid) <> "\n")
    before <- readFileStrict (root </> "log.jsonl")
    report <- captureLiveStatus root
    after <- readFileStrict (root </> "log.jsonl")
    let seatLines name = [line | line <- T.lines report, (" " <> name <> " —") `T.isInfixOf` line]
    -- Mutation: dropping the explicit-dead filter fails this oracle.
    assert "an explicitly retired seat leaves the default live view" $ null (seatLines "retired")
    -- Mutation: filtering idle seats or dropping live-pid evidence fails this oracle.
    assert "a live idle seat stays visible and green" $
      case seatLines "idle" of
        [line] -> "🟢" `T.isPrefixOf` line && not ("stale?" `T.isInfixOf` line)
        _ -> False
    -- Mutation: treating stale work as retirement or a crash fails this oracle.
    assert "a stale working seat with a live pid stays visible as stale" $
      case seatLines "stale" of
        [line] -> "🟡" `T.isPrefixOf` line && "stale?" `T.isInfixOf` line && not ("crashed?" `T.isInfixOf` line)
        _ -> False
    -- Mutation: filtering every dead pid rather than only explicit retirement fails this oracle.
    assert "a dead pid without explicit retirement stays visible as crashed" $
      case seatLines "crashed" of
        [line] -> "🔴" `T.isPrefixOf` line && "crashed?" `T.isInfixOf` line && not ("retired" `T.isInfixOf` line)
        _ -> False
    -- Mutation: requiring a heartbeat for inclusion fails this oracle.
    assert "a cursor-only unknown seat stays visible" $
      case seatLines "unknown" of
        [line] -> "⚪" `T.isPrefixOf` line && "no heartbeat" `T.isInfixOf` line
        _ -> False
    -- Mutation: dropping historical participants indiscriminately fails this oracle.
    assert "a participant without heartbeat or cursor stays visible" $
      case seatLines "participant" of
        [line] -> "⚪" `T.isPrefixOf` line && "no heartbeat" `T.isInfixOf` line
        _ -> False
    assert "the presentational filter leaves historical log evidence untouched" $ before == after
    retiredHeartbeat <- readHeartbeat root "retired"
    assert "the presentational filter leaves the explicit-dead heartbeat untouched" $
      case retiredHeartbeat of
        Just (Dead, _, _) -> True
        _ -> False
    removePathForcibly root

  -------------------------------------------------------------------------
  -- READY is a pure reader over the log, not a second task store.
  -------------------------------------------------------------------------
  putStrLn "bus task READY view"
  do
    let stored i f audience ancestors text = case parseTimeText "2026-10-28T00:00:00" of
          Just ts -> Stamped (ts, i) (Post f Nothing audience ancestors text)
          Nothing -> error "bad READY fixture timestamp"
        readyIds = map (snd . stamp) . readyPosts
        posts =
          [ stored 0 "moka" [] [] "TASK · open offer",
            stored 1 "moka" [] [] "TASK · claimed offer",
            stored 2 "mucho" ["moka"] [1] "CLAIM · task 1",
            stored 3 "moka" ["mucho"] [] "TASK · assigned work",
            stored 4 "moka" [] [] "TASK · completed offer",
            stored 5 "mucho" ["moka"] [4] "CLAIM · task 4",
            stored 6 "mucho" ["moka"] [4] "DONE · task 4",
            stored 7 "moka" [] [] "TASK · blocked offer",
            stored 8 "moka" [] [] "LINK · 0 blocks 7",
            stored 9 "moka" [] [] "TASK · resolved offer",
            stored 10 "moka" ["mucho"] [] "TASK · assigned blocker",
            stored 11 "mucho" ["moka"] [10] "DONE · task 10",
            stored 12 "moka" [] [] "LINK · 10 blocks 9",
            stored 13 "mucho" ["moka"] [0] "NOTE · unrelated reply",
            stored 14 "moka" [] [] "TASKED · malformed prose",
            stored 15 "moka" [] [] "LINK · malformed dependency"
          ]
    assert "READY contains only unclaimed open offers with resolved blockers" $ readyIds posts == [0, 9]
    assert "a CLAIM removes an open offer" $ 1 `notElem` readyIds posts
    assert "a directed TASK is assigned rather than READY" $ 3 `notElem` readyIds posts
    assert "an owner DONE removes an open offer" $ 4 `notElem` readyIds posts
    assert "an incomplete blocker withholds READY" $ 7 `notElem` readyIds posts
    assert "a completed blocker releases READY" $ 9 `elem` readyIds posts
    assert "unrelated replies and malformed prose do not change READY" $ readyIds posts == readyIds (take 13 posts)

  do
    let stored i f audience ancestors text = case parseTimeText "2026-10-28T00:00:00" of
          Just ts -> Stamped (ts, i) (Post f Nothing audience ancestors text)
          Nothing -> error "bad READY conflict fixture timestamp"
        readyIds = map (snd . stamp) . readyPosts
        posts =
          [ stored 20 "moka" [] [] "TASK · contested blocker",
            stored 21 "mucho" ["moka"] [20] "CLAIM · task 20",
            stored 22 "stinky" ["moka"] [20] "CLAIM · task 20",
            stored 23 "stinky" ["moka"] [20] "DONE · task 20",
            stored 24 "moka" [] [] "TASK · dependent offer",
            stored 25 "moka" [] [] "LINK · 20 blocks 24"
          ]
    assert "a later claimer cannot complete the first claimer's task" $ readyIds posts == []
    assert "the first claimer's DONE resolves the dependent offer" $
      readyIds (posts <> [stored 26 "mucho" ["moka"] [20] "DONE · task 20"]) == [24]

  do
    let stored i f audience ancestors text = case parseTimeText "2026-10-28T00:00:00" of
          Just ts -> Stamped (ts, i) (Post f Nothing audience ancestors text)
          Nothing -> error "bad ancestry fixture timestamp"
        readyIds = map (snd . stamp) . readyPosts
        offer = stored 30 "moka" [] [] "TASK · parcel alpha"
        claimInside = stored 31 "mucho" ["moka"] [30] "CLAIM · parcel alpha"
        claimOutside = stored 31 "mucho" ["moka"] [] "CLAIM · parcel alpha"
        dependent = stored 32 "moka" [] [] "TASK · dependent parcel"
        dependency = stored 33 "moka" [] [] "LINK · 30 blocks 32"
        doneInside = stored 34 "mucho" ["moka"] [30] "DONE · parcel alpha"
        doneOutside = stored 34 "mucho" ["moka"] [] "DONE · parcel alpha"
        withDone donePost = [offer, claimInside, dependent, dependency, donePost]
        namesPostAndOffer warning =
          "post 31" `T.isInfixOf` warning
            && "offer 30" `T.isInfixOf` warning
            && "--thread 30" `T.isInfixOf` warning
    assert "an inside CLAIM removes its offer from READY" $
      readyIds [offer, claimInside] == []
    assert "an outside CLAIM leaves its offer READY" $
      readyIds [offer, claimOutside] == [30]
    assert "an inside CLAIM earns no ancestry warning" $
      null (readyWarnings [offer, claimInside])
    assert "an outside CLAIM names the post and resembling offer" $
      case readyWarnings [offer, claimOutside] of
        [warning] -> namesPostAndOffer warning
        _ -> False
    assert "an unrelated outside CLAIM earns no spurious offer warning" $
      null (readyWarnings [offer, stored 35 "mucho" ["moka"] [] "CLAIM · unrelated"])
    assert "an owner DONE inside the component releases a dependent offer" $
      readyIds (withDone doneInside) == [32]
    assert "an owner DONE outside the component leaves the dependent offer blocked" $
      readyIds (withDone doneOutside) == []
    assert "inside CLAIM and DONE earn no ancestry warnings beside another open offer" $
      null (readyWarnings (withDone doneInside))
    assert "an outside DONE names its post and resembling offer" $
      case readyWarnings (withDone doneOutside) of
        [warning] ->
          "post 34" `T.isInfixOf` warning
            && "offer 30" `T.isInfixOf` warning
            && "--thread 30" `T.isInfixOf` warning
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
    root <- tempRoot "free-agent-seat-axioma"
    _ <- postLocal root (mkPost "human" ["echo"] "hello")
    _ <- postLocal root (mkPost "human" ["echo"] "🟢 landed")
    bus <- openBus root
    runSeatBus bus "echo" ["echo"] (hostShard (mkHost "echo" (pure . map ("echo:" <>))))
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
    root <- tempRoot "free-agent-supervision-axioma"
    _ <- postLocal root (mkPost "human" ["fragile"] "hello")
    _ <- postLocal root (mkPost "human" ["fragile"] "boom")
    _ <- postLocal root (mkPost "human" ["fragile"] "world")
    _ <- postLocal root (mkPost "human" ["fragile"] "🟢 landed")
    bus <- openBus root
    let fragileHost = mkHost "fragile" $ \ws ->
          if ws == ["boom"]
            then error "boom"
            else pure (map ("fragile:" <>) ws)
    runSeatBus bus "fragile" ["fragile"] (hostShard fragileHost)
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
    root <- tempRoot "free-agent-selfhalt-axioma"
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
    runSeatBus bus "echo" ["echo"] (hostShard host)
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
  -- Multi-seat card meeting: two seat shards share a subscription on the bus
  --
  -- A seed addressed to a shared card wakes every seat; each replies to the
  -- original sender.  A pre-planted halt mark stops all seats once they have
  -- processed the seed.
  -------------------------------------------------------------------------
  putStrLn "multi-seat card meeting"
  do
    root <- tempRoot "free-agent-meeting-axioma"
    let card = "panel"
    _ <- postLocal root (mkPost "human" [card] "discuss")
    _ <- postLocal root (mkPost "human" [card] "🟢 landed")
    bus <- openBus root
    let alphaSeat = hostShard (mkHost "alpha" (pure . map ("alpha:" <>)))
        betaSeat = hostShard (mkHost "beta" (pure . map ("beta:" <>)))
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
  -- The card's default: a seat's reply lands in the channel it answers and
  -- addresses the sender only — the channel is inherited, the recipient list
  -- is not widened.  A reply carrying the channel's membership forward would
  -- re-wake every member (the amplifier the design card flags), so a
  -- wire-preserving "convergence" of the audience rule goes red here.
  -------------------------------------------------------------------------
  putStrLn "reply audience"
  do
    root <- tempRoot "free-agent-r2b-audience"
    _ <- postLocal root (mkChannelPost "human" "commons" ["human", "aud"] ("ping" :: Text))
    _ <- postLocal root (mkPost "human" ["aud"] (markGlyph Landed <> " done"))
    bus <- openBus root
    runSeatBus bus "aud" ["commons", "aud"] (hostShard (mkHost "aud" (pure . map ("ack: " <>))))
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "aud"]
    assert "R2b a reply lands in the channel and addresses the sender only (no amplifier)" $
      case replies of
        [r] -> body r == "ack: ping" && to r == ["human"] && channel r == Just "commons"
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
    root <- tempRoot "free-agent-m1a-ancestry"
    let path = root </> "log.jsonl"
        plant = do
          _ <- postLocal root (mkPost "human" ["m1a"] "ask")
          postLocal root (mkPost "human" ["m1a"] (markGlyph Landed <> " done"))
    -- Pin the cursor to 0 up front (the Q7 idiom): fork-first + plant is
    -- a scheduling race under -threaded — if the plant wins, the seat's
    -- latestPostId cold-boot skips the script.
    writeCursor root "m1a" 0
    done <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent
          "m1a"
          ["m1a"]
          root
          Nothing
          ( \stored ->
              ioTurn (pure [mkPost "m1a" [from (stamped stored)] ("ack: " <> body (stamped stored))])
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
    root <- tempRoot "free-agent-m1a-escalation"
    let path = root </> "log.jsonl"
        plant = do
          _ <- postLocal root (mkPost "human" ["m1x"] "boom-ask")
          postLocal root (mkPost "human" ["m1x"] (markGlyph Landed <> " done"))
    -- Pin the cursor to 0 up front (the Q7 idiom; see the ancestry test).
    writeCursor root "m1x" 0
    done <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent "m1x" ["m1x"] root Nothing (\_ -> ioTurn (error "handler exploded"))
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
    root <- tempRoot "free-agent-m1a-relay"
    let path = root </> "log.jsonl"
    -- Pin the cursor to 0 up front (the Q7 idiom; see the ancestry test).
    writeCursor root "m1r" 0
    done <- newEmptyMVar
    -- The 🔴 is control: the handler must never run (it would error and
    -- post a second escalation, breaking the singleton assertion below).
    _ <-
      forkIO $
        runTestAgent
          "m1r"
          ["m1r"]
          root
          (Just (QuiesceConfig 3 "boss" 1_000_000))
          ( \_ ->
              ioTurn (error "handler should never run on an escalation mark")
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
    root <- tempRoot "free-agent-q7-seam"
    let logPath = root </> "log.jsonl"
    writeCursor root seatName 0
    -- The pure calculus feeds 'loop' the entire tape in one batch: the
    -- seat sees all four asks at once (replies carry ask-indices, not
    -- file ids) and the acks come out only at the end. The operational
    -- driver must reproduce exactly that shape: start the seat inside
    -- the first postLocal's flock window (before it appends) and gate
    -- it on the full plant, so the gap-drain cannot see a partial
    -- script — it either runs before any line exists (the watcher then
    -- delivers everything) or after the whole append. A fork-first or
    -- turn-taking driver is a scheduling race under -threaded.
    done <- newEmptyMVar
    planted <- newEmptyMVar
    _ <- postLocal root a
    _ <-
      forkIO $ do
        takeMVar planted
        runTestAgent
          seatName
          [seatName]
          root
          Nothing
          ( \stored ->
              ioTurn (pure [mkPost seatName [from (stamped stored)] ("ack: " <> body (stamped stored))])
          )
        putMVar done ()
    _ <- postLocal root b
    _ <- postLocal root c
    _ <- postLocal root stopper
    putMVar planted ()
    takeMVar done
    content <- readFileStrict logPath
    let fileLog = map stamped (mapMaybe (unframeStored @Text) (T.lines content))
    assert "Q7 the run produced the full script plus replies (no silent pair)" $
      length fileLog == 7 && length pureTimeline == 7
    assert "Q7 the seam: loop and runAgentLoop give the same final log modulo stamps" $
      pureTimeline == fileLog
    removePathForcibly root

  putStrLn "explicit reply acts"
  do
    root <- tempRoot "free-agent-reply-act"
    writeCursor root "quiet" 0
    done <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent
          "quiet"
          ["quiet"]
          root
          Nothing
          (\stored -> ioTurn (pure (replyAct "quiet" stored "plain local response")))
          >> putMVar done ()
    causing <- postLocal root (mkPost "human" ["quiet"] "*(no post)*")
    _ <- postLocal root (mkPost "human" ["quiet"] (markGlyph Landed <> " done"))
    takeMVar done
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "quiet"]
    assert "plain response text publishes no reply act" $ replies == []
    let acts = replyAct "quiet" causing "@human: acknowledged"
    assert "an addressed response is a cited explicit reply" $
      case acts of
        [reply] ->
          to reply == ["human"]
            && body reply == "acknowledged"
            && thread reply == [snd (stamp causing)]
            && channel reply == channel (stamped causing)
        _ -> False
    removePathForcibly root

  putStrLn "runner delivery refusal"
  do
    root <- tempRoot "free-agent-runner-refusal"
    writeCursor root "bounded" 0
    grant <- newDeliveryGrant 0
    invocations <- newIORef (0 :: Int)
    done <- newEmptyMVar
    _ <-
      forkIO $
        runAgentLoop
          "bounded"
          ["bounded"]
          root
          Nothing
          grant
          (\_ -> ioTurn (modifyIORef' invocations (+ 1) >> pure []))
          >> putMVar done ()
    _ <- postLocal root (mkPost "human" ["bounded"] "must not invoke")
    takeMVar done
    invocationCount <- readIORef invocations
    cursor <- readCursor root "bounded"
    assert "an exhausted runner grant invokes no backend and leaves the cursor" $
      invocationCount == 0 && cursor == 0
    removePathForcibly root
