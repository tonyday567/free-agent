{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | bus stats + seats: supervision, self-halt, multi-seat card meetings,
-- reply audience (R2b), reply ancestry (M1a), the seam (Q7).
module Axioma.Bus
  ( busTopic,
  )
where

import Axioma.Common (assert, closeBusDurable, mkPost, tempRoot)
import Circuit.Agent (Agent, Post (..), PostId, cellToAgent, deliversTo, loop, mkChannelPost, tape, watch)
import Circuit.Agent.Framing (Stamped, parseTimeText, stamp, stamped, unframeStored, pattern Stamped)
import Circuit.Agent.Mark (Mark (..), controlMark, isEscalate, isHalt, markGlyph, markOf)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, bracket, evaluate, try)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus (openBus, postLocal, readFileStrict, runSeatBus)
import Free.Agent.Bus.Cli (BusCommand (..), Depth (..), SeatCircle (..), addressConflict, applyPostFlags, busParser, checkThreadId, readyPosts, readyWarnings, runBusCommand, runStatus, seatCircle)
import Free.Agent.Bus.File (QuiesceConfig (..), readCursor, readTransaction, writeCursor, writeTransaction)
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
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, info, renderFailure)
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

runTestAgent :: Text -> [Text] -> FilePath -> Maybe QuiesceConfig -> ([Stamped Text] -> IO (Turn [Post Text])) -> IO ()
runTestAgent agentName names root mQuiesce handle = do
  grant <- newDeliveryGrant 100
  runAgentLoop agentName names root mQuiesce grant handle

captureStatus :: FilePath -> Bool -> Depth -> IO Text
captureStatus root org depth = do
  hFlush stdout
  bracket (hDuplicate stdout) restore $ \_ -> do
    withFile (root </> "status.out") WriteMode $ \h -> do
      hDuplicateTo h stdout
      runStatus root 0 depth org
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
  -- Heartbeat evidence maps to the six status circles.
  -------------------------------------------------------------------------
  putStrLn "bus heartbeat circles"
  do
    root <- tempRoot "free-agent-heartbeat"
    now <- getCurrentTime
    pid <- getProcessID
    let old = addUTCTime (-3600) now
        older = addUTCTime (-7200) now
        brief = addUTCTime (-1200) now
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
    idleCircle <- seatCircle 900 now (Just (Idle, brief, Just pid))
    assert "a live idle seat under the hour is ready" $ idleCircle == CircleReady
    parkedCircle <- seatCircle 900 now (Just (Idle, older, Just pid))
    assert "a live idle seat past the hour is parked" $ parkedCircle == CircleParked
    legacyCircle <- seatCircle 900 now (Just (Idle, old, Nothing))
    assert "an unverifiable stale legacy heartbeat is non-fab" $ legacyCircle == CircleNonFab
    wakeCircle <- seatCircle 900 now (Just (Wake, now, Just pid))
    assert "a starting seat is ready" $ wakeCircle == CircleReady
    workingCircle <- seatCircle 900 now (Just (Working, old, Just pid))
    assert "long work with a live pid is still working" $ workingCircle == CircleWorking
    legacyWorkingCircle <- seatCircle 900 now (Just (Working, old, Nothing))
    assert "stale legacy work has no fab liveness claim" $ legacyWorkingCircle == CircleNonFab
    freshCircle <- seatCircle 900 now (Just (Working, now, Just pid))
    assert "fresh work is working" $ freshCircle == CircleWorking
    absentCircle <- seatCircle 900 now Nothing
    assert "no heartbeat is non-fab" $ absentCircle == CircleNonFab
    (_, _, _, ph) <- createProcess (proc "sleep" ["0.01"])
    mpid <- getPid ph
    _ <- waitForProcess ph
    case mpid of
      Nothing -> assert "dead-pid oracle acquired a child pid" False
      Just deadPid -> do
        deadCircle <- seatCircle 900 now (Just (Working, old, Just deadPid))
        assert "a dead pid without explicit retirement is crashed" $ deadCircle == CircleCrashed
    retiredCircle <- seatCircle 900 now (Just (Dead, old, Just pid))
    assert "an explicit dead marker is retired" $ retiredCircle == CircleRetired
    removePathForcibly root

  -------------------------------------------------------------------------
  -- One status snapshot drives census, plain rows, and Org groups.
  -------------------------------------------------------------------------
  putStrLn "bus unified status"
  do
    root <- tempRoot "free-agent-unified-status"
    _ <- postLocal root (mkPost "retired" [] "retired historical evidence")
    _ <- postLocal root (mkPost "participant" [] "participant historical evidence")
    _ <- postLocal root (mkPost "alice" ["busy"] "old mail")
    _ <- postLocal root (mkPost "alice" ["busy"] "new mail")
    writeHeartbeat root "idle" Idle
    writeHeartbeat root "busy" Working
    writeHeartbeat root "retired" Dead
    writeCursor root "unknown" 0
    writeCursor root "busy" 3
    now <- getCurrentTime
    pid <- getProcessID
    (_, _, _, ph) <- createProcess (proc "sleep" ["0.01"])
    mpid <- getPid ph
    _ <- waitForProcess ph
    case mpid of
      Nothing -> assert "status oracle acquired a child pid" False
      Just deadPid ->
        TIO.writeFile
          (hbPath root "crashed")
          ("working " <> T.pack (iso8601Show now) <> " " <> T.pack (show deadPid) <> "\n")
    TIO.writeFile
      (hbPath root "parked")
      ("idle " <> T.pack (iso8601Show (addUTCTime (-7200) now)) <> " " <> T.pack (show pid) <> "\n")
    before <- readFileStrict (root </> "log.jsonl")
    cursorBefore <- readCursor root "busy"
    plain <- captureStatus root False (Depth 1)
    org <- captureStatus root True (Depth 1)
    census <- captureStatus root True (Depth 0)
    detailed <- captureStatus root False (Depth 2)
    after <- readFileStrict (root </> "log.jsonl")
    cursorAfter <- readCursor root "busy"
    let header = "🟣 1 🟢 1 🟡 1 🔴 1 ⚫ 1 ⚪ 3 | last [#3]"
        orgRows = [line | line <- T.lines org, "*** " `T.isPrefixOf` line]
    assert "plain census has exact circles, counts, and last anchor" $
      take 1 (T.lines plain) == [header]
    assert "Org census has exact circles, counts, and last anchor" $
      ("* " <> header) `T.isPrefixOf` org
    assert "plain rows carry circle, directed unread, and last post" $
      "🟣 busy (1) post [#3]" `elem` T.lines plain
    assert "Org rows carry directed unread, not raw cursor positions" $
      "*** busy (1) post [#3]" `elem` orgRows
    assert "explicitly retired membership remains visible" $
      "** ⚫" `elem` T.lines org && "*** retired (0) post [#0]" `elem` orgRows
    assert "a participant without a heartbeat is non-fab" $
      "*** participant (0) post [#1]" `elem` orgRows
    assert "a cursor-only name is non-fab" $
      "*** unknown (0) no posts" `elem` orgRows
    assert "a sender without a heartbeat is non-fab" $
      "*** alice (0) post [#3]" `elem` orgRows
    assert "all eight members occur exactly once" $ length orgRows == 8
    assert "the cursor summary and repeated state prose are gone" $
      not ("cursor summary" `T.isInfixOf` org)
        && not ("cursor [#" `T.isInfixOf` org)
        && not ("no heartbeat" `T.isInfixOf` org)
        && not (" — idle" `T.isInfixOf` org)
    assert "depth zero is exactly the census" $ T.lines census == ["* " <> header]
    assert "depth two retains raw diagnostic evidence" $
      "    cursor: 3" `elem` T.lines detailed
        && any ("    hb: working " `T.isPrefixOf`) (T.lines detailed)
        && any ("    log: " `T.isPrefixOf`) (T.lines detailed)
    assert "status reads leave the log untouched" $ before == after
    assert "status reads leave cursors untouched" $ cursorBefore == cursorAfter
    retiredHeartbeat <- readHeartbeat root "retired"
    assert "status reads leave retirement evidence untouched" $
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
    assert "an addressed outside CLAIM is a claim, not a silent no-op" $
      case readyWarnings [offer, stored 36 "mucho" ["moka"] [] "@moka: CLAIM · parcel alpha"] of
        [warning] ->
          "post 36" `T.isInfixOf` warning
            && "offer 30" `T.isInfixOf` warning
            && "--thread 30" `T.isInfixOf` warning
        _ -> False
    assert "the READY remedy names a command the same build has" $
      case readyWarnings [offer, claimOutside] of
        [warning] ->
          "bus post --from mucho --thread 30 --body CLAIM" `T.isInfixOf` warning
            && not ("bus say" `T.isInfixOf` warning)
    assert "the READY remedy says where to stand, since the default root is the cwd" $
      case readyWarnings [offer, claimOutside] of
        [warning] -> "from the bus root you queried" `T.isInfixOf` warning
        _ -> False

  -------------------------------------------------------------------------
  -- address conflict: a channel beside an addressee is refused by name,
  -- because delivery follows the channel and the addressee would vanish.
  -------------------------------------------------------------------------
  do
    assert "a channel beside an addressee is refused, naming both" $
      case addressConflict (Just "card:loom/runner-ux.md") ["scully"] of
        Just why ->
          "card:loom/runner-ux.md" `T.isInfixOf` why
            && "scully" `T.isInfixOf` why
            && "🔴" `T.isInfixOf` why
        Nothing -> False
    assert "a channel alone is an address" $ addressConflict (Just "card:x") [] == Nothing
    assert "an addressee alone is an address" $ addressConflict Nothing ["scully"] == Nothing
    assert "an empty --to list beside a channel is no address" $
      addressConflict (Just "card:x") ["", ""] == Nothing
    assert "an empty channel is no address" $ addressConflict (Just "") ["scully"] == Nothing

  -------------------------------------------------------------------------
  -- READY lint distance: a stray below its resembling offers must not
  -- underflow the Natural subtraction in the closest-id tie-break.
  -------------------------------------------------------------------------
  do
    let stored i f audience ancestors text = case parseTimeText "2026-10-28T00:00:00" of
          Just ts -> Stamped (ts, i) (Post f Nothing audience ancestors text)
          Nothing -> error "bad READY distance fixture timestamp"
        near = stored 42 "moka" [] [] "TASK · omega parcel"
        far = stored 43 "moka" [] [] "TASK · sigma parcel"
        below = [stored 41 "mucho" ["moka"] [] "DONE · omega sigma", near, far]
        above = [stored 44 "mucho" ["moka"] [] "DONE · omega sigma", near, far]
    belowOutcome <- try (evaluate (length (readyWarnings below))) :: IO (Either SomeException Int)
    assert "a stray below its offers computes the lint without underflow" $
      case belowOutcome of
        Right 1 ->
          case readyWarnings below of
            [warning] -> "post 41" `T.isInfixOf` warning && "offer 42" `T.isInfixOf` warning
            _ -> False
        _ -> False
    aboveOutcome <- try (evaluate (length (readyWarnings above))) :: IO (Either SomeException Int)
    assert "a stray above its offers still picks the nearest id" $
      case aboveOutcome of
        Right 1 ->
          case readyWarnings above of
            [warning] -> "post 44" `T.isInfixOf` warning && "offer 43" `T.isInfixOf` warning
            _ -> False
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
    _ <- postLocal root (mkPost "human" ["echo"] (markGlyph Landed))
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
    _ <- postLocal root (mkPost "human" ["fragile"] (markGlyph Landed))
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
    _ <- postLocal root (mkPost "human" ["echo"] (markGlyph Landed))
    bus <- openBus root
    let host = mkHost "echo" $ \ws ->
          pure $
            if "task" `elem` ws
              then [markGlyph Landed <> " task done"]
              else
                if "finish" `elem` ws
                  then [markGlyph StandDown]
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
    _ <- postLocal root (mkPost "human" [card] (markGlyph Landed))
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
    _ <- postLocal root (mkPost "human" ["aud"] (markGlyph Landed))
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
  -- Explicit post acts (M1a): runAgentLoop adds no inferred cause or ancestry.
  -- A handler that deliberately sets a thread keeps it; Q7 pins that path.
  -------------------------------------------------------------------------
  putStrLn "explicit post acts (M1a)"

  do
    root <- tempRoot "free-agent-m1a-ancestry"
    let path = root </> "log.jsonl"
        plant = do
          _ <- postLocal root (mkPost "human" ["m1a"] "ask")
          postLocal root (mkPost "human" ["m1a"] (markGlyph Landed))
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
          ( \storedBatch ->
              ioTurn (pure [mkPost "m1a" [from (stamped stored)] ("ack: " <> body (stamped stored)) | stored <- storedBatch])
          )
          >> putMVar done ()
    _ <- plant
    takeMVar done
    content <- TIO.readFile path
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "m1a"]
    assert "M1a runAgentLoop does not infer ancestry for an explicit post act" $
      case replies of
        [r] -> body r == "ack: ask" && thread r == []
        _ -> False
    removePathForcibly root

  -------------------------------------------------------------------------
  -- Whole-batch delivery and transaction recovery over real bus files.
  -------------------------------------------------------------------------
  putStrLn "batch delivery and transaction recovery"

  -- Two backlog posts cause one quiet turn. After its cursor commits, one live
  -- post causes a second turn whose batch is a singleton. Neither turn posts.
  do
    root <- tempRoot "free-agent-batch-quiet"
    writeCursor root "batch" 0
    _ <- postLocal root (mkPost "human" ["batch"] "one")
    _ <- postLocal root (mkPost "human" ["batch"] "two")
    seen <- newIORef ([] :: [[PostId]])
    done <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent
          "batch"
          ["batch"]
          root
          Nothing
          ( \storedBatch -> do
              modifyIORef' seen (map (snd . stamp) storedBatch :)
              ioTurn (pure [])
          )
          >> putMVar done ()
    let awaitCursor target remaining = do
          current <- readCursor root "batch"
          if current == target
            then pure ()
            else
              if remaining <= (0 :: Int)
                then error "timed out waiting for the quiet batch cursor"
                else threadDelay 10000 >> awaitCursor target (remaining - 1)
    awaitCursor 2 500
    _ <- postLocal root (mkPost "human" ["batch"] "three")
    _ <- postLocal root (mkPost "human" ["batch"] (markGlyph Landed))
    takeMVar done
    batches <- reverse <$> readIORef seen
    cursor <- readCursor root "batch"
    transaction <- readTransaction root "batch"
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        publications = [stamped s | s <- parsed, from (stamped s) == "batch"]
    assert "backlog drains into one turn and live singleton delivery is a batch of one" $
      batches == [[0, 1], [2]]
    assert "quiet turns write the transaction before advancing past the batch" $
      transaction == Just 3 && cursor == 4 && null publications
    removePathForcibly root

  -- A failed delivery leaves no commit evidence and the retry is handed the
  -- same whole batch. A batch that fails twice is committed as a skip instead
  -- (Axioma.Interrupt Z2 owns that witness), so what is measured here is the
  -- atomicity of the attempts made before that decision.
  do
    root <- tempRoot "free-agent-batch-retry"
    writeCursor root "recover" 0
    _ <- postLocal root (mkPost "human" ["recover"] "one")
    _ <- postLocal root (mkPost "human" ["recover"] "two")
    _ <- postLocal root (mkPost "human" ["recover"] (markGlyph Landed))
    seen <- newIORef ([] :: [[PostId]])
    attempts <- newIORef (0 :: Int)
    afterFailure <- newIORef (Nothing :: Maybe (PostId, Maybe PostId))
    done <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent
          "recover"
          ["recover"]
          root
          Nothing
          ( \storedBatch -> do
              modifyIORef' seen (map (snd . stamp) storedBatch :)
              n <- readIORef attempts
              writeIORef attempts (n + 1)
              if n == 0
                then ioTurn (error "handler exploded")
                else do
                  -- What the failed attempt left behind, read before the retry
                  -- touches anything.
                  c <- readCursor root "recover"
                  t <- readTransaction root "recover"
                  writeIORef afterFailure (Just (c, t))
                  ioTurn (pure [])
          )
          >> putMVar done ()
    takeMVar done
    batches <- reverse <$> readIORef seen
    tries <- readIORef attempts
    left <- readIORef afterFailure
    cursor <- readCursor root "recover"
    transaction <- readTransaction root "recover"
    assert "a failed delivery leaves the batch and no commit evidence behind" $
      left == Just (0, Nothing) && tries == 2
    assert "the retry is presented the same whole batch" $
      batches == [[0, 1], [0, 1]]
    assert "the retried batch then commits once, transaction before cursor" $
      cursor == 3 && transaction == Just 2
    -- A completed batch is not presented again: the only new post is the
    -- control mark that stops the loop.
    _ <- postLocal root (mkPost "human" ["recover"] (markGlyph Landed))
    restarted <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent
          "recover"
          ["recover"]
          root
          Nothing
          ( \storedBatch -> do
              modifyIORef' seen (map (snd . stamp) storedBatch :)
              ioTurn (pure [])
          )
          >> putMVar restarted ()
    takeMVar restarted
    batchesAfter <- reverse <$> readIORef seen
    assert "a completed batch is not presented again on restart" $
      batchesAfter == [[0, 1], [0, 1]]
    removePathForcibly root

  -- This is the exact durable state left by a crash after .transaction and
  -- before the cursor write: recovery finishes the cursor without a new turn.
  do
    root <- tempRoot "free-agent-transaction-recovery"
    writeCursor root "marked" 0
    _ <- postLocal root (mkPost "human" ["marked"] "one")
    _ <- postLocal root (mkPost "human" ["marked"] "two")
    _ <- postLocal root (mkPost "human" ["marked"] (markGlyph Landed))
    writeTransaction root "marked" 2
    invocations <- newIORef (0 :: Int)
    done <- newEmptyMVar
    _ <-
      forkIO $
        runTestAgent
          "marked"
          ["marked"]
          root
          Nothing
          (\_ -> ioTurn (modifyIORef' invocations (+ 1) >> pure []))
          >> putMVar done ()
    takeMVar done
    invocationCount <- readIORef invocations
    cursor <- readCursor root "marked"
    assert "a transaction ahead of its cursor recovers without repeating the agent turn" $
      invocationCount == 0 && cursor == 3
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
    _ <- postLocal root (mkPost "human" ["m1r"] (markGlyph Escalate))
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
  -- (free-agent). Both handlers deliberately cite the parent's id; the
  -- operational runner preserves that explicit ancestry without inferring
  -- another cause. The final logs must be the SAME [Post Text] once stamps
  -- are stripped — no modulo left in the comparison.
  --
  -- Counterexamples this pins: a runner that drops or rewrites deliberate
  -- ancestry, a cold-boot cursor that skips the script (the seat answers
  -- nothing), a seat that answers the halt mark, and a silent pair of empty
  -- logs (the non-triviality clause).
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
           in case controlMark p of
                Just m
                  | isHalt m -> []
                _ -> [(mkPost seatName [from p] ("ack: " <> body p)) {thread = [parentId]}]
        agent = cellToAgent (tape q7Seat) :: Agent (->) [Post Text] (Post Text) [Post Text]
        a = mkPost "human" [seatName] "one"
        b = mkPost "human" [seatName] "two"
        c = mkPost "human" [seatName] "three"
        stopper = mkPost "human" [seatName] (markGlyph Landed)
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
          ( \storedBatch ->
              ioTurn
                ( pure
                    [ (mkPost seatName [from (stamped stored)] ("ack: " <> body (stamped stored)))
                        { thread = [snd (stamp stored)]
                        }
                    | stored <- storedBatch
                    ]
                )
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
          (\_ -> ioTurn (pure (replyAct "quiet" "LOCAL\nplain local response")))
          >> putMVar done ()
    _ <- postLocal root (mkPost "human" ["quiet"] "*(no post)*")
    _ <- postLocal root (mkPost "human" ["quiet"] (markGlyph Landed))
    takeMVar done
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "quiet"]
    assert "LOCAL response text publishes no reply act" $ replies == []
    let acts = replyAct "quiet" "@human: acknowledged"
    assert "an addressed response publishes without inferred ancestry or channel" $
      case acts of
        [reply] ->
          to reply == ["human"]
            && body reply == "acknowledged"
            && thread reply == []
            && channel reply == Nothing
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

  putStrLn "bus post flags (--thread, --channel)"
  do
    root <- tempRoot "free-agent-post-flags"
    _ <- postLocal root (mkPost "human" ["seat"] "parent")
    -- The flag interface: both flags reach the command record.
    let parseFlags = execParserPure defaultPrefs (info busParser mempty)
    assert "post --thread and --channel reach the command record" $
      case parseFlags ["post", "--root", root, "--from", "seat", "--body", "reply", "--thread", "0", "--channel", "ops"] of
        Success (PostCommand _ (Just "seat") _ (Just "reply") (Just 0) (Just "ops") False) -> True
        _ -> False
    assert "post without the flags stays a root with no channel" $
      case parseFlags ["post", "--root", root, "--from", "seat", "--body", "reply"] of
        Success (PostCommand _ _ _ _ Nothing Nothing False) -> True
        _ -> False
    -- `bus say` was removed: post is the surviving command, and it takes the
    -- piped body say used to take (the parser allows --from with no --body).
    assert "bus say is gone from the interface" $
      case parseFlags ["say", "--root", root, "--from", "seat", "--to", "human", "hello"] of
        Failure _ -> True
        _ -> False
    assert "post accepts --from with no --body, which is the piped-body form" $
      case parseFlags ["post", "--root", root, "--from", "seat", "--to", "human"] of
        Success (PostCommand _ (Just "seat") ["human"] Nothing Nothing Nothing False) -> True
        _ -> False
    -- help names both flags, so the interface is discoverable, not just parsed.
    assert "post --help names --thread and --channel" $
      case parseFlags ["post", "--help"] of
        Failure f ->
          let (msg, _) = renderFailure f "free-agent"
           in "--thread" `T.isInfixOf` T.pack msg && "--channel" `T.isInfixOf` T.pack msg
        _ -> False
    -- The effect: a flag post lands ancestry and a channel in the log, and the
    -- channel addresses it (an empty --to still delivers to a subscriber).
    runBusCommand (PostCommand root (Just "seat") [] (Just "reply") (Just 0) (Just "ops") False)
    content <- TIO.readFile (root </> "log.jsonl")
    let landed = map stamped (mapMaybe (unframeStored @Text) (T.lines content))
        replyPost = last landed
    assert "post --thread stores one ancestry edge to the named post" $
      thread replyPost == [0]
    assert "post --channel stores the channel" $ channel replyPost == Just "ops"
    assert "a channel post addresses by channel: it delivers to a subscriber and not to its --to list" $
      deliversTo replyPost ["ops"] && not (deliversTo replyPost ["seat"])
    -- One rule for every post: the parent must exist.
    present <- checkThreadId root 0
    missing <- checkThreadId root 99
    assert "the shared --thread rule accepts an existing id and refuses a typo" $
      present == Right () && case missing of Left _ -> True; Right _ -> False
    -- Compatibility: no flags means the JSON post is untouched; a flag wins.
    let jsonPost = mkChannelPost "a" "old" ["b"] "body"
    assert "no flags leave a JSON post's ancestry and channel untouched" $
      applyPostFlags Nothing Nothing jsonPost == jsonPost
    assert "a channel flag overrides the JSON channel and a thread flag sets one edge" $
      channel (applyPostFlags Nothing (Just "new") jsonPost) == Just "new"
        && thread (applyPostFlags (Just 4) Nothing jsonPost) == [4]
        && to (applyPostFlags (Just 4) (Just "new") jsonPost) == ["b"]
    removePathForcibly root
