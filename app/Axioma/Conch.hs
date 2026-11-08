{-# LANGUAGE OverloadedStrings #-}

-- | Conch integration witnesses over real scratch-bus JSONL traffic.
module Axioma.Conch
  ( conchTopic,
  )
where

import Axioma.Common (assert, tempRoot)
import Circuit.Agent (Name, Post (..), PostId)
import Circuit.Agent.Conch
  ( Conch (..),
    ConchConfig (..),
    ConchPayload,
    ConchPost,
    ConchState (..),
    Custody (..),
    emptyConchState,
    reservedFiles,
    stepConch,
  )
import Circuit.Agent.Framing (Stamped, stamp, stamped, unframeStored)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, bracket, throwIO, try)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Bus (readFileStrict)
import Free.Agent.Bus.Daemon (postViaDaemon, runDaemon)
import Free.Agent.Conch (ConchReceipt (..), processConchPost, recoverConch)
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import Prelude

withRoot :: String -> (FilePath -> IO a) -> IO a
withRoot name = bracket (tempRoot name) removePathForcibly

config :: [Name] -> ConchConfig
config strats =
  ConchConfig
    { systemSeat = "conch-system",
      stratSeats = Set.fromList strats,
      recipientSeats = Set.fromList ["alice", "bob", "carol", "dave", "eve", "orange", "violet", "u1", "u2"]
    }

ordinary :: Name -> Name -> Text -> ConchPost
ordinary f c text = Post f (Just c) [] [] (Nothing, text)

claim :: Name -> Name -> [FilePath] -> Text -> ConchPost
claim f c paths text = Post f (Just c) [] [] (Just (Claim paths), text)

handover :: Name -> Name -> [Name] -> Maybe PostId -> Text -> ConchPost
handover f c recipients parent text =
  Post f (Just c) recipients (maybe [] pure parent) (Just Handover, text)

postId :: Stamped ConchPayload -> PostId
postId = snd . stamp

postProcess :: ConchConfig -> FilePath -> ConchPost -> IO (Stamped ConchPayload)
postProcess cfg root p = receiptPost <$> processConchPost cfg root p

readConchLog :: FilePath -> IO [Stamped ConchPayload]
readConchLog root =
  mapMaybe (unframeStored @ConchPayload) . T.lines
    <$> readFileStrict (root </> "log.jsonl")

channelHandovers :: Name -> [Stamped ConchPayload] -> [Stamped ConchPayload]
channelHandovers c =
  filter
    ( \s ->
        channel (stamped s) == Just c
          && fst (body (stamped s)) == Just Handover
    )

holderAt :: Name -> ConchState -> Maybe Name
holderAt c = fmap holder . Map.lookup c . custody

filesAt :: Name -> ConchState -> [FilePath]
filesAt c = maybe [] (sort . Set.toList . reservations) . Map.lookup c . custody

allFiles :: ConchState -> [FilePath]
allFiles = sort . Set.toList . reservedFiles

readOptional :: FilePath -> IO (Maybe Text)
readOptional path = do
  exists <- doesFileExist path
  if exists then Just <$> readFileStrict path else pure Nothing

makeResources :: FilePath -> [FilePath] -> IO ()
makeResources root paths =
  mapM_
    ( \path -> do
        createDirectoryIfMissing True (takeDirectory (root </> path))
        writeFile (root </> path) path
    )
    paths

conchTopic :: IO ()
conchTopic = do
  putStrLn "Conch ordinary posts and framing"
  withRoot "free-agent-conch-ordinary" $ \root -> do
    let cfg = config []
        posts =
          [ ordinary "alice" "duty" "not yet",
            ordinary "bob" "duty" "[complete] the prose is not custody",
            ordinary "carol" "duty" "TASK READY COMPLETE DISCOVER ERROR [discover] [error]",
            ordinary "dave" "duty" "DONE CLAIM [done]"
          ]
    mapM_ (postProcess cfg root) posts
    st <- recoverConch cfg root
    parsed <- readConchLog root
    raw <- readFileStrict (root </> "log.jsonl")
    assert "ordinary and schedule posts mint no custody (catches text interpreted as a Claim or Handover)" $
      custody st == Map.empty
    assert "ordinary and schedule posts emit no system Handover (catches privileged task vocabulary)" $
      null (channelHandovers "duty" parsed)
    assert "exact pair/Maybe payload framing preserves every real bus line (catches dropped channel or body decoding)" $
      length parsed == length (T.lines raw)
        && map (body . stamped) parsed == map body posts
        && map (channel . stamped) parsed == map channel posts

  putStrLn "Conch bare payload round-trips through the real FIFO daemon"
  withRoot "free-agent-conch-daemon" $ \root ->
    bracket (forkIO (runDaemon @ConchPayload root)) killThread $ \_ -> do
      threadDelay 100000
      let bare = handover "alice" "daemon-channel" ["bob"] (Just 41) "exact body: λ\nsecond line"
      stored <- postViaDaemon root bare
      parsed <- readConchLog root
      assert "bare daemon round-trip preserves Just channel (catches framePost dropping channel)" $
        channel (stamped stored) == Just "daemon-channel"
      assert "bare daemon round-trip preserves exact body and Handover parent (catches payload or thread loss)" $
        body (stamped stored) == body bare && thread (stamped stored) == [41] && to (stamped stored) == ["bob"]
      assert "the real FIFO daemon durably stores the exact stamped ConchPayload (catches receipt/log divergence)" $
        parsed == [stored]

  putStrLn "Conch dual handshake and named transfer"
  withRoot "free-agent-conch-handshake" $ \root -> do
    let cfg = config []
        resource = "src/alpha.hs"
    makeResources root [resource]
    _ <- postProcess cfg root (claim "alice" "parcel" [resource] "initial request")
    st0 <- recoverConch cfg root
    assert "an available unowned Claim earns exactly one holder (catches Claim ignored or duplicated system Handovers)" $
      holderAt "parcel" st0 == Just "alice"
    assert "the initial system Handover reserves the requested whole file (catches custody without resources)" $
      filesAt "parcel" st0 == [root </> resource]
    _ <- postProcess cfg root (claim "bob" "parcel" [] "may I hold it?")
    st1 <- recoverConch cfg root
    log1 <- readConchLog root
    assert "a Claim alone cannot take an owned conch (catches binding Claim semantics)" $
      holderAt "parcel" st1 == Just "alice"
    assert "a tact Claim on an owned conch earns no system Handover (catches automatic revocation)" $
      length (channelHandovers "parcel" log1) == 1
    _ <- postProcess cfg root (handover "alice" "parcel" ["bob"] Nothing "ok")
    st2 <- recoverConch cfg root
    assert "the authorised named Handover installs its recipient (catches Claims acquiring without Handover)" $
      holderAt "parcel" st2 == Just "bob"
    assert "an unlinked named Handover transfers existing reservations with the conch (catches stranded file ownership)" $
      filesAt "parcel" st2 == [root </> resource]
    bobClaim <- postProcess cfg root (claim "carol" "parcel" ["src/beta.hs"] "new resource request")
    _ <- postProcess cfg root (handover "bob" "parcel" ["carol"] (Just (postId bobClaim)) "accepted")
    st3 <- recoverConch cfg root
    assert "a linked accepted Claim supplies the recipient's requested resources (catches stale reservations carried across acceptance)" $
      holderAt "parcel" st3 == Just "carol" && filesAt "parcel" st3 == [root </> "src/beta.hs"]
    assert "resource handover leaves no reservation with the previous holder (catches duplicate global file custody)" $
      allFiles st3 == [root </> "src/beta.hs"]

  putStrLn "Conch refused and losing Claims stay dead"
  withRoot "free-agent-conch-refused" $ \root -> do
    let cfg = config []
    _ <- postProcess cfg root (claim "alice" "refusal" ["a.txt"] "mine")
    _ <- postProcess cfg root (claim "bob" "refusal" [] "losing request")
    _ <- postProcess cfg root (ordinary "alice" "refusal" "not yet")
    abandonment <- processConchPost cfg root (handover "alice" "refusal" [] Nothing "abandon")
    _ <- postProcess cfg root (ordinary "carol" "refusal" "later unrelated traffic")
    st <- recoverConch cfg root
    parsed <- readConchLog root
    assert "unnominated abandonment emits no fallback (catches fallback generated for an empty nomination)" $
      null (receiptHandovers abandonment)
    assert "a refused Claim is not queued for later acquisition (catches automatic acquisition after abandonment)" $
      holderAt "refusal" st == Nothing
    assert "a refused Claim receives no delayed system Handover (catches automatic reposting)" $
      length (channelHandovers "refusal" parsed) == 2
    assert "abandonment releases the old file reservation (catches a resource leak into the pool)" $
      allFiles st == []

  putStrLn "Conch invalid nomination and unnominated abandonment"
  withRoot "free-agent-conch-abandon" $ \root -> do
    let cfg = config []
    _ <- postProcess cfg root (claim "alice" "invalid" ["a.txt", "b.txt"] "mine")
    _ <- postProcess cfg root (handover "alice" "invalid" ["ghost"] Nothing "bad recipient")
    st0 <- recoverConch cfg root
    assert "an invalid nomination returns the conch to the pool (catches recipient validation failure)" $
      holderAt "invalid" st0 == Nothing
    assert "an invalid nomination releases the previous holder's resources (catches failed nomination trapping custody)" $
      allFiles st0 == []
    _ <- postProcess cfg root (claim "bob" "invalid" [] "fresh request after invalid nomination")
    st1 <- recoverConch cfg root
    assert "a fresh Claim after invalid nomination can acquire (catches the old holder remaining installed)" $
      holderAt "invalid" st1 == Just "bob"
    _ <- postProcess cfg root (handover "bob" "invalid" [] Nothing "walking away without an outcome tag")
    st2 <- recoverConch cfg root
    assert "an unnominated Handover abandons without DONE or other schedule vocabulary (catches text-gated release)" $
      holderAt "invalid" st2 == Nothing && allFiles st2 == []

  putStrLn "Conch invalid nominations emit one releasing fallback"
  let fallbackCase tag sender nominees =
        withRoot ("free-agent-conch-fallback-" <> tag) $ \root -> do
          let cfg = config []
          _ <- postProcess cfg root (claim "alice" "fallback" ["a.txt", "b.txt"] "mine")
          causing <- processConchPost cfg root (handover sender "fallback" nominees Nothing "invalid nomination")
          rawBefore <- readFileStrict (root </> "log.jsonl")
          st <- recoverConch cfg root
          rawAfter <- readFileStrict (root </> "log.jsonl")
          parsed <- readConchLog root
          assert "an authorised invalid nomination emits exactly one fallback (catches missing or repeated fallback Handovers)" $
            length (receiptHandovers causing) == 1
          case receiptHandovers causing of
            [fallback] ->
              assert "the fallback is system-authored, unnominated, and threaded to its causing Handover (catches wrong provenance or recipient)" $
                let p = stamped fallback
                 in from p == systemSeat cfg
                      && to p == []
                      && channel p == Just "fallback"
                      && thread p == [postId (receiptPost causing)]
            _ -> pure ()
          assert "invalid nomination releases custody and all reservations (catches fallback that strands resources)" $
            holderAt "fallback" st == Nothing && filesAt "fallback" st == [] && allFiles st == []
          assert "durable history contains the causing and fallback Handovers exactly once (catches receipt-only or duplicated fallback)" $
            length (channelHandovers "fallback" parsed) == 3
          assert "replaying invalid nomination appends no second fallback (catches recovery re-emission)" $
            rawAfter == rawBefore
  fallbackCase "singleton" "alice" ["ghost"]
  fallbackCase "multiple" "alice" ["bob", "carol"]

  putStrLn "Conch third-party Handovers cannot steal or release"
  withRoot "free-agent-conch-theft" $ \root -> do
    let cfg = config []
    _ <- postProcess cfg root (claim "alice" "owned" ["a.txt"] "mine")
    theft <- processConchPost cfg root (handover "eve" "owned" ["eve"] Nothing "steal")
    st0 <- recoverConch cfg root
    assert "a third-party named Handover cannot steal custody (catches missing holder authorization)" $
      holderAt "owned" st0 == Just "alice" && filesAt "owned" st0 == [root </> "a.txt"]
    unauthorisedRelease <- processConchPost cfg root (handover "eve" "owned" [] Nothing "forced abandonment")
    st1 <- recoverConch cfg root
    parsed <- readConchLog root
    assert "unauthorised Handovers emit no fallback (catches fallback authority confused with nomination validity)" $
      null (receiptHandovers theft) && null (receiptHandovers unauthorisedRelease)
    assert "a third-party unnominated Handover cannot release custody (catches abandonment authorization bypass)" $
      holderAt "owned" st1 == Just "alice" && allFiles st1 == [root </> "a.txt"]
    assert "unauthorised Handovers earn no responsive system Handover (catches runtime amplifying theft attempts)" $
      length (channelHandovers "owned" parsed) == 3
        && holderAt "owned" st1 == Just "alice"

  putStrLn "Conch resource acquisition is all-or-nothing"
  withRoot "free-agent-conch-resources" $ \root -> do
    let cfg = config []
    makeResources root ["a.txt", "b.txt"]
    _ <- postProcess cfg root (claim "alice" "left" ["a.txt"] "left resource")
    _ <- postProcess cfg root (claim "bob" "right" ["b.txt"] "right resource")
    _ <- postProcess cfg root (claim "carol" "join" ["a.txt", "b.txt"] "atomic request")
    st0 <- recoverConch cfg root
    parsed0 <- readConchLog root
    assert "a conflicting multi-file Claim acquires no conch (catches partial multi-resource acquisition)" $
      holderAt "join" st0 == Nothing
    assert "a conflicting multi-file Claim emits no system Handover (catches partial or delayed resource grants)" $
      null (channelHandovers "join" parsed0)
    assert "a failed multi-file Claim cannot steal either existing reservation (catches cross-channel resource revocation)" $
      filesAt "left" st0 == [root </> "a.txt"] && filesAt "right" st0 == [root </> "b.txt"]
    _ <- postProcess cfg root (handover "alice" "left" [] Nothing "release a")
    _ <- postProcess cfg root (handover "bob" "right" [] Nothing "release b")
    _ <- postProcess cfg root (ordinary "dave" "join" "resources are now free")
    st1 <- recoverConch cfg root
    assert "resource release does not wake the failed multi-file Claim (catches queued resource waiters)" $
      holderAt "join" st1 == Nothing && allFiles st1 == []
    _ <- postProcess cfg root (claim "carol" "join" ["a.txt", "b.txt"] "new atomic request")
    st2 <- recoverConch cfg root
    assert "a new Claim can atomically acquire the released path set (catches stale conflict state)" $
      holderAt "join" st2 == Just "carol" && filesAt "join" st2 == sort [root </> "a.txt", root </> "b.txt"]

  putStrLn "Conch configured strat force, tact refusal, and seat rename"
  let forceCase rootName owner tact strat = withRoot rootName $ \root -> do
        let cfg = config [strat]
        _ <- postProcess cfg root (claim owner "force" ["a.txt"] "tact owner")
        _ <- postProcess cfg root (claim tact "force" [] "ordinary tact request")
        st0 <- recoverConch cfg root
        assert "an ordinary tact cannot force revocation (catches force capability granted to every seat)" $
          holderAt "force" st0 == Just owner
        _ <- postProcess cfg root (claim strat "force" [] "configured force request")
        st1 <- recoverConch cfg root
        parsed <- readConchLog root
        assert "a configured strat force-claim revokes through a system Handover (catches missing force capability)" $
          holderAt "force" st1 == Just strat
        assert "force uses the linked Claim's path set (catches stale reservations surviving revocation)" $
          allFiles st1 == []
        assert "tact refusal plus strat force yields exactly two binding Handovers (catches a hidden tact transfer)" $
          length (channelHandovers "force" parsed) == 2
  forceCase "free-agent-conch-force" "alice" "bob" "orange"
  forceCase "free-agent-conch-renamed" "u1" "u2" "violet"

  putStrLn "Conch binding Handover order beats Claim arrival order"
  do
    let cfg = config []
        first = claim "alice" "race-order" [] "Claim arrived first"
        second = claim "bob" "race-order" [] "Claim arrived second"
        lookupPost pid = lookup pid [(0, first), (1, second)]
        (st0, responses) = stepConch cfg lookupPost emptyConchState 1 second
    case responses of
      [winner] -> do
        let (st1, winnerEffects) = stepConch cfg lookupPost st0 2 winner
            (st2, loserEffects) = stepConch cfg lookupPost st1 0 first
        assert "processing the second Claim first makes it the holder (catches Claim-arrival order deciding the race)" $
          holderAt "race-order" st2 == Just "bob"
        assert "the sole system Handover cites the processed winning Claim (catches misidentified Claim parentage)" $
          to winner == ["bob"] && thread winner == [1]
        assert "the losing earlier Claim earns no later system Handover (catches automatic loser reposting)" $
          null winnerEffects && null loserEffects
      _ -> assert "an available second Claim receives one system Handover (catches a missing or duplicate winning transition)" False

  putStrLn "Conch concurrent Claimants have one winner"
  withRoot "free-agent-conch-concurrent" $ \root -> do
    let cfg = config []
    gate <- newEmptyMVar
    doneA <- newEmptyMVar
    doneB <- newEmptyMVar
    let racer name done = do
          _ <- forkIO $ do
            result <- try @SomeException (readMVar gate >> postProcess cfg root (claim name "concurrent" [] (name <> " request")))
            putMVar done result
          pure ()
    racer "alice" doneA
    racer "bob" doneB
    putMVar gate ()
    resultA <- takeMVar doneA
    resultB <- takeMVar doneB
    _ <- either throwIO pure resultA
    _ <- either throwIO pure resultB
    st <- recoverConch cfg root
    parsed <- readConchLog root
    assert "concurrent Claimants produce exactly one holder (catches unsynchronised double acquisition)" $
      case holderAt "concurrent" st of
        Just h -> h `elem` ["alice", "bob"]
        Nothing -> False
    assert "concurrent Claimants produce exactly one system Handover (catches duplicate binding transitions)" $
      length (channelHandovers "concurrent" parsed) == 1
    assert "real concurrent bus appends retain a contiguous three-post log (catches id collisions or lost posts)" $
      map postId parsed == [0, 1, 2]

  putStrLn "Conch restart reconstructs without transfers or loser wakes"
  withRoot "free-agent-conch-restart" $ \root -> do
    let cfg = config []
        alicePing = root </> ".ping-alice"
        bobPing = root </> ".ping-bob"
    _ <- postProcess cfg root (claim "alice" "restart" ["a.txt"] "winner")
    _ <- postProcess cfg root (claim "bob" "restart" [] "loser")
    beforeState <- recoverConch cfg root
    beforeRaw <- readFileStrict (root </> "log.jsonl")
    beforeAlicePing <- readOptional alicePing
    beforeBobPing <- readOptional bobPing
    afterState <- recoverConch cfg root
    afterRaw <- readFileStrict (root </> "log.jsonl")
    afterAlicePing <- readOptional alicePing
    afterBobPing <- readOptional bobPing
    assert "restart preserves holder and reservations (catches replay dropping binding Handovers)" $
      beforeState == afterState
        && holderAt "restart" afterState == Just "alice"
        && filesAt "restart" afterState == [root </> "a.txt"]
    assert "restart appends no repeated transfer or loser Handover (catches Claims replayed as runtime input)" $
      beforeRaw == afterRaw
    assert "restart writes no winner or loser ping (catches repeated claimant wakes)" $
      beforeAlicePing == afterAlicePing && beforeBobPing == afterBobPing
