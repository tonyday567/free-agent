{-# LANGUAGE OverloadedStrings #-}

-- | Pure log analysis for the free-agent bus.
--
-- Computes flow metrics from a stamped JSONL log: Re_bus, SNR,
-- posts-per-deliverable, and time-to-quiescence.  Classifications are
-- regex-driven and overridable so old transcripts can be re-analysed.
module Free.Agent.BusStats
  ( -- * Rules
    Rules (..),
    defaultRules,
    Classification (..),
    classify,
    isDeliverable,
    isDoneClaim,

    -- * Slicing
    SliceMode (..),
    slicePosts,

    -- * Statistics
    Stats (..),
    computeStats,

    -- * Rendering
    renderStats,
    renderStatsJson,
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (Stamped (..))
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (decodeUtf8)
import Data.List (minimumBy, sort, sortOn)
import Data.Ord (comparing)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Numeric.Natural (Natural)
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))

-- ---------------------------------------------------------------------------
-- Classification rules
-- ---------------------------------------------------------------------------

-- | Regex-driven classification rules.  Each field is a single regex; the
-- default 'signalRE' combines path, mark, and decision-word alternatives.
data Rules = Rules
  { -- | Regex matching noise (status pings, idle chatter).
    noiseRE :: Text,
    -- | Regex matching signal (paths, marks, decisions).
    signalRE :: Text,
    -- | Regex matching a conductor deliverable mark.
    deliverableRE :: Text
  }
  deriving (Eq, Show)

-- | Sensible defaults decided with the card.
defaultRules :: Rules
defaultRules =
  Rules
    { noiseRE = "standing by|session complete|^ack$|ping|still here|waiting on",
      signalRE = "[^[:space:]]+\\.(md|hs|cabal|jsonl)|🟢|⟝|🚩|decided|fixed|bug|found",
      deliverableRE = "🟢"
    }

-- | Path regex shared between signal detection and DONE-claim detection.
pathRE :: Text
pathRE = "[^[:space:]]+\\.(md|hs|cabal|jsonl)"

-- | A post is signal, noise, or neutral.
data Classification = Signal | Noise | Neutral
  deriving (Eq, Show)

-- | Classify a post.  Signal takes precedence over noise: a post matching the
-- noise regex but carrying a path or mark is counted as signal.
classify :: Rules -> Post Text -> Classification
classify rules p
  | matches (signalRE rules) (body p) = Signal
  | matches (noiseRE rules) (body p) = Noise
  | otherwise = Neutral

-- | Whether a post is a conductor deliverable.
isDeliverable :: Rules -> Post Text -> Bool
isDeliverable rules p = matches (deliverableRE rules) (body p)

-- | Whether a post claims completion and cites a path.
isDoneClaim :: Post Text -> Bool
isDoneClaim p =
  T.isInfixOf "DONE" (T.toUpper (body p)) && matches pathRE (body p)

-- | Case-insensitive regex match against a body.
matches :: Text -> Text -> Bool
matches pat txt = T.unpack txt =~ T.unpack pat

-- ---------------------------------------------------------------------------
-- Time handling
-- ---------------------------------------------------------------------------

-- | Parse the timestamp format written by the bus scribe.
-- | Format a timestamp for display.
formatTs :: UTCTime -> Text
formatTs = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

-- | Minutes between two timestamps.
minutes :: UTCTime -> UTCTime -> Double
minutes a b = realToFrac (diffUTCTime b a) / 60.0

-- ---------------------------------------------------------------------------
-- Slicing
-- ---------------------------------------------------------------------------

-- | How to partition the log for metric computation.
data SliceMode
  = -- | One slice over the entire log.
    WholeLog
  | -- | Fixed-width time buckets in minutes.
    WindowMinutes Int
  | -- | One slice per thread tree (grouped by root post id).
    ByThread
  | -- | One slice per authoring agent.
    ByAgent
  deriving (Eq, Show)

-- | Partition stamped posts into slices.  Posts without a parseable timestamp
-- are dropped from time-based slicing, but kept for thread slicing (where the
-- id is enough).
slicePosts :: SliceMode -> [Stamped Text] -> [(Text, [Stamped Text])]
slicePosts WholeLog posts = [("whole", posts)]
slicePosts (WindowMinutes m) posts =
  case timed of
    [] -> []
    _ -> map (\(k, vs) -> (bucketLabel k, vs)) (Map.toList buckets)
  where
    timed = [(timeStamp p, p) | p <- posts]
    base = minimum (map fst timed)
    bucket t = floor (minutes base t) `div` m
    buckets = Map.fromListWith (flip (++)) (map (\(t, p) -> (bucket t, [p])) timed)
    bucketLabel k =
      let start = addMinutes base (k * m)
          end = addMinutes start m
       in T.concat [formatTs start, "..", formatTs end]
slicePosts ByThread posts =
  sortOn fst [(showt k, v) | (k, v) <- Map.toList roots]
  where
    idx = Map.fromList [(stamp p, p) | p <- posts]
    rootOf p = case thread (stamped p) of
      [] -> stamp p
      ts -> case mapMaybe (`Map.lookup` idx) ts of
        [] -> minimum ts
        ps -> rootOf (minimumBy (comparing stamp) ps)
    roots = Map.fromListWith (++) [(rootOf p, [p]) | p <- posts]
slicePosts ByAgent posts =
  sortOn fst [(k, v) | (k, v) <- Map.toList byAuthor]
  where
    byAuthor = Map.fromListWith (++) [(from (stamped p), [p]) | p <- posts]

addMinutes :: UTCTime -> Int -> UTCTime
addMinutes t n = addUTCTime (fromIntegral (n * 60) :: NominalDiffTime) t

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Computed metrics for one slice.
data Stats = Stats
  { statSlice :: Text,
    statPosts :: Int,
    statAgents :: Int,
    statDurationMinutes :: Double,
    statPostsPerMinute :: Double,
    statDamping :: Int,
    statReBus :: Double,
    statSignal :: Int,
    statNoise :: Int,
    statUnclassified :: Int,
    statSNR :: Maybe Double,
    statSNRPrime :: Maybe Double,
    statDeliverables :: Int,
    statDoneClaims :: Int,
    statPostsPerDeliverable :: Maybe Double,
    statTimeToQuiescenceMinutes :: Double
  }
  deriving (Eq, Show)

-- | Compute stats for one slice given classification rules and a damping count.
computeStats :: Rules -> Int -> Text -> [Stamped Text] -> Stats
computeStats rules damping label posts =
  Stats
    { statSlice = label,
      statPosts = n,
      statAgents = length agents,
      statDurationMinutes = dur,
      statPostsPerMinute = ppm,
      statDamping = max 1 damping,
      statReBus = reBus,
      statSignal = signalCount,
      statNoise = noiseCount,
      statUnclassified = unclassifiedCount,
      statSNR = snr,
      statSNRPrime = snrPrime,
      statDeliverables = deliverables,
      statDoneClaims = doneClaims,
      statPostsPerDeliverable = ppd,
      statTimeToQuiescenceMinutes = ttq
    }
  where
    n = length posts
    agents = sort $ Map.keys $ Map.fromList [(from (stamped p), ()) | p <- posts]
    timestamps = map timeStamp posts
    (startTime, endTime) = case timestamps of
      [] -> (Nothing, Nothing)
      ts -> (Just (minimum ts), Just (maximum ts))
    dur = maybe 0 (\s -> maybe 0 (minutes s) endTime) startTime
    ppm = if dur > 0 then fromIntegral n / dur else 0
    reBus =
      if dur > 0 && damping > 0
        then fromIntegral (length agents) * ppm / fromIntegral damping
        else 0
    classified = map (classify rules . stamped) posts
    signalCount = length (filter (== Signal) classified)
    noiseCount = length (filter (== Noise) classified)
    unclassifiedCount = n - signalCount - noiseCount
    snr
      | noiseCount > 0 = Just (fromIntegral signalCount / fromIntegral noiseCount)
      | signalCount > 0 = Nothing -- conceptually infinite
      | otherwise = Just 0
    snrPrime
      | unclassifiedCount + noiseCount > 0 =
          Just (fromIntegral signalCount / fromIntegral (unclassifiedCount + noiseCount))
      | signalCount > 0 = Nothing -- conceptually infinite
      | otherwise = Just 0
    deliverables = length (filter (isDeliverable rules . stamped) posts)
    doneClaims = length (filter (isDoneClaim . stamped) posts)
    ppd = if deliverables > 0 then Just (fromIntegral n / fromIntegral deliverables) else Nothing
    ttq = case startTime of
      Nothing -> 0
      Just start ->
        let signalPosts = [p | p <- posts, classify rules (stamped p) == Signal]
            signalTimes = map timeStamp signalPosts
         in case signalTimes of
              [] -> 0
              ts -> minutes start (maximum ts)

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | Human-readable plain-text report.
renderStats :: Rules -> [Stats] -> Text
renderStats rules stats =
  T.unlines $
    [ "bus-stats",
      "  noise regex:    " <> noiseRE rules,
      "  signal regex:   " <> signalRE rules,
      "  deliverable:    " <> deliverableRE rules,
      ""
    ]
      ++ concatMap renderSlice stats
  where
    renderSlice s =
      [ "slice: " <> statSlice s,
        "  posts:               " <> showt (statPosts s),
        "  agents:              " <> showt (statAgents s),
        "  duration (min):      " <> showd (statDurationMinutes s),
        "  posts/min:           " <> showd (statPostsPerMinute s),
        "  damping rules:       " <> showt (statDamping s),
        "  Re_bus:              " <> showd (statReBus s),
        "  signal:              " <> showt (statSignal s),
        "  noise:               " <> showt (statNoise s),
        "  unclassified:        " <> showt (statUnclassified s),
        "  SNR:                 " <> maybe "-" showd (statSNR s),
        "  SNR':                " <> maybe "-" showd (statSNRPrime s),
        "  deliverables:        " <> showt (statDeliverables s),
        "  done claims:         " <> showt (statDoneClaims s),
        "  posts/deliverable:   " <> maybe "n/a" showd (statPostsPerDeliverable s),
        "  time to quiescence (min): " <> showd (statTimeToQuiescenceMinutes s),
        ""
      ]

showt :: (Show a) => a -> Text
showt = T.pack . show

showd :: Double -> Text
showd = T.pack . printf "%0.3f"

-- | JSON report.
renderStatsJson :: Rules -> [Stats] -> Text
renderStatsJson rules stats =
  decodeUtf8 (BL.toStrict (encode (object ["rules" .= rulesObj, "slices" .= map statsObject stats])))
  where
    rulesObj =
      object
        [ "noise" .= noiseRE rules,
          "signal" .= signalRE rules,
          "deliverable" .= deliverableRE rules
        ]
    statsObject s =
      object
        [ "slice" .= statSlice s,
          "posts" .= statPosts s,
          "agents" .= statAgents s,
          "duration_minutes" .= statDurationMinutes s,
          "posts_per_minute" .= statPostsPerMinute s,
          "damping_rules" .= statDamping s,
          "re_bus" .= statReBus s,
          "signal" .= statSignal s,
          "noise" .= statNoise s,
          "unclassified" .= statUnclassified s,
          "snr" .= statSNR s,
          "snr_prime" .= statSNRPrime s,
          "deliverables" .= statDeliverables s,
          "done_claims" .= statDoneClaims s,
          "posts_per_deliverable" .= statPostsPerDeliverable s,
          "time_to_quiescence_minutes" .= statTimeToQuiescenceMinutes s
        ]
