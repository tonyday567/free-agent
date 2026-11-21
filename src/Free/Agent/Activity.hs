{-# LANGUAGE OverloadedStrings #-}

-- | Independent live activity sampling for text-producing protocol runners.
--
-- ACP chunks do not carry provider token counts. 'observeChunk' therefore
-- commits an explicit lexical estimate, never provider usage. Sampling is a
-- separate clock-driven activity: quiet intervals contribute zero instantaneous
-- rates, and the circuits-stats averager smooths those samples without waiting
-- for prompt settlement.
module Free.Agent.Activity
  ( TokenCount (..),
    TimeStamp (..),
    ActivitySample (..),
    ActivityObserver,
    newActivityObserver,
    observeChunk,
    observeAcpEmission,
    currentTokens,
    sampleActivity,
    withActivitySampler,
    appendActivitySample,
  )
where

import Circuit.Stats (Averager (..), av)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Free.Agent.Acp (AcpEmission (..), Update (..))
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- | Cumulative estimated tokens emitted by thought and reply chunks.
newtype TokenCount = TokenCount {unTokenCount :: Int}
  deriving (Eq, Ord, Show)

-- | Monotonic nanoseconds; this clock is unrelated to wall-clock bus stamps.
newtype TimeStamp = TimeStamp {unTimeStamp :: Word64}
  deriving (Eq, Ord, Show)

-- | One independent rate observation, including the cumulative count and both
-- instantaneous and exponentially weighted rates.
data ActivitySample = ActivitySample
  { sampleTime :: TimeStamp,
    sampleTokens :: TokenCount,
    sampleInstantRate :: Double,
    sampleRate :: Double
  }
  deriving (Eq, Show)

data SampleState = SampleState
  { stateTime :: TimeStamp,
    stateTokens :: TokenCount,
    stateAverage :: Averager Double Double
  }

data ActivityObserver = ActivityObserver
  { observerTokens :: IORef TokenCount,
    observerSample :: IORef SampleState,
    observerDecay :: Double
  }

-- | Start an observer with the supplied averager decay, as in
-- @Circuit.Stats.ma decay@.
newActivityObserver :: Double -> IO ActivityObserver
newActivityObserver decay = do
  now <- TimeStamp <$> getMonotonicTimeNSec
  tokens <- newIORef (TokenCount 0)
  sample <- newIORef (SampleState now (TokenCount 0) (Averager (0, 0)))
  pure (ActivityObserver tokens sample decay)

-- | Commit one non-empty thought or reply chunk immediately. The estimate is
-- one token per whitespace-delimited word, with a one-token minimum for a
-- non-empty chunk; it is deliberately not represented as provider usage.
observeChunk :: ActivityObserver -> Text -> IO TokenCount
observeChunk observer text =
  atomicModifyIORef' (observerTokens observer) $ \(TokenCount count) ->
    let next = TokenCount (count + estimate text)
     in (next, next)
  where
    estimate chunk
      | T.null chunk = 0
      | otherwise = max 1 (length (T.words chunk))

-- | Observe only thought and reply text. Protocol maintenance, stop, usage,
-- and settlement retain the current count and never manufacture tokens.
observeAcpEmission :: ActivityObserver -> AcpEmission -> IO TokenCount
observeAcpEmission observer emission = case emission of
  AcpProtocol _ (Just (AgentThoughtChunk text)) -> observeChunk observer text
  AcpProtocol _ (Just (AgentMessageChunk text)) -> observeChunk observer text
  _ -> currentTokens observer

currentTokens :: ActivityObserver -> IO TokenCount
currentTokens observer = readIORef (observerTokens observer)

-- | Sample cumulative count against the monotonic clock. Equal counts produce
-- a genuine zero-rate input, so the average decays during quiet intervals.
sampleActivity :: ActivityObserver -> IO ActivitySample
sampleActivity observer = do
  now <- TimeStamp <$> getMonotonicTimeNSec
  tokens <- currentTokens observer
  atomicModifyIORef' (observerSample observer) $ \previous ->
    let elapsed = fromIntegral (unTimeStamp now - unTimeStamp (stateTime previous)) / 1e9
        delta = unTokenCount tokens - unTokenCount (stateTokens previous)
        instant = if elapsed > 0 then fromIntegral delta / elapsed else 0
        averaged = smooth (observerDecay observer) instant (stateAverage previous)
        sample = ActivitySample now tokens instant (av averaged)
     in (SampleState now tokens averaged, sample)
  where
    smooth decay rate (Averager (total, count)) =
      Averager (decay * total + rate, decay * count + 1)

-- | Run an independent periodic sampler for the duration of an action.
withActivitySampler :: ActivityObserver -> Int -> (ActivitySample -> IO ()) -> IO a -> IO a
withActivitySampler observer interval sink action =
  bracket (forkIO (forever (sampleActivity observer >>= sink >> threadDelay interval))) killThread (const action)

-- | Append one local JSONL activity surface. No bus log or post type is used.
appendActivitySample :: FilePath -> ActivitySample -> IO ()
appendActivitySample path sample = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.appendFile path (render sample <> "\n")
  where
    render (ActivitySample (TimeStamp time) (TokenCount tokens) instant averaged) =
      "{\"ts\":"
        <> T.pack (show time)
        <> ",\"tokens\":"
        <> T.pack (show tokens)
        <> ",\"instant_rate\":"
        <> T.pack (show instant)
        <> ",\"rate\":"
        <> T.pack (show averaged)
        <> "}"
