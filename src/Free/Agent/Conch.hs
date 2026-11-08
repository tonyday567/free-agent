{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Durable Conch execution over a free-agent JSONL bus root.
--
-- 'processConchPost' is the file-truth boundary.  It normalises Claim paths,
-- serialises the read/decide/append sequence under the bus log lock, and
-- appends any system Handover immediately after its causing Claim.  Recovery
-- folds the same stamped history through 'reconstructConch' without appending
-- posts or writing recipient pings.
module Free.Agent.Conch
  ( ConchReceipt (..),
    processConchPost,
    recoverConch,
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Conch
  ( Conch (..),
    ConchConfig,
    ConchPayload,
    ConchPost,
    ConchState,
    emptyConchState,
    reconstructConch,
    stepConch,
  )
import Circuit.Agent.Framing
  ( Log (..),
    Stamped,
    stamp,
    stamped,
    unframeStored,
    pattern Stamped,
  )
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Free.Agent.Bus
  ( appendStoredPostsUnlocked,
    readLogStrict,
    touchLog,
    withLogLock,
    writePings,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath (isRelative, normalise, (<.>), (</>))

-- | Durable evidence for one submitted post.  A refused Claim or an
-- unauthorised Handover has no system Handovers; ordinary conversation and
-- refused operations still remain on the public tape.
data ConchReceipt = ConchReceipt
  { receiptPost :: Stamped ConchPayload,
    receiptHandovers :: [Stamped ConchPayload],
    receiptState :: ConchState
  }
  deriving (Eq, Show)

-- | Append and execute one Conch post against the current file truth.
--
-- The bus root is also the base for relative reservation paths.  The stored
-- Claim carries absolute, lexically normalised paths, so recovery does not
-- depend on a later process working directory.  The log lock orders competing
-- Claims and their binding Handovers across processes using this bus.
processConchPost :: ConchConfig -> FilePath -> ConchPost -> IO ConchReceipt
processConchPost cfg root post = do
  absoluteRoot <- makeAbsolute root
  createDirectoryIfMissing True absoluteRoot
  let path = absoluteRoot </> "log.jsonl"
  exists <- doesFileExist path
  unless exists $ touchLog path
  (receipt, appended) <- withLogLock (path <.> "lock") $ do
    (prior, nextId) <- readLogUnlocked absoluteRoot path
    now <- getCurrentTime
    let incoming = Stamped (now, nextId) (normaliseConchPost absoluteRoot post)
        priorPairs = stampedPairs prior
        state = reconstructConch cfg priorPairs
        byId = Map.fromList priorPairs
        lookupPost = (`Map.lookup` byId)
        (_, responses) = stepConch cfg lookupPost state nextId (stamped incoming)
        handovers =
          [ Stamped (now, nextId + fromIntegral offset) response
          | (offset, response) <- zip [1 :: Int ..] responses
          ]
        appendedPosts = incoming : handovers
        finalState = reconstructConch cfg (stampedPairs (prior ++ appendedPosts))
    appendStoredPostsUnlocked path appendedPosts
    pure (ConchReceipt incoming handovers finalState, appendedPosts)
  writePings path appended
  pure receipt

-- | Reconstruct current custody and reservations from durable Handovers.
-- This performs no appends, system transitions, or ping writes.
recoverConch :: ConchConfig -> FilePath -> IO ConchState
recoverConch cfg root = do
  absoluteRoot <- makeAbsolute root
  let path = absoluteRoot </> "log.jsonl"
  exists <- doesFileExist path
  if not exists
    then pure emptyConchState
    else do
      Log posts <- readLogStrict path
      pure (reconstructConch cfg (stampedPairs (map (normaliseStored absoluteRoot) posts)))

readLogUnlocked :: FilePath -> FilePath -> IO ([Stamped ConchPayload], PostId)
readLogUnlocked root path = do
  bytes <- BS.readFile path
  let nextId = fromIntegral (BS.count 0x0A bytes)
      posts = mapMaybe unframeStored (filter (not . T.null) (T.lines (TE.decodeUtf8 bytes)))
  pure (map (normaliseStored root) posts, nextId)

stampedPairs :: [Stamped ConchPayload] -> [(PostId, ConchPost)]
stampedPairs = map (\stored -> (snd (stamp stored), stamped stored))

normaliseStored :: FilePath -> Stamped ConchPayload -> Stamped ConchPayload
normaliseStored root (Stamped token post) = Stamped token (normaliseConchPost root post)

normaliseConchPost :: FilePath -> ConchPost -> ConchPost
normaliseConchPost root post = post {body = first (fmap normaliseConch) (body post)}
  where
    normaliseConch (Claim paths) = Claim (map (normalisePath root) paths)
    normaliseConch Handover = Handover

normalisePath :: FilePath -> FilePath -> FilePath
normalisePath root path = normalise (if isRelative path then root </> path else path)
