{-# LANGUAGE OverloadedStrings #-}

-- | Receiver-owned wake state. Quarantined posts remain stored but cannot buy
-- a turn until an explicit admission moves their ids to the admitted set.
module Free.Agent.Wake
  ( quarantinePath,
    admittedPath,
    quarantinedPosts,
    reviewAdmitted,
    readAdmitted,
    quarantinePosts,
    admitPosts,
    clearAdmitted,
  )
where

import Circuit.Parser.Framing (Stored)
import Circuit.Post (PostId)
import Circuit.Shared (stamp)
import Control.Monad (when)
import Data.List (nub, sort, (\\))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Agent (Name)
import Free.Agent.Bus (readFileStrict, readLogStrict)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | Currently refused wakes, one stored post id per line. This is receiver
-- state, not a bus post and not a second settlement cursor.
quarantinePath :: FilePath -> Name -> FilePath
quarantinePath root name = root </> (".quarantine-" <> T.unpack name)

-- | Previously quarantined ids explicitly made eligible for one delivery.
-- Successful settlement removes the ids; failure leaves them for a restart.
admittedPath :: FilePath -> Name -> FilePath
admittedPath root name = root </> (".admitted-" <> T.unpack name)

quarantinedPosts :: FilePath -> Name -> IO [Stored]
quarantinedPosts = postsAt quarantinePath

reviewAdmitted :: FilePath -> Name -> IO [Stored]
reviewAdmitted = postsAt admittedPath

readAdmitted :: FilePath -> Name -> IO [PostId]
readAdmitted root name = readIds (admittedPath root name)

-- | Record refused wakes without changing the bus log or delivery cursor.
quarantinePosts :: FilePath -> Name -> [PostId] -> IO ()
quarantinePosts _ _ [] = pure ()
quarantinePosts root name ids = do
  let path = quarantinePath root name
  existing <- readIds path
  admitted <- readAdmitted root name
  writeIds path (orderedNub (existing <> filter (`notElem` admitted) ids))

-- | Admit only currently quarantined ids. A delivered id is no longer
-- quarantined, so admitting it again is refused rather than replayed.
-- Writing admission first makes a crash between the two writes safe: the id
-- may occur in both sets, and admission wins without losing the stored post.
admitPosts :: FilePath -> Name -> [PostId] -> IO (Either Text [PostId])
admitPosts root name ids = do
  let requested = orderedNub ids
      quarantine = quarantinePath root name
      admitted = admittedPath root name
  quarantined <- readIds quarantine
  let missing = requested \\ quarantined
  if not (null missing)
    then pure (Left ("not quarantined for " <> name <> ": " <> renderIds missing))
    else do
      existing <- readIds admitted
      writeIds admitted (orderedNub (existing <> requested))
      writeIds quarantine (quarantined \\ requested)
      pure (Right requested)

-- | Clear admitted ids as part of committing their batch, before transaction
-- and cursor evidence. A crash earlier leaves them eligible for recovery; a
-- crash later can only quarantine the unsettled post again, never replay a
-- committed admission from stale state.
clearAdmitted :: FilePath -> Name -> [PostId] -> IO ()
clearAdmitted root name ids = do
  let path = admittedPath root name
  exists <- doesFileExist path
  when exists $ do
    existing <- readIds path
    writeIds path (existing \\ ids)

postsAt :: (FilePath -> Name -> FilePath) -> FilePath -> Name -> IO [Stored]
postsAt statePath root name = do
  ids <- readIds (statePath root name)
  if null ids
    then pure []
    else do
      posts <- readLogStrict (root </> "log.jsonl")
      pure (filter ((`elem` ids) . snd . stamp) posts)

readIds :: FilePath -> IO [PostId]
readIds path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      content <- readFileStrict path
      traverse (parseId path) (filter (not . T.null) (T.lines content))

parseId :: FilePath -> Text -> IO PostId
parseId path value =
  maybe (ioError (userError (path <> ": invalid post id: " <> T.unpack value))) pure (readMaybe (T.unpack value))

writeIds :: FilePath -> [PostId] -> IO ()
writeIds path ids = TIO.writeFile path (T.unlines (map (T.pack . show) ids))

orderedNub :: [PostId] -> [PostId]
orderedNub = sort . nub

renderIds :: [PostId] -> Text
renderIds = T.intercalate "," . map (T.pack . show)
