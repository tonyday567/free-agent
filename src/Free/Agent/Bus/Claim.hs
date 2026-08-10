{-# LANGUAGE OverloadedStrings #-}

-- | Mechanical claim gate for a free-agent bus root.
--
-- .claim-N files are the authority; the bus post is only the record.
-- Uses check-then-create — the race window is microseconds vs 30s LLM latency.
module Free.Agent.Bus.Claim
  ( -- * Claim operations
    claimTask,
    checkTask,
    releaseTask,
    listClaims,
    wipeClaims,

    -- * Paths
    claimPath,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))

-- | Path to the claim lock file for a task.
claimPath :: FilePath -> Int -> FilePath
claimPath root n = root </> (".claim-" <> show n)

-- | Claim task @n@ for @name@.  Returns 'True' on success, 'False' if
-- already claimed (file exists when we check).
claimTask :: FilePath -> Int -> Text -> IO Bool
claimTask root n name = do
  let path = claimPath root n
  e <- doesFileExist path
  if e
    then do
      holder <- readHolder path
      TIO.putStrLn $ "task-" <> T.pack (show n) <> " held by " <> holder
      pure False
    else do
      TIO.writeFile path (name <> " 0")
      pure True

-- | Who holds task @n@?  Returns 'Nothing' if unclaimed.
checkTask :: FilePath -> Int -> IO (Maybe Text)
checkTask root n = do
  let path = claimPath root n
  e <- doesFileExist path
  if not e
    then pure Nothing
    else Just <$> readHolder path

-- | Read the first word of a claim file (the holder name).
readHolder :: FilePath -> IO Text
readHolder path = do
  txt <- TIO.readFile path
  pure $ T.takeWhile (/= ' ') (T.strip txt)

-- | Release (delete) the claim on task @n@.
releaseTask :: FilePath -> Int -> IO ()
releaseTask root n = do
  let path = claimPath root n
  e <- doesFileExist path
  if e then removeFile path else pure ()

-- | List all current claims.  Returns @[(task number, holder)]@.
listClaims :: FilePath -> IO [(Int, Text)]
listClaims root = do
  ents <- listDirectory root
  let claims = [ent | ent <- ents, ".claim-" `T.isPrefixOf` T.pack ent]
  mapM (\ent -> do
    let n = read (drop 7 ent)
    holder <- readHolder (root </> ent)
    pure (n, holder)) claims

-- | Remove all claim files in the bus root.
wipeClaims :: FilePath -> IO ()
wipeClaims root = do
  ents <- listDirectory root
  mapM_ (\ent -> removeFile (root </> ent))
    [ ent | ent <- ents, ".claim-" `T.isPrefixOf` T.pack ent ]
