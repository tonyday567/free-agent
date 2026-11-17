{-# LANGUAGE OverloadedStrings #-}

-- | Mechanical claim gate for a free-agent bus root.
--
-- .claim-N files are the authority; the bus post is only the record.
-- Exclusive creation decides ownership atomically.
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

import Control.Exception (IOException, throwIO, try)
import Control.Monad (forM, when)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (readFileStrict)
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, stderr)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.IO (OpenMode (WriteOnly), creat, defaultFileFlags, exclusive, fdToHandle, openFd)
import Text.Read (readMaybe)

-- | Path to the claim lock file for a task.
claimPath :: FilePath -> Int -> FilePath
claimPath root n = root </> (".claim-" <> show n)

-- | Claim task @n@ for @name@. Exclusive creation is the claim decision;
-- a holder that already exists wins without another write.
claimTask :: FilePath -> Int -> Text -> IO Bool
claimTask root n name = do
  let path = claimPath root n
  created <- try @IOException $ do
    fd <- openFd path WriteOnly defaultFileFlags {creat = Just 0o600, exclusive = True}
    handle <- fdToHandle fd
    TIO.hPutStr handle (name <> " 0")
    hClose handle
  case created of
    Right () -> pure True
    Left err
      | isAlreadyExistsError err -> do
          holder <- readHolder path
          TIO.putStrLn $ "task-" <> T.pack (show n) <> " held by " <> holder
          pure False
      | otherwise -> throwIO err

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
  txt <- readFileStrict path
  pure $ T.takeWhile (/= ' ') (T.strip txt)

-- | Release (delete) the claim on task @n@.
releaseTask :: FilePath -> Int -> IO ()
releaseTask root n = do
  let path = claimPath root n
  e <- doesFileExist path
  when e $ removeFile path

-- | List all current claims. Foreign or malformed marker names are reported
-- and skipped rather than making the whole listing partial.
listClaims :: FilePath -> IO [(Int, Text)]
listClaims root = do
  ents <- listDirectory root
  let claims = [ent | ent <- ents, ".claim-" `T.isPrefixOf` T.pack ent]
  catMaybes
    <$> forM
      claims
      ( \ent -> case readMaybe (drop 7 ent) of
          Nothing -> do
            TIO.hPutStrLn stderr ("🟡 ignoring malformed claim marker: " <> T.pack ent)
            pure Nothing
          Just n -> do
            holder <- readHolder (root </> ent)
            pure (Just (n, holder))
      )

-- | Remove all claim files in the bus root.
wipeClaims :: FilePath -> IO ()
wipeClaims root = do
  ents <- listDirectory root
  mapM_
    (\ent -> removeFile (root </> ent))
    [ent | ent <- ents, ".claim-" `T.isPrefixOf` T.pack ent]
