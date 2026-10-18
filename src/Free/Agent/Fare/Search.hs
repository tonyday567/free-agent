{-# LANGUAGE OverloadedStrings #-}

-- | fare search — bounded recursive substring search.
--
-- Layer 1 is a grep-style listing (@path:line:content@) under the
-- item's own byte budget ('layer1Bound', from wave 1: 78% of the
-- week's grep results were under 2KB), taken from the head by
-- default; --tail flips to the last lines under the same budget.
-- When the listing is truncated, the full listing is written to a
-- temp file the agent can slice with @fare read@ — deeper layers are
-- one ask away, not in the response.
--
-- Deliberate deviations from grep, recorded on cake/fare.md:
--
--   ⟜ substring match, case-sensitive — no regex (wave-1 definition
--     hunts are literal identifiers; regex is a morph if usage asks)
--   ⟜ exit 0 on no matches — "0 matches" is an answer, not an error;
--     the week audit showed grep's exit-1 mostly manufacturing
--     isError noise
--   ⟜ binary files are skipped, not mojibake'd (invalid UTF-8)
module Free.Agent.Fare.Search (runSearch) where

import Control.Monad (forM, when)
import Data.ByteString qualified as B
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO qualified as TIO
import Free.Agent.Fare.Config (End (..), applyEndUnder)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    pathIsSymbolicLink,
  )
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, isRegularFile)
import System.Posix.Process (getProcessID)

-- | the item's own layer-1 bound, in bytes (wave 1: 78% under 2KB).
layer1Bound :: Int
layer1Bound = 2000

-- | display cap for a single listed line; the full line lives in the
-- artifact file when the listing is truncated.
lineDisplayCap :: Int
lineDisplayCap = 240

-- | directories never searched — build products and VCS metadata.
skipDirs :: [String]
skipDirs = [".git", "dist", "dist-newstyle", ".stack-work", "node_modules", ".venv", "venv", ".hg", ".svn"]

-- | run a search and print the distilled layer-1 answer.
runSearch :: Text -> FilePath -> Maybe End -> IO ()
runSearch pat path mend = do
  isFile <- doesFileExist path
  files <- if isFile then pure [path] else collectFiles path
  found <- concat <$> forM files (matchesIn pat)
  let sorted = sort found
      total = length sorted
      totalFiles = length (nub [p | (p, _, _) <- sorted])
      listed = applyEndUnder layer1Bound (fromMaybe Head mend) (formatMatch <$> sorted)
  TIO.putStrLn (verdict total totalFiles (length listed) mend)
  mapM_ TIO.putStrLn listed
  when (length listed < total) $ do
    artifactPath <- writeArtifact sorted
    TIO.putStrLn ("full listing: " <> T.pack artifactPath <> " — next: fare read " <> T.pack artifactPath <> " 1,20")

-- ---------------------------------------------------------------------------
-- Traversal
-- ---------------------------------------------------------------------------

-- | collect files under ROOT, skipping symlinks and 'skipDirs'. The
-- order is sorted at every level so head/tail slices are stable.
collectFiles :: FilePath -> IO [FilePath]
collectFiles root = go root
  where
    go dir = do
      entries <- sort <$> listDirectory dir
      fmap concat $ forM entries $ \e -> do
        let p = dir </> e
        isLink <- pathIsSymbolicLink p
        if isLink
          then pure []
          else do
            isDir <- doesDirectoryExist p
            if isDir
              then
                if e `elem` skipDirs
                  then pure []
                  else go p
              else do
                regular <- isRegularFile <$> getFileStatus p
                pure [p | regular]

-- | match a pattern in one file; invalid UTF-8 (binary) yields no
-- matches rather than an error.
matchesIn :: Text -> FilePath -> IO [(FilePath, Int, Text)]
matchesIn pat p = do
  bytes <- B.readFile p
  pure $ case decodeUtf8' bytes of
    Left _ -> []
    Right content ->
      [ (p, n, l)
      | (n, l) <- zip [1 :: Int ..] (T.lines content),
        pat `T.isInfixOf` l
      ]

-- ---------------------------------------------------------------------------
-- Output
-- ---------------------------------------------------------------------------

formatMatch :: (FilePath, Int, Text) -> Text
formatMatch (p, n, l) =
  T.pack p <> ":" <> T.pack (show n) <> ":" <> clip l

-- | the layer-1 line cap; content beyond it lives in the artifact.
clip :: Text -> Text
clip l
  | T.length l > lineDisplayCap = T.take lineDisplayCap l <> " …"
  | otherwise = l

verdict :: Int -> Int -> Int -> Maybe End -> Text
verdict total totalFiles shown mend
  | total == 0 = "0 matches"
  | shown >= total = T.pack (show total) <> " matches"
  | otherwise =
      T.pack (show total)
        <> " matches in "
        <> T.pack (show totalFiles)
        <> " files — showing "
        <> endWord
        <> " "
        <> T.pack (show shown)
        <> " (bound "
        <> T.pack (show layer1Bound)
        <> " bytes)"
  where
    endWord = case fromMaybe Head mend of
      Head -> "first"
      Tail -> "last"

-- | write the full listing (unclipped lines) to a temp file, unique
-- per process so concurrent seats do not collide.
writeArtifact :: [(FilePath, Int, Text)] -> IO FilePath
writeArtifact found = do
  pid <- getProcessID
  tmp <- (</> ("fare-search-" <> show pid <> ".txt")) <$> getTemporaryDirectory
  TIO.writeFile tmp (T.unlines (formatFull <$> found))
  pure tmp

formatFull :: (FilePath, Int, Text) -> Text
formatFull (p, n, l) = T.pack p <> ":" <> T.pack (show n) <> ":" <> l
