{-# LANGUAGE OverloadedStrings #-}

-- | Card archive: markdown cards as a stamped JSONL log.
--
-- Same image type as the bus ('StoredPost'), separate log file
-- (@coffee-permanent.jsonl@). Body is raw markdown ('Card' = 'Text'). Structured
-- residual-of edges live in 'Post.thread' as 'PostId's.
--
-- Design: coffee loom @card-archive.md@.
module Free.Agent.CardArchive
  ( -- * Identity
    CardName,
    cardNameOf,
    cardTitle,
    cardStatus,

    -- * Link extraction
    extractLinkNames,
    extractResidualOf,

    -- * Migration
    migrateDir,
    loadJsonl,
    threadEdges,
    nameEdges,
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing
  ( StoredPost,
    Stamped (..),
    frameStored,
    formatNow,
    parseLine,
  )
import Data.Foldable (traverse_)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>), (<.>))
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.IO (IOMode (WriteMode), withFile)
import Prelude

-- | Card identity: archive basename without @.md@.
type CardName = Text

-- | Basename of a card path as its name.
cardNameOf :: FilePath -> CardName
cardNameOf = T.pack . takeBaseName

-- | First @#@ heading in the body, stripped of the leading hashes.
cardTitle :: Text -> Maybe Text
cardTitle body =
  case mapMaybe heading (T.lines body) of
    (t : _) -> Just t
    [] -> Nothing
  where
    heading line =
      let t = T.strip line
       in if T.isPrefixOf "#" t
            then Just (T.strip . T.dropWhile (== '#') $ t)
            else Nothing

-- | Status line: first line that starts with @status@ (after optional
-- leading whitespace).
cardStatus :: Text -> Maybe Text
cardStatus body =
  case mapMaybe statusLine (T.lines body) of
    (s : _) -> Just s
    [] -> Nothing
  where
    statusLine line =
      let t = T.strip line
       in if T.isPrefixOf "status" t then Just t else Nothing

-- | Extract local markdown link targets of the form @](name.md)@ or
-- @](name)@, dropping URLs and paths with separators.
extractLinkNames :: Text -> [CardName]
extractLinkNames = nub . mapMaybe fromTarget . linkTargets
  where
    linkTargets body = go body
      where
        go t =
          case T.breakOn "](" t of
            (_, rest)
              | T.null rest -> []
              | otherwise ->
                  let after = T.drop 2 rest -- drop "]("
                      (target, more) = T.break (== ')') after
                   in target : go (T.drop 1 more)
    fromTarget raw =
      let t = T.strip raw
          noHash = T.takeWhile (/= '#') t
          base = T.pack . takeBaseName . T.unpack $ noHash
       in if T.null base
            || T.any (`elem` ("/:" :: String)) noHash
            || T.isPrefixOf "http" noHash
            then Nothing
            else Just base

-- | Names listed on @residual-of:@ lines (comma-separated, optional @.md@).
extractResidualOf :: Text -> [CardName]
extractResidualOf body =
  nub $
    concatMap parseLine' (T.lines body)
  where
    parseLine' line =
      let t = T.strip line
       in if T.isPrefixOf "residual-of:" t
            then
              map cleanName
                . filter (not . T.null)
                . map T.strip
                . T.splitOn ","
                $ T.drop (T.length "residual-of:") t
            else []
    cleanName n =
      let n' = T.strip n
       in if T.isSuffixOf ".md" n' then T.dropEnd 3 n' else n'

-- | All parent names a card should cite in 'thread': residual-of lines plus
-- local markdown links, excluding self.
parentNames :: CardName -> Text -> [CardName]
parentNames self body =
  filter (/= self) . nub $ extractResidualOf body ++ extractLinkNames body

-- | Two-pass migrate: assign stable postIds by sorted basename, resolve
-- link names to ids, write @cards.jsonl@ under lock.
--
-- Returns @(posts written, unresolved link names)@.
migrateDir :: FilePath -> FilePath -> IO ([StoredPost], [CardName])
migrateDir dir logPath = do
  entries <- listDirectory dir
  let mdFiles =
        sort
          [ dir </> f
            | f <- entries,
              takeExtension f == ".md"
          ]
  named <- traverse readCard mdFiles
  let nameToId :: Map CardName PostId
      nameToId =
        Map.fromList
          [ (name, fromIntegral i)
            | (i, (name, _)) <- zip [0 :: Int ..] named
          ]
      unresolved = nub $
        concat
          [ [n | n <- parentNames name body, not (Map.member n nameToId)]
            | (name, body) <- named
          ]
  ts <- formatNow
  let posts =
        [ Stamped pid ts (Post name [] thread body)
          | (i, (name, body)) <- zip [0 :: Int ..] named,
            let pid = fromIntegral i
                thread =
                  mapMaybe (`Map.lookup` nameToId) (parentNames name body)
        ]
  createDirectoryIfMissing True (takeDirectory logPath)
  writeJsonlAtomic logPath posts
  pure (posts, unresolved)
  where
    readCard fp = do
      body <- TIO.readFile fp
      pure (cardNameOf fp, body)

-- | Write a full JSONL image under lock (overwrite).
writeJsonlAtomic :: FilePath -> [StoredPost] -> IO ()
writeJsonlAtomic path posts =
  withFileLock (path <.> "lock") Exclusive $ \_lock ->
    withFile path WriteMode $ \h ->
      traverse_ (TIO.hPutStrLn h . frameStored) posts

-- | Load a cards log into memory (oldest first). Malformed lines skipped.
loadJsonl :: FilePath -> IO [StoredPost]
loadJsonl path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      lines' <- T.lines <$> TIO.readFile path
      pure (mapMaybe parseLine lines')

-- | Edge list as @(childId, parentId)@ pairs from 'thread'.
threadEdges :: [StoredPost] -> [(PostId, PostId)]
threadEdges posts =
  [ (stampId s, parent)
    | s <- posts,
      parent <- thread (stamped s)
  ]

-- | Edge list as @(childName, parentName)@ using 'from' as the card name.
nameEdges :: [StoredPost] -> [(CardName, CardName)]
nameEdges posts =
  let idToName =
        Map.fromList
          [(stampId s, from (stamped s)) | s <- posts]
   in [ (child, parent)
        | s <- posts,
          let child = from (stamped s),
          pid <- thread (stamped s),
          Just parent <- [Map.lookup pid idToName]
      ]
