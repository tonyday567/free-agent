{-# LANGUAGE OverloadedStrings #-}

-- | Card archive CLI.
--
--   card-archive scribe [--log PATH] <card.md>
--   card-archive migrate [--log PATH] <dir>
--   card-archive edges [--log PATH]
--   card-archive lookup [--log PATH] <postId|name>
--
-- Default log: ./coffee-permanent.jsonl  (for migrate: <dir>/coffee-permanent.jsonl when --log omitted)
module Main (main) where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (StoredPost, Stamped (..), frameStored)
import Control.Monad (unless, when)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (scribeCard)
import Free.Agent.CardArchive
  ( cardNameOf,
    cardStatus,
    cardTitle,
    extractResidualOf,
    loadJsonl,
    migrateDir,
    nameEdges,
  )
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ("scribe" : rest) -> runScribe rest
    ("migrate" : rest) -> runMigrate rest
    ("edges" : rest) -> runEdges rest
    ("lookup" : rest) -> runLookup rest
    _ -> usage >> exitFailure

usage :: IO ()
usage = do
  TIO.putStrLn "Usage:"
  TIO.putStrLn "  card-archive scribe  [--log PATH] <card.md>"
  TIO.putStrLn "  card-archive migrate [--log PATH] <dir>"
  TIO.putStrLn "  card-archive edges   [--log PATH]"
  TIO.putStrLn "  card-archive lookup  [--log PATH] <postId|name>"
  TIO.putStrLn ""
  TIO.putStrLn "Default log: ./coffee-permanent.jsonl"
  TIO.putStrLn "migrate default log: <dir>/coffee-permanent.jsonl"

-- ---------------------------------------------------------------------------
-- Flag parsing
-- ---------------------------------------------------------------------------

-- | Parse @--log PATH@; remaining args are positional.
parseFlags :: [String] -> (Maybe FilePath, [String])
parseFlags = go Nothing
  where
    go _mlog ("--log" : p : rest) = go (Just p) rest
    go mlog (x : rest) =
      let (l, rs) = go mlog rest
       in (l, x : rs)
    go mlog [] = (mlog, [])

defaultLog :: FilePath
defaultLog = "coffee-permanent.jsonl"

-- ---------------------------------------------------------------------------
-- scribe
-- ---------------------------------------------------------------------------

runScribe :: [String] -> IO ()
runScribe args = do
  let (mlog, rest) = parseFlags args
  case rest of
    [cardPath] -> do
      exists <- doesFileExist cardPath
      if not exists
        then TIO.putStrLn ("🔴 missing card: " <> T.pack cardPath) >> exitFailure
        else do
          body <- TIO.readFile cardPath
          let name = cardNameOf cardPath
              logPath = fromMaybe defaultLog mlog
          existing <- loadJsonl logPath
          let nameToId =
                Map.fromList
                  [(from (stamped s), stampId s) | s <- existing]
              deps = extractResidualOf body
              thread = mapMaybe (`Map.lookup` nameToId) deps
              post = Post name [] thread body
          stored <- scribeCard logPath post
          TIO.putStrLn (frameStored stored)
          TIO.putStrLn $
            "🟢 scribed "
              <> name
              <> " id="
              <> T.pack (show (stampId stored))
              <> " → "
              <> T.pack logPath
          unless (null deps) $
            TIO.putStrLn $
              "   thread: "
                <> T.intercalate "," (map (T.pack . show) thread)
                <> " ("
                <> T.pack (show (length deps - length thread))
                <> " unresolved)"
    _ -> usage >> exitFailure

-- ---------------------------------------------------------------------------
-- migrate
-- ---------------------------------------------------------------------------

runMigrate :: [String] -> IO ()
runMigrate args = do
  let (mlog, rest) = parseFlags args
  case rest of
    [dir] -> do
      let logPath = fromMaybe (dir </> "cards.jsonl") mlog
      (posts, unresolved) <- migrateDir dir logPath
      TIO.putStrLn $
        "🟢 migrated "
          <> T.pack (show (length posts))
          <> " cards → "
          <> T.pack logPath
      let edges = nameEdges posts
      TIO.putStrLn $ "   edges: " <> T.pack (show (length edges))
      if null unresolved
        then TIO.putStrLn "   unresolved links: 0"
        else do
          TIO.putStrLn $
            "   unresolved links: "
              <> T.pack (show (length unresolved))
          traverse_ (\n -> TIO.putStrLn ("   · " <> n)) (take 20 unresolved)
          when (length unresolved > 20) $
            TIO.putStrLn $
              "   … +" <> T.pack (show (length unresolved - 20)) <> " more"
    _ -> usage >> exitFailure

-- ---------------------------------------------------------------------------
-- edges
-- ---------------------------------------------------------------------------

runEdges :: [String] -> IO ()
runEdges args = do
  let (mlog, rest) = parseFlags args
  case rest of
    [] -> do
      let logPath = fromMaybe defaultLog mlog
      posts <- loadJsonl logPath
      let edges = nameEdges posts
      TIO.putStrLn $
        "# "
          <> T.pack (show (length posts))
          <> " cards, "
          <> T.pack (show (length edges))
          <> " edges"
      traverse_ (\(c, p) -> TIO.putStrLn (c <> " --> " <> p)) edges
    _ -> usage >> exitFailure

-- ---------------------------------------------------------------------------
-- lookup
-- ---------------------------------------------------------------------------

runLookup :: [String] -> IO ()
runLookup args = do
  let (mlog, rest) = parseFlags args
  case rest of
    [key] -> do
      let logPath = fromMaybe defaultLog mlog
      posts <- loadJsonl logPath
      case findPost key posts of
        Nothing -> TIO.putStrLn ("🔴 not found: " <> T.pack key) >> exitFailure
        Just s -> do
          let p = stamped s
              body' = body p
          TIO.putStrLn $
            "id="
              <> T.pack (show (stampId s))
              <> " ts="
              <> stampTs s
              <> " from="
              <> from p
          TIO.putStrLn $
            "title: "
              <> fromMaybe "(none)" (cardTitle body')
          TIO.putStrLn $
            "status: "
              <> fromMaybe "(none)" (cardStatus body')
          TIO.putStrLn $
            "thread: "
              <> T.intercalate "," (map (T.pack . show) (thread p))
          TIO.putStrLn "---"
          TIO.putStr body'
    _ -> usage >> exitFailure

findPost :: String -> [StoredPost] -> Maybe StoredPost
findPost key posts =
  case readMaybe key :: Maybe Integer of
    Just n ->
      listToMaybe [s | s <- posts, stampId s == fromIntegral n]
    Nothing ->
      let name = T.pack key
       in listToMaybe [s | s <- posts, from (stamped s) == name]
