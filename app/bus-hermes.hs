{-# LANGUAGE OverloadedStrings #-}

-- | Hermes-backed agent runner for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), forwards each body to
-- Hermes with the given system prompt, and writes the cleaned reply lines back
-- through the scribe with proper thread edges.
--
-- Usage:
--   free-agent-bus-hermes ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]
--     [--quiesce N] [--pitboss NAME]
module Main (main) where

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), PostId, mkPost, sortNub)
import Circuit.Agent.Framing (StoredPost, stampId, stamped)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Monad (guard)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File
  ( QuiesceConfig (..),
    cursorPath,
    findScribe,
    readCursor,
    scribePost,
    tailLog,
    writeCursor,
  )
import Free.Agent.Host (hermesHost)
import Free.Agent.Seat (FreeSeat, hostSeat, interpretSeat)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

-- | Parse optional quiescence flags, returning the remaining positional args.
parseQuiesceFlags :: [String] -> Maybe (Maybe QuiesceConfig, [String])
parseQuiesceFlags = go Nothing
  where
    go mq [] = Just (mq, [])
    go _ ("--quiesce" : n : rest) = case readMaybe @Int n of
      Just k | k > 0 -> go (Just (QuiesceConfig k "pitboss" 1000000)) rest
      _ -> Nothing
    go (Just qc) ("--pitboss" : name : rest) =
      go (Just (qc {qcPitboss = T.pack name})) rest
    go Nothing ("--pitboss" : _) = Nothing
    go mq (arg : rest)
      | "-" `isPrefixOf` arg = Nothing
      | otherwise = do
          (mq', rest') <- go mq rest
          pure (mq', arg : rest')

-- | Parse arguments of the form:
--   ROOT NAME [NAME...] PROMPT.md [SESSION-FILE] [--quiesce N] [--pitboss NAME]
parseArgs :: [String] -> Maybe (FilePath, [Name], FilePath, FilePath, Maybe QuiesceConfig)
parseArgs args = do
  (mQuiesce, posArgs) <- parseQuiesceFlags args
  case posArgs of
    [] -> Nothing
    (root : rest) -> do
      let (names, promptAndSess) = break (".md" `isSuffixOf`) rest
      promptFile <- listToMaybe promptAndSess
      let sessCandidates = drop 1 promptAndSess
      let sessionFile =
            case sessCandidates of
              [s] -> s
              _ -> root </> ".sessions" </> agentNameOf names <> ".sid"
      guard (not (null names))
      pure (root, map T.pack names, promptFile, sessionFile, mQuiesce)
  where
    agentNameOf [] = "agent"
    agentNameOf (n : _) = n

usage :: IO ()
usage = do
  TIO.putStrLn "Usage: free-agent-bus-hermes ROOT NAME [NAME...] PROMPT.md [SESSION-FILE]"
  TIO.putStrLn "                                          [--quiesce N] [--pitboss NAME]"
  TIO.putStrLn ""
  TIO.putStrLn "  ROOT            directory containing log.jsonl"
  TIO.putStrLn "  NAME            agent name(s) to subscribe to"
  TIO.putStrLn "  PROMPT.md       system prompt markdown file"
  TIO.putStrLn "  SESSION-FILE    optional Hermes session file (default: ROOT/.sessions/NAME.sid)"
  TIO.putStrLn "  --quiesce N     exit after N empty one-second cycles"
  TIO.putStrLn "  --pitboss NAME  recipient for the quiescence marker (default: pitboss)"

-- | Drop Hermes CLI noise lines that can precede the actual reply text,
-- so routing and empty-reply filtering work on the real model output.
scrubReply :: Post Text -> Post Text
scrubReply p =
  let ls = T.lines (body p)
      clean = filter (not . noise) ls
      noise l =
        T.null l
          || "↪" `T.isPrefixOf` l
          || "session_id:" `T.isPrefixOf` l
          || "Warning:" `T.isPrefixOf` l
          || "Resumed session" `T.isInfixOf` l
          || "Resume this session with:" `T.isInfixOf` l
          || "⚕" `T.isPrefixOf` l
          || "❯" `T.isPrefixOf` l
   in p {body = T.strip (T.unlines clean)}

-- | Parse a leading @name: prefix from a reply body. When present, redirect
-- the post to that name and strip the prefix.
routeReply :: Post Text -> Post Text
routeReply p =
  case T.stripPrefix "@" (body p) of
    Nothing -> p
    Just rest ->
      let (name, afterName) = T.break (== ':') rest
          name' = T.strip name
       in if T.null name' || T.null afterName
            then p
            else p {to = [name'], body = T.strip (T.drop 1 afterName)}

-- | Decorate an incoming post body with its sender so the LLM can tell who is
-- speaking on the bus. The original stamped post is untouched.
decorateSender :: StoredPost -> StoredPost
decorateSender stored =
  let p = stamped stored
   in stored {stamped = p {body = from p <> ": " <> body p}}

-- | Run one stored post through the seat and produce reply posts with thread
-- edges citing the parent id.
runOne :: FreeSeat -> StoredPost -> IO [Post Text]
runOne seat stored = do
  let stored' = decorateSender stored
      p = stamped stored'
      parentId = stampId stored
      sh = interpretSeat seat
  (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
  pure [routeReply (scrubReply out) {thread = sortNub (parentId : thread out)} | out <- outs]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Nothing -> do
      usage
      exitFailure
    Just (root, names, promptFile, sessionFile, mQuiesce) -> do
      systemPrompt <- TIO.readFile promptFile
      createDirectoryIfMissing True (takeDirectory sessionFile)
      let agentName = case names of (n : _) -> n; [] -> "agent"
          host = hermesHost agentName systemPrompt sessionFile
          seat = hostSeat host
      scribe <- findScribe
      TIO.putStrLn $ "🟢 bus hermes agent starting: " <> T.intercalate "," names
      TIO.putStrLn $ "   root: " <> T.pack root
      TIO.putStrLn $ "   prompt: " <> T.pack promptFile
      TIO.putStrLn $ "   session: " <> T.pack sessionFile
      TIO.putStrLn $ "   scribe: " <> T.pack scribe
      let path = root </> "log.jsonl"
          keepReply p =
            let b = T.strip (body p)
             in not (T.null b) && not ("⚠️" `T.isPrefixOf` b)
          onQuiesce = do
            let qc = maybe (error "quiesce action without config") id mQuiesce
                p = mkPost agentName [qcPitboss qc] ("🟡 quiescent after " <> T.pack (show (qcCycles qc)) <> " empty cycles")
            scribePost scribe root p
      cursor <- readCursor root agentName
      TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
      tailLog path names cursor (fmap (\qc -> (qc, onQuiesce)) mQuiesce) $ \stored -> do
        replies <- filter keepReply <$> runOne seat stored
        traverse_ (scribePost scribe root) replies
        writeCursor root agentName (stampId stored + 1)
