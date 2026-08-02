{-# LANGUAGE OverloadedStrings #-}

-- | Two real process-backed agents (Abbott and Costello) wired through a
-- shared addressed log. Each agent is a 'Shard (StateT [Post Text] IO) [Post Text] [Post Text]'
-- backed by a Perl script.
module Main (main) where

import Circuit.Agent (Post (..), Shard, close, companion, conjoint)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Monad.State (StateT, runStateT)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Host (processHost)
import Free.Agent.Host qualified as Host
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Prelude

mkPost :: Text -> [Text] -> Text -> Post Text
mkPost a ds = Post a ds []

-- | Close a same-type shard once under 'StateT [Post Text] IO'.
runShardIO :: Shard (StateT [Post Text] IO) [Post Text] [Post Text] -> [Post Text] -> IO [Post Text]
runShardIO sh ins =
  fst <$> runStateT (runKleisli (close (conjoint sh) (companion sh)) ins) []

-- | Build a process-backed shard named @who@ that replies to the sender.
agentShard :: Text -> FilePath -> [String] -> Shard (StateT [Post Text] IO) [Post Text] [Post Text]
agentShard who script args =
  let h = (processHost who "perl" (script : args)) {Host.hostBodyMode = Host.BodyWhole}
   in Host.hostShard h

-- | Pretty-print one post.
printPost :: Post Text -> IO ()
printPost p =
  putStrLn $
    T.unpack (from p)
      <> " -> "
      <> T.unpack (T.intercalate "," (to p))
      <> ": "
      <> T.unpack (body p)

-- | Run alternating turns until nobody replies or we hit the round limit.
runDialogue ::
  Shard (StateT [Post Text] IO) [Post Text] [Post Text] ->
  Shard (StateT [Post Text] IO) [Post Text] [Post Text] ->
  [Post Text] ->
  Int ->
  IO [Post Text]
runDialogue _ _ log0 0 = pure log0
runDialogue abbott costello log0 rounds = go log0 0
  where
    go log n
      | n >= rounds = pure log
      | otherwise = do
          let target = if even n then "abbott" else "costello"
              agent = if even n then abbott else costello
          case find (\p -> target `elem` to p) log of
            Nothing -> do
              putStrLn $ "No post addressed to " <> T.unpack target <> "; quiescence."
              pure log
            Just p -> do
              outs <- runShardIO agent [p]
              if null outs
                then do
                  putStrLn $ T.unpack target <> " was silent; halting."
                  pure log
                else do
                  mapM_ printPost outs
                  go (outs ++ log) (n + 1)

main :: IO ()
main = do
  args <- getArgs
  (abbottScript, costelloScript) <- case args of
    [a, c] -> pure (a, c)
    _ ->
      pure
        ( "app/abbott-costello/abbott.pl",
          "app/abbott-costello/costello.pl"
        )

  putStrLn "Abbott & Costello process-agent wiring demo"
  putStrLn $ "  abbott script: " <> abbottScript
  putStrLn $ "  costello script: " <> costelloScript

  let abbott = agentShard "abbott" abbottScript []
      costello = agentShard "costello" costelloScript []

  let seed = mkPost "costello" ["abbott"] "Who's on first?"
  putStrLn "\nseed:"
  printPost seed
  putStrLn ""

  finalLog <- runDialogue abbott costello [seed] 12

  putStrLn "\n--- final log (newest first) ---"
  mapM_ printPost finalLog
