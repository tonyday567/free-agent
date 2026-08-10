{-# LANGUAGE OverloadedStrings #-}

-- | Mechanical claim gate: filesystem-level atomic task claiming.
--
-- Usage:
--   bus-claim claim ROOT N NAME    — atomically claim task N for NAME
--   bus-claim check ROOT N         — who holds task N
--   bus-claim release ROOT N       — drop the claim
--   bus-claim list ROOT            — all current claims
--   bus-claim wipe ROOT            — remove all claims (lab reset)
--
-- The .claim-N file is the authority; the bus post is only the record.
module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.Claim (checkTask, claimTask, listClaims, releaseTask, wipeClaims)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["claim", root, nStr, name] -> do
      let n = read nStr
      ok <- claimTask root n (T.pack name)
      if not ok then exitFailure else pure ()

    ["check", root, nStr] -> do
      let n = read nStr
      mh <- checkTask root n
      case mh of
        Nothing -> do
          TIO.putStrLn $ "task-" <> T.pack (show n) <> " free"
          exitFailure
        Just h -> TIO.putStrLn $ "task-" <> T.pack (show n) <> " held by " <> h

    ["release", root, nStr] ->
      releaseTask root (read nStr)

    ["list", root] -> do
      claims <- listClaims root
      if null claims
        then TIO.putStrLn "no claims"
        else mapM_ (\(n, h) -> TIO.putStrLn (T.pack (show n) <> " " <> h)) claims

    ["wipe", root] ->
      wipeClaims root

    _ -> do
      TIO.putStrLn "usage: bus-claim claim|check|list|release|wipe ROOT [N] [NAME]"
      exitFailure
