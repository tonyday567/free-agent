{-# LANGUAGE OverloadedStrings #-}

-- | Mechanical claim gate: filesystem-level atomic task claiming.
module Main (main) where

import Control.Monad (unless)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.Claim (checkTask, claimTask, listClaims, releaseTask, wipeClaims)
import Options.Applicative
import System.Exit (exitFailure)
import Prelude

taskArg :: Parser Int
taskArg =
  argument
    auto
    ( metavar "N"
        <> help "task number"
    )

rootArg :: Parser FilePath
rootArg =
  argument
    str
    ( metavar "ROOT"
        <> help "bus root directory"
    )

nameArg :: Parser T.Text
nameArg =
  argument
    (T.pack <$> str)
    ( metavar "NAME"
        <> help "claimant name"
    )

data ClaimCommand
  = Claim FilePath Int T.Text
  | Check FilePath Int
  | Release FilePath Int
  | List FilePath
  | Wipe FilePath
  deriving (Show)

claimParser :: Parser ClaimCommand
claimParser =
  hsubparser
    ( command "claim" (info (Claim <$> rootArg <*> taskArg <*> nameArg <**> helper) (progDesc "atomically claim task N for NAME"))
        <> command "check" (info (Check <$> rootArg <*> taskArg <**> helper) (progDesc "who holds task N"))
        <> command "release" (info (Release <$> rootArg <*> taskArg <**> helper) (progDesc "drop the claim"))
        <> command "list" (info (List <$> rootArg <**> helper) (progDesc "all current claims"))
        <> command "wipe" (info (Wipe <$> rootArg <**> helper) (progDesc "remove all claims (lab reset)"))
    )

opts :: ParserInfo ClaimCommand
opts =
  info
    (claimParser <**> helper)
    ( fullDesc
        <> progDesc "Filesystem-level atomic task claiming"
        <> header "bus-claim - claim, check, list, release, wipe"
    )

main :: IO ()
main = execParser opts >>= run

run :: ClaimCommand -> IO ()
run (Claim root n name) = do
  ok <- claimTask root n name
  unless ok exitFailure
run (Check root n) = do
  mh <- checkTask root n
  case mh of
    Nothing -> do
      TIO.putStrLn $ "task-" <> T.pack (show n) <> " free"
      exitFailure
    Just h -> TIO.putStrLn $ "task-" <> T.pack (show n) <> " held by " <> h
run (Release root n) = releaseTask root n
run (List root) = do
  claims <- listClaims root
  if null claims
    then TIO.putStrLn "no claims"
    else mapM_ (\(n', h) -> TIO.putStrLn (T.pack (show n') <> " " <> h)) claims
run (Wipe root) = wipeClaims root
