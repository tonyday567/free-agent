{-# LANGUAGE OverloadedStrings #-}

-- | Fare docspec command and module-selection oracles.
module Axioma.Fare
  ( fareTopic,
  )
where

import Axioma.Common (assert)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Free.Agent.Fare.Cli (FareCommand (..), GateConfig (..), fareParser)
import Free.Agent.Fare.Exec (runTool)
import Free.Agent.Fare.Gate (DocspecScope (..), docspecArgs, docspecPackage)
import Options.Applicative (execParserPure, getParseResult, helper, info, prefs, (<**>))
import System.Exit (ExitCode (..))

fareTopic :: IO ()
fareTopic = do
  putStrLn "fare docspec scope"
  assert "bare gate excludes docspec" $ parseGate ["gate"] `selects` Nothing
  assert "--docspec selects repository scope" $ parseGate ["gate", "--docspec"] `selects` Just DocspecRepository
  assert "--docspec-full selects the linked plan" $ parseGate ["gate", "--docspec-full"] `selects` Just DocspecFull
  assert "docspec scope flags are mutually exclusive" $ isNothing (parseGate ["gate", "--docspec", "--docspec-full"])
  assert "repository package comes from its cabal file" $ docspecPackage ["src", "free-agent.cabal"] == Right "free-agent"
  assert "repository scope supplies the native package target" $ docspecArgs DocspecRepository "free-agent" == ["free-agent"]
  assert "full scope supplies no module or package restriction" $ null (docspecArgs DocspecFull "free-agent")

  repository <- phase1 DocspecRepository
  case repository of
    Right (ExitSuccess, out) -> do
      assert "repository docspec selects a root module" $ "docspec.phase1: Free.Agent.Fare.Gate:" `T.isInfixOf` out
      assert "repository docspec excludes the linked Circuit.Machine counterexample" $
        not ("docspec.phase1: Circuit.Machine:" `T.isInfixOf` out)
    _ -> assert "repository docspec phase 1 runs" False

  full <- phase1 DocspecFull
  case full of
    Right (ExitSuccess, out) -> do
      assert "full docspec retains a root module" $ "docspec.phase1: Free.Agent.Fare.Gate:" `T.isInfixOf` out
      assert "full docspec retains linked dependency modules" $ "docspec.phase1: Circuit.Machine:" `T.isInfixOf` out
    _ -> assert "full docspec phase 1 runs" False

parseGate :: [String] -> Maybe FareCommand
parseGate = getParseResult . execParserPure (prefs mempty) (info (fareParser <**> helper) mempty)

selects :: Maybe FareCommand -> Maybe DocspecScope -> Bool
selects (Just (GateCmd cfg)) scope = gaRoot cfg == "." && gaDocspec cfg == scope
selects _ _ = False

phase1 :: DocspecScope -> IO (Either T.Text (ExitCode, T.Text))
phase1 scope = runTool "." "cabal-docspec" ("--phase1" : docspecArgs scope "free-agent")
