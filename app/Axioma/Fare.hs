{-# LANGUAGE OverloadedStrings #-}

-- | Fare docspec command and module-selection oracles.
module Axioma.Fare
  ( fareTopic,
  )
where

import Axioma.Common (assert)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Free.Agent.Fare.Cli (FareCommand (..), GateConfig (..), GateDocspec (..), IndexConfig (..), fareParser)
import Free.Agent.Fare.Config (parseFareConfig)
import Free.Agent.Fare.Exec (runTool)
import Free.Agent.Fare.Gate (DocspecScope (..), docspecArgs, docspecPackage)
import Free.Agent.Fare.Index (Area (..), areas, normalizeKeyword, renderRows, renderVerdict, snippetWidth)
import Options.Applicative (ParserResult (..), execParserPure, getParseResult, helper, info, prefs, renderFailure, (<**>))
import System.Exit (ExitCode (..))

fareTopic :: IO ()
fareTopic = do
  putStrLn "fare docspec scope"
  assert "bare gate defers to repository configuration" $ parseGate ["gate"] `selects` RepositoryDefault
  assert "--no-docspec explicitly excludes docspec" $ parseGate ["gate", "--no-docspec"] `selects` DocspecExcluded
  assert "--docspec selects repository scope" $ parseGate ["gate", "--docspec"] `selects` DocspecSelected DocspecRepository
  assert "--docspec-full selects the linked plan" $ parseGate ["gate", "--docspec-full"] `selects` DocspecSelected DocspecFull
  assert "docspec scope flags are mutually exclusive" $ isNothing (parseGate ["gate", "--docspec", "--docspec-full"])
  assert "explicit exclusion and inclusion are mutually exclusive" $ isNothing (parseGate ["gate", "--no-docspec", "--docspec"])
  assert "empty repository configuration excludes docspec" $ parseFareConfig "" == Right Nothing
  assert "repository configuration selects the house gate" $ parseFareConfig "gate.docspec = repository\n" == Right (Just DocspecRepository)
  assert "full configuration selects the linked plan" $ parseFareConfig "gate.docspec=full\n" == Right (Just DocspecFull)
  assert "none configuration explicitly excludes docspec" $ parseFareConfig "# quick lane\ngate.docspec = none\n" == Right Nothing
  assert "unknown docspec configuration is rejected" $ isLeft (parseFareConfig "gate.docspec = sometimes\n")
  assert "unknown repository settings are rejected" $ isLeft (parseFareConfig "gate.docepcs = repository\n")
  assert "repository package comes from its cabal file" $ docspecPackage ["src", "free-agent.cabal"] == Right "free-agent"
  assert "repository scope supplies the native package target" $ docspecArgs DocspecRepository "free-agent" == ["free-agent"]
  assert "full scope supplies no module or package restriction" $ null (docspecArgs DocspecFull "free-agent")

  putStrLn "fare index (decayed-area query)"
  assert "index parses KEYWORD with the store default and no cutoff" $
    case parseIndex ["index", "git"] of
      Just (IndexCmd cfg) -> inKeyword cfg == "git" && inRoot cfg == "" && inCutoff cfg == Nothing
      _ -> False
  assert "index parses explicit ROOT and --cutoff" $
    case parseIndex ["index", "git", "/tmp/store", "--cutoff", "1"] of
      Just (IndexCmd cfg) -> inRoot cfg == "/tmp/store" && inCutoff cfg == Just 1
      _ -> False
  assert "index refuses a non-numeric --cutoff" $ isNothing (parseIndex ["index", "git", "--cutoff", "hot"])
  assert "index refuses a missing KEYWORD" $ isNothing (parseIndex ["index"])
  assert "help names the store default and the cutoff default" $
    case execParserPure (prefs mempty) (info (fareParser <**> helper) mempty) ["index", "--help"] of
      Failure failure ->
        let text = T.pack (fst (renderFailure failure "fare"))
         in "~/.kimi-code/sessions" `T.isInfixOf` text && "0.5" `T.isInfixOf` text
      _ -> False
  -- the contract's falsifiers, on the contract's own fixture text
  assert "a keyword is normalized to lowercase" $ normalizeKeyword " Git " == "git"
  assert "Git and git are the same query" $
    areas 0.5 (normalizeKeyword "Git") fixturePartOne == areas 0.5 (normalizeKeyword "git") fixturePartOne
  assert "a lowercase token matches inside a token, not just exactly" $
    [(arStart a, arEnd a, arPeak a) | a <- areas 0.5 "git" "github"] == [(0, 0, 1)]
  assert "the decayed run reproduces kw-index" $
    [(arStart a, arEnd a, arPeak a) | a <- areas 0.5 "git" fixturePartOne] == [(0, 3, 3.439)]
  assert "the run continues while decay stays above the cutoff" $
    [(arStart a, arEnd a, arPeak a) | a <- areas 0.5 "git" "alpha git omega"] == [(1, 2, 1)]
  assert "a score exactly equal to the cutoff is not an area" $ null (areas 1 "git" "alpha git omega")
  assert "a zero-hit keyword has no areas" $ null (areas 0.5 "zzz" fixturePartOne)
  assert "the verdict is the contract's first line" $
    renderVerdict "git" 2 1 1 1 0.5 == "index git: 2 areas in 1/1 wires · parse fails 1 · cutoff 0.5 · decay 0.9"
  assert "rows are headed and ordered by descending peak" $
    renderRows [area "b" 1.0, area "a" 3.439]
      == ["path\tpart\tstart\tend\tpeak\tsnippet", "a\t0\t0\t0\t3.439\tsnip", "b\t0\t0\t0\t1.0\tsnip"]
  assert "a snippet carries no more than the fixed width" $
    all ((<= snippetWidth) . length . T.words . arSnippet) (areas 0.1 "git" (T.unwords (replicate 40 "git")))

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

parseIndex :: [String] -> Maybe FareCommand
parseIndex = getParseResult . execParserPure (prefs mempty) (info (fareParser <**> helper) mempty)

-- | the first content part of the contract's fixture wire.
fixturePartOne :: T.Text
fixturePartOne = "Git github git git"

area :: FilePath -> Double -> Area
area p peak = Area {arPath = p, arPart = 0, arStart = 0, arEnd = 0, arPeak = peak, arSnippet = "snip"}

selects :: Maybe FareCommand -> GateDocspec -> Bool
selects (Just (GateCmd cfg)) scope = gaRoot cfg == "." && gaDocspec cfg == scope
selects _ _ = False

phase1 :: DocspecScope -> IO (Either T.Text (ExitCode, T.Text))
phase1 scope = runTool "." "cabal-docspec" ("--phase1" : docspecArgs scope "free-agent")
