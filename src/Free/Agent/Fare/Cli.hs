{-# LANGUAGE OverloadedStrings #-}

-- | fare CLI — free-agent restricted equipment, a parsimonious tool menu.
--
-- The distilled answer is the interface and deeper layers are one ask away
-- (cake/fare.md §design principles). Each item's layer-1 answer is bounded
-- by its own distillation, including which end the slice takes; --head and
-- --tail exist only as testing knobs for untried seats and harnessings,
-- overriding the end with no steady-state default.
--
-- Subcommands (cake/fare.md; all live):
--
--   search PATTERN [PATH] [--head|--tail]   bounded recursive substring search
--   read FILE [RANGE] [--head|--tail]       file slices, never whole files
--   gate [ROOT] [--docspec]                 distilled build/lint verdict
--   state [ROOT] [--head|--tail]            distilled git status/log/diff
module Free.Agent.Fare.Cli
  ( SearchConfig (..),
    ReadConfig (..),
    GateConfig (..),
    StateConfig (..),
    FareCommand (..),
    fareParser,
    runFareCommand,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Fare.Config (End, endOpt, rootArg)
import Free.Agent.Fare.Gate (runGate)
import Free.Agent.Fare.Read (runRead)
import Free.Agent.Fare.Search (runSearch)
import Free.Agent.Fare.State (runState)
import Options.Applicative

-- ---------------------------------------------------------------------------
-- Config records
-- ---------------------------------------------------------------------------

-- | search configuration.
data SearchConfig = SearchConfig
  { sePattern :: Text,
    sePath :: FilePath,
    seEnd :: Maybe End
  }
  deriving (Show)

-- | read configuration. Range is a slice spec such as @40,80@ or @+40@;
-- the parser lands with the implementation.
data ReadConfig = ReadConfig
  { reFile :: FilePath,
    reRange :: Maybe Text,
    reEnd :: Maybe End
  }
  deriving (Show)

-- | gate configuration: distilled build/lint verdict over ROOT.
-- --head/--tail do not exist here: a verdict has no slice to flip.
data GateConfig = GateConfig
  { gaRoot :: FilePath,
    gaDocspec :: Bool
  }
  deriving (Show)

-- | state configuration: distilled git status/log/diff over ROOT.
data StateConfig = StateConfig
  { stRoot :: FilePath,
    stEnd :: Maybe End
  }
  deriving (Show)

data FareCommand
  = SearchCmd SearchConfig
  | ReadCmd ReadConfig
  | GateCmd GateConfig
  | StateCmd StateConfig
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Shared options (End, endOpt, rootArg live in Free.Agent.Fare.Config)
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Subcommand parsers
-- ---------------------------------------------------------------------------

searchParser :: Parser FareCommand
searchParser =
  SearchCmd
    <$> ( SearchConfig
            <$> argument (T.pack <$> str) (metavar "PATTERN" <> help "Search pattern")
            <*> rootArg
            <*> endOpt
        )

readParser :: Parser FareCommand
readParser =
  ReadCmd
    <$> ( ReadConfig
            <$> argument str (metavar "FILE" <> help "File to slice")
            <*> optional
              ( argument
                  (T.pack <$> str)
                  ( metavar "RANGE"
                      <> help "Line slice, e.g. 40,80 or +40"
                  )
              )
            <*> endOpt
        )

gateParser :: Parser FareCommand
gateParser =
  GateCmd
    <$> ( GateConfig
            <$> rootArg
            <*> switch (long "docspec" <> help "Add cabal-docspec to the suite (30s+; tests the last build)")
        )

stateParser :: Parser FareCommand
stateParser =
  StateCmd
    <$> (StateConfig <$> rootArg <*> endOpt)

-- ---------------------------------------------------------------------------
-- Top-level parser
-- ---------------------------------------------------------------------------

fareParser :: Parser FareCommand
fareParser =
  subparser
    ( command "search" (info (searchParser <**> helper) (progDesc "Bounded search over files"))
        <> command "read" (info (readParser <**> helper) (progDesc "File slices, never whole files"))
        <> command "gate" (info (gateParser <**> helper) (progDesc "Distilled build/lint verdict"))
        <> command "state" (info (stateParser <**> helper) (progDesc "Distilled git status/log/diff"))
    )

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

runFareCommand :: FareCommand -> IO ()
runFareCommand = \case
  SearchCmd cfg ->
    runSearch (sePattern cfg) (sePath cfg) (seEnd cfg)
  ReadCmd cfg ->
    runRead (reFile cfg) (reRange cfg) (reEnd cfg)
  GateCmd cfg ->
    runGate (gaRoot cfg) (gaDocspec cfg)
  StateCmd cfg ->
    runState (stRoot cfg) (stEnd cfg)
