{-# LANGUAGE OverloadedStrings #-}

-- | fare CLI — free-agent restricted equipment, a parsimonious tool menu.
--
-- The distilled answer is the interface and deeper layers are one ask away
-- (storage/coffee-cards/fare.md §design principles). Each item's layer-1 answer is bounded
-- by its own distillation, including which end the slice takes; --head and
-- --tail exist only as testing knobs for untried seats and harnessings,
-- overriding the end with no steady-state default.
--
-- Subcommands (storage/coffee-cards/fare.md; all live):
--
--   search PATTERN [PATH] [--head|--tail]   bounded recursive substring search
--   read FILE [RANGE] [--head|--tail]       file slices, never whole files
--   gate [ROOT] [--docspec|--docspec-full|--no-docspec]
--                                               distilled build/lint verdict
--   state [ROOT] [--head|--tail]            distilled git status/log/diff
--   index KEYWORD [ROOT] [--cutoff N]       decayed-area query over kimi wires
module Free.Agent.Fare.Cli
  ( SearchConfig (..),
    ReadConfig (..),
    GateDocspec (..),
    GateConfig (..),
    StateConfig (..),
    IndexConfig (..),
    FareCommand (..),
    fareParser,
    runFareCommand,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Fare.Config (End, endOpt, readGateDocspec, rootArg)
import Free.Agent.Fare.Gate (DocspecScope (..), runGate)
import Free.Agent.Fare.Index (runIndex)
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

-- | Invocation choice for the docspec lane. The default is resolved from
-- ROOT's .fare file; explicit choices always win.
data GateDocspec
  = RepositoryDefault
  | DocspecExcluded
  | DocspecSelected DocspecScope
  deriving (Eq, Show)

-- | gate configuration: distilled build/lint verdict over ROOT.
-- --head/--tail do not exist here: a verdict has no slice to flip.
data GateConfig = GateConfig
  { gaRoot :: FilePath,
    gaDocspec :: GateDocspec
  }
  deriving (Show)

-- | state configuration: distilled git status/log/diff over ROOT.
data StateConfig = StateConfig
  { stRoot :: FilePath,
    stEnd :: Maybe End
  }
  deriving (Show)

-- | index configuration: the decayed-area query over a wire store.
data IndexConfig = IndexConfig
  { inKeyword :: Text,
    inRoot :: FilePath,
    inCutoff :: Maybe Double
  }
  deriving (Show)

data FareCommand
  = SearchCmd SearchConfig
  | ReadCmd ReadConfig
  | GateCmd GateConfig
  | StateCmd StateConfig
  | IndexCmd IndexConfig
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
            <*> ( fromMaybe RepositoryDefault
                    <$> optional
                      ( flag'
                          DocspecExcluded
                          ( long "no-docspec"
                              <> help "Run only build, ormolu and hlint; the verdict names both excluded docspec lanes"
                          )
                          <|> flag'
                            (DocspecSelected DocspecRepository)
                            ( long "docspec"
                                <> help "Override .fare: add cabal-docspec for ROOT's package only (30s+; tests the last build; excludes linked local projects)"
                            )
                          <|> flag'
                            (DocspecSelected DocspecFull)
                            ( long "docspec-full"
                                <> help "Override .fare: add cabal-docspec for ROOT and all linked local projects (30s+; tests the last build)"
                            )
                      )
                )
        )

stateParser :: Parser FareCommand
stateParser =
  StateCmd
    <$> (StateConfig <$> rootArg <*> endOpt)

indexParser :: Parser FareCommand
indexParser =
  IndexCmd
    <$> ( IndexConfig
            <$> argument (T.pack <$> str) (metavar "KEYWORD" <> help "Keyword: one token, no whitespace; matched case-insensitively inside each token")
            <*> argument
              str
              ( metavar "ROOT"
                  <> value ""
                  <> help "Wire store to search (default: ~/.kimi-code/sessions)"
              )
            <*> optional
              ( option
                  auto
                  ( long "cutoff"
                      <> metavar "N"
                      <> help "Area cutoff: a run scores strictly above N (default 0.5)"
                  )
              )
        )

-- ---------------------------------------------------------------------------
-- Top-level parser
-- ---------------------------------------------------------------------------

fareParser :: Parser FareCommand
fareParser =
  subparser
    ( command "search" (info (searchParser <**> helper) (progDesc "Bounded search over files"))
        <> command "read" (info (readParser <**> helper) (progDesc "File slices, never whole files"))
        <> command
          "gate"
          ( info
              (gateParser <**> helper)
              ( fullDesc
                  <> progDesc "Run build, ormolu, hlint and the configured docspec lane. Default: ROOT/.fare gate.docspec = none|repository|full (none without the file); explicit flags override it. PASS/FAIL names the selected scope and exclusions."
              )
          )
        <> command "state" (info (stateParser <**> helper) (progDesc "Distilled git status/log/diff"))
        <> command
          "index"
          ( info
              (indexParser <**> helper)
              ( fullDesc
                  <> progDesc
                    "Decayed-area query over kimi wires: lowercase tokens, literal substring inside a token, runs scoring strictly above the cutoff. Evidence boundary: content.part text inside context.append_loop_event records only, zero-based token offsets; not a whole-event or whole-dialect search."
              )
          )
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
  GateCmd cfg -> do
    scope <- case gaDocspec cfg of
      RepositoryDefault -> readGateDocspec (gaRoot cfg)
      DocspecExcluded -> pure Nothing
      DocspecSelected selected -> pure (Just selected)
    runGate (gaRoot cfg) scope
  StateCmd cfg ->
    runState (stRoot cfg) (stEnd cfg)
  IndexCmd cfg ->
    runIndex (inKeyword cfg) (inRoot cfg) (inCutoff cfg)
