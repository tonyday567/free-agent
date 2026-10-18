{-# LANGUAGE OverloadedStrings #-}

-- | fare CLI — free-agent restricted equipment, a parsimonious tool menu.
--
-- The distilled answer is the interface and deeper layers are one ask away
-- (cake/fare.md §design principles). Each item's layer-1 answer is bounded
-- by its own distillation, including which end the slice takes; --head and
-- --tail exist only as testing knobs for untried seats and harnessings,
-- overriding the end with no steady-state default.
--
-- Subcommands (cake/fare.md; search, read, gate, and state are live;
-- usage, pc, and bash are stubs):
--
--   search PATTERN [PATH] [--head|--tail]   bounded recursive substring search
--   read FILE [RANGE] [--head|--tail]       file slices, never whole files
--   gate [ROOT] [--docspec]                 distilled build/lint verdict
--   state [ROOT] [--head|--tail]            distilled git status/log/diff
--   usage [day|week] [--head|--tail]        fare's own meter
--   pc [--host HOST] CMD [ARG...]           the PC lane (environment held)
--   bash CMD [ARG...]                       the free-form lane (innovation)
--
-- The pc lane holds the non-interactive-ssh environment so callers do not
-- re-pay the export-PATH wrapper class on every call. Pass dash-leading
-- flags after @--@ for pc and bash (optparse does not forward them).
module Free.Agent.Fare.Cli
  ( SearchConfig (..),
    ReadConfig (..),
    GateConfig (..),
    StateConfig (..),
    UsageConfig (..),
    PcConfig (..),
    BashConfig (..),
    FareCommand (..),
    fareParser,
    runFareCommand,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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

-- | usage configuration: the meter, over a day or a week of wire.
data UsageConfig = UsageConfig
  { usPeriod :: Text,
    usEnd :: Maybe End
  }
  deriving (Show)

-- | pc configuration: run CMD on the PC with the environment held.
data PcConfig = PcConfig
  { pcHost :: Maybe Text,
    pcCmd :: Text,
    pcArgs :: [Text],
    pcEnd :: Maybe End
  }
  deriving (Show)

-- | bash configuration: the free-form lane. Metered usage here is the
-- menu's innovation signal — new practices enter through this lane and
-- solidify into dedicated menu items.
data BashConfig = BashConfig
  { baCmd :: Text,
    baArgs :: [Text],
    baEnd :: Maybe End
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Command sum
-- ---------------------------------------------------------------------------

data FareCommand
  = SearchCmd SearchConfig
  | ReadCmd ReadConfig
  | GateCmd GateConfig
  | StateCmd StateConfig
  | UsageCmd UsageConfig
  | PcCmd PcConfig
  | BashCmd BashConfig
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

usageParser :: Parser FareCommand
usageParser =
  UsageCmd
    <$> ( UsageConfig
            <$> argument
              (eitherReader parsePeriod)
              ( metavar "PERIOD"
                  <> value "week"
                  <> showDefault
                  <> help "day | week"
              )
            <*> endOpt
        )
  where
    parsePeriod "day" = Right "day"
    parsePeriod "week" = Right "week"
    parsePeriod other = Left ("expected day | week, got: " ++ other)

pcParser :: Parser FareCommand
pcParser =
  PcCmd
    <$> ( PcConfig
            <$> optional
              ( option
                  (T.pack <$> str)
                  ( long "host"
                      <> metavar "HOST"
                      <> help "PC ssh target (default: the lane's held environment)"
                  )
              )
            <*> fmap T.pack (argument str (metavar "CMD" <> help "Command to run on the PC"))
            <*> many (fmap T.pack (argument str (metavar "ARG..." <> help "Arguments (dash-leading flags after --)")))
            <*> endOpt
        )

bashParser :: Parser FareCommand
bashParser =
  BashCmd
    <$> ( BashConfig
            <$> fmap T.pack (argument str (metavar "CMD" <> help "Command to run"))
            <*> many (fmap T.pack (argument str (metavar "ARG..." <> help "Arguments (dash-leading flags after --)")))
            <*> endOpt
        )

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
        <> command "usage" (info (usageParser <**> helper) (progDesc "fare's own meter"))
        <> command "pc" (info (pcParser <**> helper) (progDesc "Run a command on the PC (environment held)"))
        <> command "bash" (info (bashParser <**> helper) (progDesc "Free-form lane (practice innovation)"))
    )

-- ---------------------------------------------------------------------------
-- Stub dispatch (wave 2: parse for real, print the distilled-shaped stub)
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
  UsageCmd cfg ->
    stubLine "usage" (usPeriod cfg) [] (usEnd cfg)
  PcCmd cfg ->
    stubLine "pc" (pcCmd cfg) [T.intercalate " " (pcArgs cfg)] (pcEnd cfg)
  BashCmd cfg ->
    stubLine "bash" (baCmd cfg) [T.intercalate " " (baArgs cfg)] (baEnd cfg)

stubLine :: Text -> Text -> [Text] -> Maybe End -> IO ()
stubLine name target fields end =
  TIO.putStrLn $
    "STUB fare "
      <> name
      <> " "
      <> target
      <> ( case filter (not . T.null) fields of
             [] -> ""
             fs -> " " <> T.intercalate " " fs
         )
      <> " end="
      <> maybe "default" (T.pack . show) end
