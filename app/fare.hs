-- | fare — free-agent restricted equipment: a parsimonious tool menu.
--
--   fare search PATTERN [PATH] [--head|--tail]
--   fare read FILE [RANGE] [--head|--tail]
--   fare gate [ROOT] [--docspec|--docspec-full|--no-docspec]
--   fare state [ROOT] [--head|--tail]
--   fare index KEYWORD [ROOT] [--cutoff N]
--   fare usage [day|week] [--head|--tail]
--   fare pc [--host HOST] CMD [ARG...] [--head|--tail]
--   fare bash CMD [ARG...] [--head|--tail]
--   fare --version
module Main (main) where

import Data.Version (showVersion)
import Free.Agent.Fare.Cli (fareParser, runFareCommand)
import Options.Applicative
  ( Parser,
    customExecParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
    long,
    prefs,
    progDesc,
    short,
    showHelpOnError,
    (<**>),
  )
import Paths_free_agent (version)

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnError) opts
  runFareCommand cmd
  where
    opts =
      info
        ((fareParser <**> helper) <**> versionOption)
        ( fullDesc
            <> progDesc "Restricted, curated tool menu for agent seats"
            <> header "fare - free-agent restricted equipment"
        )

-- | The build witness: which fare produced a verdict. Answers with the
-- package version the binary was linked from and exits 0.
versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("fare " <> showVersion version)
    ( long "version"
        <> short 'V'
        <> help "Print the build version and exit"
    )
