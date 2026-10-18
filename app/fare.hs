-- | fare — free-agent restricted equipment: a parsimonious tool menu.
--
--   fare search PATTERN [PATH] [--head|--tail]
--   fare read FILE [RANGE] [--head|--tail]
--   fare gate [ROOT] [--docspec]
--   fare state [ROOT] [--head|--tail]
--   fare usage [day|week] [--head|--tail]
--   fare pc [--host HOST] CMD [ARG...] [--head|--tail]
--   fare bash CMD [ARG...] [--head|--tail]
module Main (main) where

import Free.Agent.Fare.Cli (fareParser, runFareCommand)
import Options.Applicative
  ( execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
  )

main :: IO ()
main = do
  cmd <- execParser opts
  runFareCommand cmd
  where
    opts =
      info
        (fareParser <**> helper)
        ( fullDesc
            <> progDesc "Restricted, curated tool menu for agent seats"
            <> header "fare - free-agent restricted equipment"
        )
