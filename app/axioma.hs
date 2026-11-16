-- | free-agent oracle tests — thin dispatcher over the 'Axioma' topics.
--
-- The accretion monolith is split into topic modules on the circuits canon
-- model: one command per topic plus @all@.  The topic modules are the
-- curation; oracle IDs (B1-B8, Q1-Q5, M1a, Q7, R2a/R2b, R3c) are kept
-- where wave journals reference them.
module Main (main) where

import Axioma.Acp (acpTopic)
import Axioma.Backend (backendTopic)
import Axioma.Bus (busTopic)
import Axioma.Delivery (deliveryTopic)
import Axioma.Halt (haltTopic)
import Axioma.Interrupt (interruptTopic)
import Axioma.Lifecycle (lifecycleTopic)
import Axioma.Meeting (meetingTopic)
import Axioma.Ports (portsTopic)
import Axioma.Quiesce (quiesceTopic)
import Axioma.Recovery (recoveryTopic)
import Axioma.Refresh (refreshTopic)
import Axioma.Resume (resumeTopic)
import Axioma.Seat (seatTopic)
import Axioma.Transcript (transcriptTopic)
import Axioma.Usage (usageTopic)
import Options.Applicative

data Topic
  = All
  | Seat
  | Transcript
  | Delivery
  | Meeting
  | Ports
  | Bus
  | Halt
  | Interrupt
  | Quiesce
  | Refresh
  | Recovery
  | Resume
  | Acp
  | Backend
  | Lifecycle
  | Usage
  deriving (Show, Eq, Bounded, Enum)

topicName :: Topic -> String
topicName All = "all"
topicName Seat = "seat"
topicName Transcript = "transcript"
topicName Delivery = "delivery"
topicName Meeting = "meeting"
topicName Ports = "ports"
topicName Bus = "bus"
topicName Halt = "halt"
topicName Interrupt = "interrupt"
topicName Quiesce = "quiesce"
topicName Refresh = "refresh"
topicName Recovery = "recovery"
topicName Resume = "resume"
topicName Acp = "acp"
topicName Backend = "backend"
topicName Lifecycle = "lifecycle"
topicName Usage = "usage"

topicDesc :: Topic -> String
topicDesc All = "run all topics"
topicDesc Seat = "host shard round-trips: echo/process/CLI host sketches, BodyLines split, session resume"
topicDesc Transcript = "transcript oracle (B1): JSONL records, scrub agreement, silent failure"
topicDesc Delivery = "delivery settlement: quiet response, exactly-once await, explicit root and reply posts"
topicDesc Meeting = "meeting skeletons, panel meeting, replay, derivations as 2-cells"
topicDesc Ports = "process framing, death-as-content, merged stdio, and honest doubles (S1-S6)"
topicDesc Bus = "file-log kernel, CLI projections, and runtime witnesses"
topicDesc Halt = "the halt form (Y1): a status label is content, a bare first line is control"
topicDesc Interrupt = "the interrupted handler (Z1): an interrupted turn is not retried"
topicDesc Quiesce = "tailLog draining + the quiescence Moore (Q1-Q5)"
topicDesc Refresh = "automatic same-name session refresh and durable handoff"
topicDesc Recovery = "B16 recovery classes: a resume failure is not a turn failure, and what a skip records"
topicDesc Resume = "CLI resume, retry, and transcript witnesses"
topicDesc Acp = "ACP seat (R2a): scripted double + wired-binary integration"
topicDesc Backend = "Backend recipes, independent Session legs, typed outcomes, cancellation, and local protocol peers"
topicDesc Lifecycle = "file-log lock stress and same-process handle exclusion"
topicDesc Usage = "Hermes latest-prompt usage anchor"

topicParser :: Parser Topic
topicParser =
  subparser $
    foldr
      (<>)
      (commandGroup "Topics:")
      [ command
          (topicName t)
          (info (pure t <**> helper) (progDesc (topicDesc t)))
      | t <- [minBound .. maxBound],
        t /= All
      ]
      <> command
        "all"
        (info (pure All <**> helper) (progDesc (topicDesc All)))

opts :: ParserInfo Topic
opts =
  info
    (topicParser <**> helper)
    ( fullDesc
        <> progDesc "Run free-agent oracles by topic"
        <> header "free-agent-axioma — topic-selectable axiom oracles"
    )

runTopic :: Topic -> IO ()
runTopic All = mapM_ runTopic (filter (/= All) [minBound .. maxBound])
runTopic Seat = seatTopic
runTopic Transcript = transcriptTopic
runTopic Delivery = deliveryTopic
runTopic Meeting = meetingTopic
runTopic Ports = portsTopic
runTopic Bus = busTopic
runTopic Halt = haltTopic
runTopic Interrupt = interruptTopic
runTopic Quiesce = quiesceTopic
runTopic Refresh = refreshTopic
runTopic Recovery = recoveryTopic
runTopic Resume = resumeTopic
runTopic Acp = acpTopic
runTopic Backend = backendTopic
runTopic Lifecycle = lifecycleTopic
runTopic Usage = usageTopic

main :: IO ()
main = do
  putStrLn "free-agent oracle tests"
  topic <- execParser opts
  runTopic topic
  putStrLn "All tests passed"
