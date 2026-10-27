-- | free-agent oracle tests — thin dispatcher over the 'Axioma' topics.
--
-- The accretion monolith is split into topic modules on the circuits canon
-- model: one command per topic plus @all@.  The topic modules are the
-- curation; oracle IDs (B1-B8, Q1-Q5, M1a, Q7, R2a/R2b, R3c) are kept
-- where wave journals reference them.
module Main (main) where

import Axioma.Acp (acpTopic)
import Axioma.Bus (busTopic)
import Axioma.Connector (connectorTopic)
import Axioma.Delivery (deliveryTopic)
import Axioma.Diagram (diagramTopic)
import Axioma.Lifecycle (lifecycleTopic)
import Axioma.Meeting (meetingTopic)
import Axioma.Persist (persistTopic)
import Axioma.Quiesce (quiesceTopic)
import Axioma.Resume (resumeTopic)
import Axioma.Seat (seatTopic)
import Axioma.Tensor (tensorTopic)
import Axioma.Transcript (transcriptTopic)
import Options.Applicative

data Topic
  = All
  | Seat
  | Transcript
  | Delivery
  | Diagram
  | Meeting
  | Bus
  | Quiesce
  | Persist
  | Resume
  | Tensor
  | Connector
  | Acp
  | Lifecycle
  deriving (Show, Eq, Bounded, Enum)

topicName :: Topic -> String
topicName All = "all"
topicName Seat = "seat"
topicName Transcript = "transcript"
topicName Delivery = "delivery"
topicName Diagram = "diagram"
topicName Meeting = "meeting"
topicName Bus = "bus"
topicName Quiesce = "quiesce"
topicName Persist = "persist"
topicName Resume = "resume"
topicName Tensor = "tensor"
topicName Connector = "connector"
topicName Acp = "acp"
topicName Lifecycle = "lifecycle"

topicDesc :: Topic -> String
topicDesc All = "run all topics"
topicDesc Seat = "host shard round-trips: echo/process/CLI host sketches, BodyLines split, session resume"
topicDesc Transcript = "transcript oracle (B1): JSONL records, scrub agreement, silent failure"
topicDesc Delivery = "delivery settlement: quiet response, exactly-once await, explicit root and reply posts"
topicDesc Diagram = "diagram bridge + bend with delay: machine bodies as string diagrams"
topicDesc Meeting = "meeting skeletons, panel meeting, replay, derivations as 2-cells"
topicDesc Bus = "bus stats + seats: supervision, self-halt, card meetings, M1a ancestry, the seam (Q7)"
topicDesc Quiesce = "tailLog draining + the quiescence Moore (Q1-Q5)"
topicDesc Persist = "watermark persister (B1-B3), anti-pollution (B4), stamping daemon"
topicDesc Resume = "empty-reply drop (B2), empty-sentinel drop (B8), resume fix (B6)"
topicDesc Tensor = "tensor/par probe: the stamping office as a connective toggle"
topicDesc Connector = "connector turn grammar (B7): zero/one/two-frame turns"
topicDesc Acp = "ACP seat (R2a): scripted double + wired-binary integration"
topicDesc Lifecycle = "lifecycle hygiene (H): lock stress, daemon liveness, death honesty (L1-L5)"

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
runTopic Diagram = diagramTopic
runTopic Meeting = meetingTopic
runTopic Bus = busTopic
runTopic Quiesce = quiesceTopic
runTopic Persist = persistTopic
runTopic Resume = resumeTopic
runTopic Tensor = tensorTopic
runTopic Connector = connectorTopic
runTopic Acp = acpTopic
runTopic Lifecycle = lifecycleTopic

main :: IO ()
main = do
  putStrLn "free-agent oracle tests"
  topic <- execParser opts
  runTopic topic
  putStrLn "All tests passed"
