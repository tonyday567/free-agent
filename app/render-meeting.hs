{-# LANGUAGE OverloadedStrings #-}

-- | The static visible artifact (stage L2d): a real panel meeting — one
-- human seed, three agents over two rounds with crossing wires, one
-- synthesis — read back as a drawing by 'meetingSkeleton' and committed as
-- an SVG.
module Main (main) where

import Chart (writeChartOptions)
import Circuit.Agent (Post, mkPost, replyTo, synthesis)
import Data.Text (Text)
import Free.Agent.Diagram (meetingSkeleton, skeletonLabels)
import Strings.Svg.Render (renderSDiagram)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Prelude

-- | One seed, three agents each replying to it (the seed forks three
-- ways), a second round where each agent replies to another agent's
-- round-1 post (the wires cross), and one synthesis merging the three
-- round-2 posts.
panelMeeting :: [Post Text]
panelMeeting = [seed, a1, b1, c1, a2, b2, c2, synth]
  where
    seed = mkPost "human" ["agent-1", "agent-2", "agent-3"] "Q: what should we do?"
    a1 = replyTo "agent-1" seed "agent-1 notes: factor A"
    b1 = replyTo "agent-2" seed "agent-2 notes: factor B"
    c1 = replyTo "agent-3" seed "agent-3 notes: factor C"
    a2 = replyTo "agent-1" b1 "agent-1 extends agent-2"
    b2 = replyTo "agent-2" c1 "agent-2 extends agent-3"
    c2 = replyTo "agent-3" a1 "agent-3 qualifies agent-1"
    synth = synthesis "synth" ["human"] [a2, b2, c2] "synthesis of round 2"

main :: IO ()
main = do
  let skel = meetingSkeleton panelMeeting
      dir = "other"
      fp = dir </> "panel-meeting.svg"
  createDirectoryIfMissing True dir
  writeChartOptions fp (renderSDiagram skel)
  mapM_ putStrLn (skeletonLabels skel)
  putStrLn ("wrote " <> fp)
