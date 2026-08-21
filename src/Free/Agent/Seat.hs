{-# LANGUAGE GADTs #-}

-- | Free seat syntax: pipelines and hosts as inspectable terms that fold into
-- 'Circuit.Agent.Tensor.AgentShard' values, with STM agents via
-- 'interpretSeatS'.
module Free.Agent.Seat
  ( FreeSeat (..),
    SeatBehaviour,
    interpretSeat,
    interpretSeatA,
    interpretSeatS,
    runAgentSBox,
    pipelineSeat,
    hostSeat,
    silentSeat,
    forkSeat,
    awaitSeat,
    raceSeat,
    fanOutSeat,
    fanInSeat,
    bundleSeat,
  )
where

import Circuit (Body (..))
import Circuit.Agent
  ( Agent,
    Post,
    runAgentM,
  )
import Circuit.Agent.Tensor
  ( AgentShard,
    awaitShard,
    fanInShard,
    fanOutShard,
    raceShard,
    silentShard,
  )
import Circuit.Ends (composeEnds0)
import Circuit.Poly (System, monoDir, system)
import Control.Arrow (Kleisli (..))
import Control.Concurrent.STM (STM, atomically)
import Data.Text (Text)
import Free.Agent.Host (Host, hostShard)
import Free.Agent.Pipeline (Pipeline, pipelineShard, runPipeline)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Free.Agent.Seat
-- >>> import Free.Agent.Pipeline
-- >>> import Free.Agent.Host
-- >>> import Circuit.Agent

-- | An inspectable '[Post Text]'-to-'[Post Text]' seat term.
--
--   * 'SeatPipeline' — a pure list-to-list pipeline over 'Post'.
--   * 'SeatHost' — a one-shot host (may perform IO).
--   * 'SeatSilent' — emits nothing and clears the buffer.
--   * 'SeatCompose' — sequential composition of seats.
--   * 'SeatFork' — syntactic marker that the input state is duplicated.
--   * 'SeatAwait' — product tensor: both branches speak on the same input.
--   * 'SeatRace' — coproduct tensor: left-biased race of branch emits.
--   * 'SeatFanOut' — run every branch on a copy of the input, concatenate.
--   * 'SeatFanIn' — fan-out then collapse the branch outputs with a summary.
data FreeSeat where
  -- | A pure pipeline stage.
  SeatPipeline :: Pipeline (Post Text) (Post Text) -> FreeSeat
  -- | A one-shot host stage (may perform IO).
  SeatHost :: Host -> FreeSeat
  -- | The silent seat: emits the empty list.
  SeatSilent :: FreeSeat
  -- | Sequential composition of seats.
  SeatCompose :: FreeSeat -> FreeSeat -> FreeSeat
  -- | Fork the current input session (semantically identity at this stage).
  SeatFork :: FreeSeat -> FreeSeat
  -- | Product / await: concatenate the emits of both branches.
  SeatAwait :: FreeSeat -> FreeSeat -> FreeSeat
  -- | Coproduct / race: left emit wins if non-empty, otherwise right.
  SeatRace :: FreeSeat -> FreeSeat -> FreeSeat
  -- | Fan-out: run every branch on a copy of the input.
  SeatFanOut :: [FreeSeat] -> FreeSeat
  -- | Fan-in: fan-out then summarize the collected branch outputs.
  SeatFanIn :: ([[Post Text]] -> [Post Text]) -> [FreeSeat] -> FreeSeat

-- | Lift a pure pipeline into a free seat term.
pipelineSeat :: Pipeline (Post Text) (Post Text) -> FreeSeat
pipelineSeat = SeatPipeline

-- | Lift a host into a free seat term.
hostSeat :: Host -> FreeSeat
hostSeat = SeatHost

-- | The silent seat: emits nothing.
silentSeat :: FreeSeat
silentSeat = SeatSilent

-- | Mark a seat as running on a forked copy of the input session.
forkSeat :: FreeSeat -> FreeSeat
forkSeat = SeatFork

-- | Product / await of two seats.
awaitSeat :: FreeSeat -> FreeSeat -> FreeSeat
awaitSeat = SeatAwait

-- | Coproduct / race of two seats (left-biased).
raceSeat :: FreeSeat -> FreeSeat -> FreeSeat
raceSeat = SeatRace

-- | Fan-out: run every seat on a copy of the input.
fanOutSeat :: [FreeSeat] -> FreeSeat
fanOutSeat = SeatFanOut

-- | Fan-in: fan-out then summarize the branch outputs.
fanInSeat :: ([[Post Text]] -> [Post Text]) -> [FreeSeat] -> FreeSeat
fanInSeat = SeatFanIn

-- | 'fanInSeat' synonym: a bundle is fan-out, map, fan-in.
bundleSeat :: ([[Post Text]] -> [Post Text]) -> [FreeSeat] -> FreeSeat
bundleSeat = fanInSeat

-- | Fold a free seat term into an agent shard by interpreting each generator
-- and composing the resulting shards with 'composeEnds0'.
--
-- The buffer state is carried by the base arrow ('Body (,) (Kleisli IO)
-- [Post Text]') rather than a monad transformer.
interpretSeat ::
  FreeSeat ->
  AgentShard [Post Text] [Post Text]
interpretSeat (SeatPipeline p) = pipelineShard p
interpretSeat (SeatHost h) = hostShard h
interpretSeat SeatSilent = silentShard
interpretSeat (SeatCompose g f) =
  composeEnds0 (interpretSeat f) (interpretSeat g)
interpretSeat (SeatFork f) = interpretSeat f
interpretSeat (SeatAwait f g) =
  awaitShard (interpretSeat f) (interpretSeat g)
interpretSeat (SeatRace f g) =
  raceShard (interpretSeat f) (interpretSeat g)
interpretSeat (SeatFanOut fs) =
  fanOutShard (map interpretSeat fs)
interpretSeat (SeatFanIn summary fs) =
  fanInShard summary (map interpretSeat fs)

-- | Pure behaviour semantics of a free seat: a function from committed posts
-- to emitted posts.
--
-- This is the specification-domain interpretation. Pipelines are list
-- transducers; 'SeatAwait'/'SeatRace'/'SeatFanOut'/'SeatFanIn' are the bundle
-- tensors; 'SeatFork' is identity. 'SeatHost' is not pure and raises a runtime
-- error.
type SeatBehaviour = [Post Text] -> [Post Text]

-- | Fold a free seat term into its pure behaviour.
--
-- The result is independent of any effectful boundary; it is the function that
-- the shard interpretation realises.
interpretSeatA :: FreeSeat -> SeatBehaviour
interpretSeatA (SeatPipeline p) = runPipeline p
interpretSeatA SeatSilent = const []
interpretSeatA (SeatHost _) = error "interpretSeatA: SeatHost cannot be interpreted as a pure behaviour"
interpretSeatA (SeatCompose g f) = interpretSeatA g . interpretSeatA f
interpretSeatA (SeatFork f) = interpretSeatA f
interpretSeatA (SeatAwait f g) = \xs -> interpretSeatA f xs ++ interpretSeatA g xs
interpretSeatA (SeatRace f g) =
  \xs ->
    let o1 = interpretSeatA f xs
     in if null o1 then interpretSeatA g xs else o1
interpretSeatA (SeatFanOut fs) = \xs -> concatMap (\f -> interpretSeatA f xs) fs
interpretSeatA (SeatFanIn summary fs) =
  \xs -> summary (map (\f -> interpretSeatA f xs) fs)

-- | Existential box around an STM agent, carrying its initial state.
--
-- The agent consumes one /batch/ of posts per step, so the whole free-seat
-- semantics @[Post Text] -> [Post Text]@ is realised in a single STM step.
data AgentSBox where
  AgentSBox :: s -> Agent (Kleisli STM) s [Post Text] [Post Text] -> AgentSBox

-- | Run an STM-agent box over a list of posts, returning only the emitted
-- posts. The state is sealed inside the box; the caller sees the same
-- list-to-list interface as 'interpretSeatA'.
runAgentSBox :: AgentSBox -> [Post Text] -> IO [Post Text]
runAgentSBox (AgentSBox s0 ag) ins =
  fst <$> atomically (runAgentM ag s0 ins)

-- | Heterogeneous list element for STM agents with varying state types.
data HAgentS where
  HAgentS :: s -> Agent (Kleisli STM) s [Post Text] [Post Text] -> HAgentS

-- | Lift a pure pipeline into a batch STM agent.
pipelineS :: Pipeline (Post Text) (Post Text) -> Agent (Kleisli STM) () [Post Text] [Post Text]
pipelineS p = system $ Kleisli $ \((), d) ->
  pure ((), (runPipeline p (monoDir d), ()))

-- | Silent STM agent: emits nothing, state unchanged.
silentS :: Agent (Kleisli STM) () [Post Text] [Post Text]
silentS = system $ Kleisli $ \((), _) -> pure ((), ([], ()))

-- | Sequential composition in STM: outputs of @f@ on the whole batch are fed
-- as the next batch to @g@.
composeS ::
  Agent (Kleisli STM) sg [Post Text] [Post Text] ->
  Agent (Kleisli STM) sf [Post Text] [Post Text] ->
  Agent (Kleisli STM) (sf, sg) [Post Text] [Post Text]
composeS g f = system $ Kleisli $ \((sf, sg), d) -> do
  (outs, sf') <- runAgentM f sf (monoDir d)
  (outs', sg') <- runAgentM g sg outs
  pure ((sf', sg'), (outs', ()))

-- | Product / await in STM: both agents run on the same batch; emits are
-- concatenated.
awaitBatchS ::
  Agent (Kleisli STM) s1 [Post Text] [Post Text] ->
  Agent (Kleisli STM) s2 [Post Text] [Post Text] ->
  Agent (Kleisli STM) (s1, s2) [Post Text] [Post Text]
awaitBatchS f g = system $ Kleisli $ \((sf, sg), d) -> do
  (outsF, sf') <- runAgentM f sf (monoDir d)
  (outsG, sg') <- runAgentM g sg (monoDir d)
  pure ((sf', sg'), (outsF <> outsG, ()))

-- | Coproduct / race in STM: left emit wins if non-empty, otherwise right.
raceBatchS ::
  Agent (Kleisli STM) s1 [Post Text] [Post Text] ->
  Agent (Kleisli STM) s2 [Post Text] [Post Text] ->
  Agent (Kleisli STM) (s1, s2) [Post Text] [Post Text]
raceBatchS f g = system $ Kleisli $ \((sf, sg), d) -> do
  (outsF, sf') <- runAgentM f sf (monoDir d)
  (outsG, sg') <- runAgentM g sg (monoDir d)
  let outs = if null outsF then outsG else outsF
  pure ((sf', sg'), (outs, ()))

-- | Fan-out in STM: run every branch on the same input batch and concatenate
-- the outputs. Branch states are kept in a heterogeneous list.
fanOutS :: Agent (Kleisli STM) [HAgentS] [Post Text] [Post Text]
fanOutS = system $ Kleisli $ \(hs, d) -> do
  let batch = monoDir d
  pairs <- mapM (\(HAgentS s a) -> do (os, s') <- runAgentM a s batch; pure (HAgentS s' a, os)) hs
  let hs' = map fst pairs
  pure (hs', (concatMap snd pairs, ()))

-- | Fan-in in STM: run every branch on the same input batch, then collapse
-- the branch outputs with the summary function.
fanInS :: ([[Post Text]] -> [Post Text]) -> Agent (Kleisli STM) [HAgentS] [Post Text] [Post Text]
fanInS summary = system $ Kleisli $ \(hs, d) -> do
  let batch = monoDir d
  pairs <- mapM (\(HAgentS s a) -> do (os, s') <- runAgentM a s batch; pure (HAgentS s' a, os)) hs
  let hs' = map fst pairs
  pure (hs', (summary (map snd pairs), ()))

-- | Unbox an 'AgentSBox' into a heterogeneous STM agent.
unboxH :: AgentSBox -> HAgentS
unboxH (AgentSBox s a) = HAgentS s a

-- | Fold a free seat term into an STM agent.
--
-- Hosts are not STM-pure and raise a runtime error. All other constructors are
-- interpreted as one-post-at-a-time STM agents whose state is hidden inside the
-- returned box.
interpretSeatS :: FreeSeat -> AgentSBox
interpretSeatS (SeatPipeline p) = AgentSBox () (pipelineS p)
interpretSeatS (SeatHost _) = error "interpretSeatS: SeatHost cannot be interpreted in STM"
interpretSeatS SeatSilent = AgentSBox () silentS
interpretSeatS (SeatCompose g f) =
  case (interpretSeatS g, interpretSeatS f) of
    (AgentSBox sg0 gag, AgentSBox sf0 fag) -> AgentSBox (sf0, sg0) (composeS gag fag)
interpretSeatS (SeatFork f) = interpretSeatS f
interpretSeatS (SeatAwait f g) =
  case (interpretSeatS f, interpretSeatS g) of
    (AgentSBox sf0 fag, AgentSBox sg0 gag) -> AgentSBox (sf0, sg0) (awaitBatchS fag gag)
interpretSeatS (SeatRace f g) =
  case (interpretSeatS f, interpretSeatS g) of
    (AgentSBox sf0 fag, AgentSBox sg0 gag) -> AgentSBox (sf0, sg0) (raceBatchS fag gag)
interpretSeatS (SeatFanOut fs) =
  let hs = map (unboxH . interpretSeatS) fs
   in AgentSBox hs fanOutS
interpretSeatS (SeatFanIn summary fs) =
  let hs = map (unboxH . interpretSeatS) fs
   in AgentSBox hs (fanInS summary)
