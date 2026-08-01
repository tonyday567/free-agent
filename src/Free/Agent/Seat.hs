{-# LANGUAGE GADTs #-}

-- | Free seat syntax: pipelines and hosts as inspectable terms that fold into
-- 'Circuit.Agent.Shard' values in 'State [Post]'.
module Free.Agent.Seat
  ( FreeSeat (..),
    interpretSeat,
    pipelineSeat,
    hostSeat,
  )
where

import Circuit (composeEnds)
import Circuit.Agent (Post, Shard)
import Control.Monad.State (State)
import Free.Agent.Host (Host, hostShard)
import Free.Agent.Pipeline (Pipeline, pipelineShard)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Free.Agent.Seat
-- >>> import Free.Agent.Pipeline
-- >>> import Free.Agent.Host
-- >>> import Circuit.Agent

-- | An inspectable '[Post]'-to-'[Post]' seat term.
--
--   * 'SeatPipeline' — a pure list-to-list pipeline over 'Post'.
--   * 'SeatHost' — a one-shot host.
--   * 'SeatCompose' — sequential composition of seats.
data FreeSeat where
  SeatPipeline :: Pipeline Post Post -> FreeSeat
  SeatHost :: Host (State [Post]) -> FreeSeat
  SeatCompose :: FreeSeat -> FreeSeat -> FreeSeat

-- | Lift a pure pipeline into a free seat term.
pipelineSeat :: Pipeline Post Post -> FreeSeat
pipelineSeat = SeatPipeline

-- | Lift a host into a free seat term.
hostSeat :: Host (State [Post]) -> FreeSeat
hostSeat = SeatHost

-- | Fold a free seat term into a shard by interpreting each generator and
-- composing the resulting shards with 'composeEnds'.
interpretSeat ::
  FreeSeat ->
  Shard (State [Post]) [Post] [Post]
interpretSeat (SeatPipeline p) = pipelineShard p
interpretSeat (SeatHost h) = hostShard h
interpretSeat (SeatCompose g f) =
  composeEnds (interpretSeat f) (interpretSeat g)
