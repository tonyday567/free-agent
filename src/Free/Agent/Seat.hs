{-# LANGUAGE GADTs #-}

-- | Free seat syntax: pipelines and hosts as inspectable terms that fold into
-- 'Circuit.Agent.Shard' values in 'StateT [Post] IO'.
module Free.Agent.Seat
  ( FreeSeat (..),
    interpretSeat,
    pipelineSeat,
    hostSeat,
  )
where

import Circuit (composeEnds)
import Circuit.Agent (Post, Shard)
import Control.Monad.State (StateT)
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
--   * 'SeatHost' — a one-shot host (may perform IO).
--   * 'SeatCompose' — sequential composition of seats.
data FreeSeat where
  SeatPipeline :: Pipeline Post Post -> FreeSeat
  SeatHost :: Host (StateT [Post] IO) -> FreeSeat
  SeatCompose :: FreeSeat -> FreeSeat -> FreeSeat

-- | Lift a pure pipeline into a free seat term.
pipelineSeat :: Pipeline Post Post -> FreeSeat
pipelineSeat = SeatPipeline

-- | Lift a host into a free seat term.
hostSeat :: Host (StateT [Post] IO) -> FreeSeat
hostSeat = SeatHost

-- | Fold a free seat term into a shard by interpreting each generator and
-- composing the resulting shards with 'composeEnds'.
--
-- The result lives in 'StateT [Post] IO' so that real process hosts can share
-- the same buffer monad as pure pipeline stages.
interpretSeat ::
  FreeSeat ->
  Shard (StateT [Post] IO) [Post] [Post]
interpretSeat (SeatPipeline p) = pipelineShard p
interpretSeat (SeatHost h) = hostShard h
interpretSeat (SeatCompose g f) =
  composeEnds (interpretSeat f) (interpretSeat g)
