{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Batch-committed agents and the pure meeting witness.
--
-- An 'Agent' receives a batch of posts through its commit channel and emits
-- a batch of replies independently. 'runAgent' is the one-shot interpreter;
-- 'Free.Agent.Bus' supplies the persistent file-log runner.
module Free.Agent.Agent
  ( Name,
    Agent,
    AgentChannel (..),
    Lifecycle (..),
    ioAgent,
    drainAgent,
    runAgent,
    PureAgent,
    tape,
    cellToAgent,
    loop,
  )
where

import Circuit.Category (K (..))
import Circuit.Machine (Body (Body), Moore)
import Circuit.Post
  ( Inbox,
    Log,
    Post,
    appendInbox,
    deliversTo,
    emptyInbox,
    inboxSubs,
    post,
    unconsInbox,
    watch,
  )
import Circuit.Stream (Cons, Snoc (..), These (..), Uncons (..))
import Circuit.Tensor (Const (..), unConst)
import Circuit.Traced (trace)
import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Text (Text)

-- | A seat or addressee name on the bus.
type Name = Text

-- | A batch-committed, independently-emitting agent.
type Agent s = Body (,) (,) (AgentChannel s) (AgentChannel s) (K IO) (Post Text Text) [Post Text Text]

-- | Client state and lifecycle in the same value.
data AgentChannel s = AgentChannel
  { agentState :: s,
    agentLifecycle :: Lifecycle
  }
  deriving (Show, Eq)

-- | Why an agent stopped.
data Lifecycle
  = Continue
  | Done
  deriving (Show, Eq)

-- | Drain ordered emissions through the first 'Done'. The final emission is
-- always performed, even when the incoming channel already reports 'Done'.
drainAgent :: Agent s -> AgentChannel s -> IO (AgentChannel s, [Post Text Text])
drainAgent (Body _ _ emit') = go
  where
    go channel = do
      (channel', posts) <- runK emit' channel
      case agentLifecycle channel' of
        Done -> pure (channel', posts)
        Continue -> fmap (posts <>) <$> go channel'

-- | Initialise from the first post, commit every later post independently,
-- then drain emissions through 'Done'. An empty input batch has no birth
-- event and therefore no output.
runAgent :: Agent s -> [Post Text Text] -> IO [Post Text Text]
runAgent _ [] = pure []
runAgent agent@(Body birth commit' _) (first : rest) = do
  born <- runK birth first
  committed <- foldM (\channel post_ -> runK commit' (channel, post_)) born rest
  snd <$> drainAgent agent committed

-- | A one-delivery agent from a batched IO action. The first emit returns
-- the action's posts beside 'Done'.
ioAgent :: ([Post Text Text] -> IO [Post Text Text]) -> Agent [Post Text Text]
ioAgent action =
  Body
    (K (\post_ -> pure (AgentChannel [post_] Continue)))
    (K (\(AgentChannel pending _, post_) -> pure (AgentChannel (pending <> [post_]) Continue)))
    ( K $ \(AgentChannel pending _) -> do
        posts <- action pending
        pure (AgentChannel [] Done, posts)
    )

-- | A pure monomial agent used by the meeting witness.
type PureAgent s a b = Body (,) (,) s s (->) a b

-- | A history agent: cons each input and observe a function of the history.
tape :: ([i] -> o) -> Moore (,) [i] (->) i o
tape f = Body (\i -> [i]) (\(hist, i) -> i : hist) (Const . f)

-- | Adapt a Moore cell into a pure agent over the same state.
cellToAgent :: Moore (,) s (->) a b -> PureAgent s a b
cellToAgent (Body j k e) = Body j k (\s -> (s, unConst (e s)))

-- Internal pure-meeting state. Only 'loop' exposes the scheduler; its result
-- is pattern-matched opaquely by the Q7 witness.
data AgentState s f = AgentState
  { asCarrier :: s,
    asInbox :: Inbox Name f
  }
  deriving (Show, Eq)

data Derivation a = Derivation
  { dAgent :: Name,
    dInput :: Post Name a,
    dOutputs :: [Post Name a],
    dChildren :: [Derivation a]
  }
  deriving (Show, Eq)

seedAgentState ::
  forall a s f.
  (Snoc s (Post Name a), Snoc f (Post Name a), Uncons f (Post Name a)) =>
  [Name] ->
  Log f ->
  AgentState s f
seedAgentState subs lg = AgentState (snocNil @s @(Post Name a)) (seedInbox @a @f subs lg)

seedInbox ::
  forall a f.
  (Snoc f (Post Name a), Uncons f (Post Name a)) =>
  [Name] ->
  Log f ->
  Inbox Name f
seedInbox subs lg = foldl' (flip appendInbox) (emptyInbox @Name @a @f subs) (watch @Name @a @f subs lg)

turnAs ::
  forall a s f.
  (Cons f (Post Name a), Uncons f (Post Name a)) =>
  Name ->
  PureAgent s (Post Name a) [Post Name a] ->
  AgentState s f ->
  Log f ->
  (AgentState s f, Log f, Maybe (Derivation a))
turnAs who sys st log0 =
  let subs = inboxSubs (asInbox st)
   in case unconsInbox @Name @a (asInbox st) of
        That _ -> (st, log0, Nothing)
        This p ->
          let (os, seen') = run1 sys (asCarrier st) p
           in (AgentState seen' (emptyInbox @Name @a subs), foldl' (flip post) log0 os, Just (Derivation who p os []))
        These p rest ->
          let (os, seen') = run1 sys (asCarrier st) p
           in (AgentState seen' rest, foldl' (flip post) log0 os, Just (Derivation who p os []))

hasPending :: forall a s f. (Uncons f (Post Name a)) => AgentState s f -> Bool
hasPending st = case unconsInbox @Name @a (asInbox st) of That _ -> False; _ -> True

streamLength :: forall f a. (Uncons f a) => f -> Int
streamLength = go 0
  where
    go :: Int -> f -> Int
    go n stream =
      case uncons @f @a stream of
        That _ -> n
        This _ -> n + 1
        These _ rest -> go (n + 1) rest

takeStream :: forall f a. (Uncons f a) => Int -> f -> [a]
takeStream n s = go n s []
  where
    go :: Int -> f -> [a] -> [a]
    go 0 _ acc = acc
    go n' stream acc =
      case uncons @f @a stream of
        That _ -> acc
        This x -> x : acc
        These x rest -> go (n' - 1) rest (x : acc)

type RosterEntry s a = (Name, [Name], PureAgent s (Post Name a) [Post Name a])

-- | Round-robin turn-loop until no agent has pending deliveries.
--
-- Roster order is the schedule. Each pass runs one turn for every agent that
-- still has pending work at its slot. This is the pure half of the Q7
-- witness against the file-log runner.
loop ::
  forall a s f.
  (Snoc s (Post Name a), Snoc f (Post Name a), Cons f (Post Name a), Uncons f (Post Name a)) =>
  [(Name, PureAgent s (Post Name a) [Post Name a])] ->
  Log f ->
  ([(Name, AgentState s f)], Log f, [Derivation a])
loop roster = loopSubs [(n, [n], a) | (n, a) <- roster]

loopSubs ::
  forall a s f.
  (Snoc s (Post Name a), Snoc f (Post Name a), Cons f (Post Name a), Uncons f (Post Name a)) =>
  [RosterEntry s a] ->
  Log f ->
  ([(Name, AgentState s f)], Log f, [Derivation a])
loopSubs roster log0 = loopWithSubs roster [(n, seedAgentState @a @s @f subs log0) | (n, subs, _) <- roster] log0

loopWithSubs ::
  forall a s f.
  (Snoc f (Post Name a), Cons f (Post Name a), Uncons f (Post Name a)) =>
  [RosterEntry s a] ->
  [(Name, AgentState s f)] ->
  Log f ->
  ([(Name, AgentState s f)], Log f, [Derivation a])
loopWithSubs roster states0 log0 = trace body ()
  where
    bundle0 = (states0, log0, []) :: ([(Name, AgentState s f)], Log f, [Derivation a])
    body (Right ()) =
      if any (hasPending @a . snd) states0
        then Left bundle0
        else Right bundle0
    body (Left bundle) =
      let bundle' = meetingPass roster bundle
       in if any (hasPending @a . snd) (fst3 bundle')
            then Left bundle'
            else Right bundle'
    fst3 (x, _, _) = x

meetingPass ::
  forall a s f.
  (Snoc f (Post Name a), Cons f (Post Name a), Uncons f (Post Name a)) =>
  [RosterEntry s a] ->
  ([(Name, AgentState s f)], Log f, [Derivation a]) ->
  ([(Name, AgentState s f)], Log f, [Derivation a])
meetingPass roster (states, lg, derivs) = foldl' meetingStep (states, lg, derivs) roster
  where
    subMap = Map.fromList [(n, subs) | (n, subs, _) <- roster]

    meetingStep (st, l, ds) (name, _subs, agent) =
      case lookup name st of
        Nothing -> (st, l, ds)
        Just sti
          | hasPending @a sti ->
              let (st', l', md) = turnAs name agent sti l
                  newCount = streamLength @f @(Post Name a) l' - streamLength @f @(Post Name a) l
                  newPosts = takeStream @f @(Post Name a) newCount l'
                  st'' = foldl' routePost (updateState name st' st) newPosts
                  ds' = maybe ds (\d -> ds ++ [d]) md
               in (st'', l', ds')
          | otherwise -> (st, l, ds)

    updateState name st' = map (\(n, s) -> if n == name then (n, st') else (n, s))

    routePost states' p =
      map
        ( \(n, st) ->
            if deliversTo p (Map.findWithDefault [n] n subMap)
              then (n, st {asInbox = appendInbox p (asInbox st)})
              else (n, st)
        )
        states'

run1 :: PureAgent s i o -> s -> i -> (o, s)
run1 (Body _ k e) s i =
  let (s', o) = e (k (s, i))
   in (o, s')
