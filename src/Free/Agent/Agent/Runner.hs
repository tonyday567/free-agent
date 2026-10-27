{-# LANGUAGE OverloadedStrings #-}

-- | Generic agent seat loop for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), calls the supplied
-- handler for each post, and writes the returned replies back through the
-- in-process bus with the cursor advanced. This is the common runtime shared
-- by the Hermes, Kimi, direct-API, and external-command seats.
module Free.Agent.Agent.Runner
  ( runAgentLoop,
  )
where

import Circuit.Agent (Name, Post (..), mkPost, sortNub)
import Circuit.Agent.Framing (Stamped, stamp, stamped)
import Circuit.Agent.Mark (Mark (..), isEscalate, isHalt, markGlyph, markOf)
import Control.Exception (SomeException, displayException, try)
import Data.Foldable (forM_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File
  ( Decision (..),
    Flow (..),
    QuiesceConfig (..),
    cursorPath,
    readCursor,
    tailLog,
    writeCursor,
  )
import Free.Agent.Bus.Heartbeat (HbState (..), writeHeartbeat)
import Free.Agent.Delivery (DeliveryGrant, Turn, TurnResult (..), awaitTurn, useDeliveryGrant)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.Process (getProcessID)

-- | Start a seat loop.
--
-- The caller supplies the subscription names, the bus root, an optional
-- quiescence config, a finite delivery grant, and a handler that turns one
-- incoming 'Stamped Text' into a live turn of explicit post acts. The handler
-- is responsible for decoration, scrubbing, and response interpretation.
--
-- Ancestry: every post produced in response to a delivery cites that
-- delivery — the delivered post's stamped id is prepended to each reply's
-- 'thread' (the same law 'runSeatBus' enforces), and the 🔴 relays do.
--
-- Decided quiet: a delivered post carrying a halt mark (🟢 or 🔵) stops the
-- loop; an escalation mark (🔴) is relayed to the pitboss (when a quiescence
-- config names one) and also stops the loop. The quiescence counter is the
-- observed-quiet bridge: after N empty cycles the seat posts the verdict
-- body from 'quiesceAgent' — a ⌛ timeout mark, an inference from wall-clock
-- silence, not a decided quiet — to the pitboss and exits.
--
-- Self-halt: a 🔵 reply is the seat deciding its own quiet — the loop stops
-- after scribing it. The F2 self-post skip means the seat never reads its
-- own mark back, so the halt is judged here, at commit time. 🟢 stays
-- exchange-level: a seat may land one exchange and host more. The ⌛ timeout
-- verdict never self-halts — the quiescence pump has already exited — and
-- as a non-halt mark it stops no other seat.
runAgentLoop ::
  -- | Agent name used for cursor and logging.
  Text ->
  -- | Subscription names.
  [Name] ->
  -- | Bus root.
  FilePath ->
  -- | Optional quiescence config.
  Maybe QuiesceConfig ->
  -- | Finite authority to invoke this seat.
  DeliveryGrant ->
  -- | Handler: incoming post -> turn of explicit post acts.
  (Stamped Text -> IO (Turn [Post Text])) ->
  IO ()
runAgentLoop agentName names root mQuiesce grant handlePost = do
  hSetBuffering stdout LineBuffering
  pid <- getProcessID
  TIO.putStrLn $ "🟢 free-agent seat starting: " <> T.intercalate "," names <> " (pid=" <> T.pack (show pid) <> ")"
  TIO.putStrLn $ "   root: " <> T.pack root
  let path = root </> "log.jsonl"
      onQuiesce d = do
        let qc = fromMaybe (error "quiesce action without config") mQuiesce
        _ <- postLocal root (mkPost agentName [qcPitboss qc] (decisionBody d))
        pure ()
      -- A mark is control, not content: halt and escalation marks are not
      -- handed to the seat handler. Silence follows the mark.
      controlFlow stored =
        case markOf (stamped stored) of
          Just m
            | isHalt m -> pure (Just Halt)
            | isEscalate m -> do
                let relay =
                      (mkPost agentName [] (markGlyph Escalate <> " escalation received; standing down"))
                        { thread = [snd (stamp stored)],
                          to = maybe [] (pure . qcPitboss) mQuiesce
                        }
                forM_ mQuiesce $ \_ -> postLocal root relay
                pure (Just Halt)
          _ -> pure Nothing
  cursor <- readCursor root agentName
  TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
  -- Heartbeats: wake on start, working around each delivered post, idle
  -- after it, dead on clean exit. The file is the seat's only liveness
  -- claim — `bus status --live` derives crash from staleness, never from
  -- a claim, so the seat writes states and nothing else.
  writeHeartbeat root agentName Wake
  tailLog path names cursor (fmap (\qc -> (qc, onQuiesce)) mQuiesce) $ \stored -> do
    writeHeartbeat root agentName Working
    result <-
      controlFlow stored >>= \case
        Just flow -> do
          writeCursor root agentName (snd (stamp stored) + 1)
          pure flow
        Nothing -> do
          -- F2: skip self-posts — an agent should never receive its own posts
          if from (stamped stored) == agentName
            then do
              writeCursor root agentName (snd (stamp stored) + 1)
              pure Continue
            else do
              granted <- useDeliveryGrant grant
              case granted of
                Left refusal -> do
                  hPutStrLn stderr $
                    "delivery refused for "
                      ++ T.unpack agentName
                      ++ " on post "
                      ++ show (snd (stamp stored))
                      ++ ": "
                      ++ show refusal
                  pure Halt
                Right () -> do
                  eresult <- try @SomeException (handlePost stored >>= awaitTurn)
                  flow <- case eresult of
                    Left e -> do
                      let p = stamped stored
                          exc = T.pack (displayException e)
                          recipients = from p : maybe [] (pure . qcPitboss) mQuiesce
                      hPutStrLn stderr $
                        "🔴 "
                          ++ T.unpack agentName
                          ++ " handler failed on post "
                          ++ show (snd (stamp stored))
                          ++ ": "
                          ++ T.unpack exc
                      _ <-
                        postLocal root $
                          (mkPost agentName recipients (markGlyph Escalate <> " handler failed: " <> exc))
                            { thread = [snd (stamp stored)]
                            }
                      pure Continue
                    Right TurnCancelled -> pure Continue
                    Right (TurnResponse replies) -> do
                      let keep p =
                            let b = T.strip (body p)
                             in not (T.null b) && b /= "(empty)"
                          nonEmpty = filter keep replies
                          cite p = p {thread = sortNub (snd (stamp stored) : thread p)}
                      traverse_ (postLocal root . cite) nonEmpty
                      pure $
                        if any ((== Just StandDown) . markOf) replies
                          then Halt
                          else Continue
                  writeCursor root agentName (snd (stamp stored) + 1)
                  pure flow
    writeHeartbeat root agentName Idle
    pure result
  writeHeartbeat root agentName Dead
