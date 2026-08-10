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

import Circuit.Agent (Name, Post (..), mkPost)
import Circuit.Agent.Framing (Stamped (..), stamp, stamped)
import Circuit.Agent.Mark (Mark (..), isEscalate, isHalt, markGlyph, markOf)
import Control.Exception (SomeException, displayException, try)
import Data.Foldable (forM_, traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File
  ( Flow (..),
    QuiesceConfig (..),
    cursorPath,
    readCursor,
    tailLog,
    writeCursor,
  )
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

-- | Start a seat loop.
--
-- The caller supplies the subscription names, the bus root, an optional
-- quiescence config, and a handler that turns one incoming 'Stamped Text' into
-- zero or more reply posts. The handler is responsible for decoration,
-- scrubbing, routing, and filtering; this function handles the bus plumbing.
--
-- Decided quiet: a delivered post carrying a halt mark (🟢 or 🔵) stops the
-- loop; an escalation mark (🔴) is relayed to the pitboss (when a quiescence
-- config names one) and also stops the loop. The quiescence counter is the
-- observed-quiet bridge: after N empty cycles the seat posts 🔵 to the
-- pitboss and exits.
--
-- Self-halt: a 🔵 reply is the seat deciding its own quiet — the loop stops
-- after scribing it. The F2 self-post skip means the seat never reads its
-- own mark back, so the halt is judged here, at commit time. 🟢 stays
-- exchange-level: a seat may land one exchange and host more.
runAgentLoop ::
  -- | Agent name used for cursor and logging.
  Text ->
  -- | Subscription names.
  [Name] ->
  -- | Bus root.
  FilePath ->
  -- | Optional quiescence config.
  Maybe QuiesceConfig ->
  -- | Handler: incoming post -> reply posts.
  (Stamped Text -> IO [Post Text]) ->
  IO ()
runAgentLoop agentName names root mQuiesce handlePost = do
  hSetBuffering stdout LineBuffering
  pid <- getProcessID
  TIO.putStrLn $ "🟢 free-agent seat starting: " <> T.intercalate "," names <> " (pid=" <> T.pack (show pid) <> ")"
  TIO.putStrLn $ "   root: " <> T.pack root
  let path = root </> "log.jsonl"
      onQuiesce = do
        let qc = maybe (error "quiesce action without config") id mQuiesce
            p = mkPost agentName [qcPitboss qc] (markGlyph StandDown <> " standing down after " <> T.pack (show (qcCycles qc)) <> " empty cycles")
        _ <- postLocal root p
        pure ()
      -- A mark is control, not content: halt and escalation marks are not
      -- handed to the seat handler. Silence follows the mark.
      controlFlow stored =
        case markOf (stamped stored) of
          Just m
            | isHalt m -> pure (Just Halt)
            | isEscalate m -> do
                forM_ mQuiesce $ \qc ->
                  postLocal root (mkPost agentName [qcPitboss qc] (markGlyph Escalate <> " escalation received; standing down"))
                pure (Just Halt)
          _ -> pure Nothing
  cursor <- readCursor root agentName
  TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
  tailLog path names cursor (fmap (\qc -> (qc, onQuiesce)) mQuiesce) $ \stored ->
    controlFlow stored >>= \case
      Just flow -> do
        writeCursor root agentName (stamp stored + 1)
        pure flow
      Nothing -> do
        -- F2: skip self-posts — an agent should never receive its own posts
        if from (stamped stored) == agentName
          then do
            writeCursor root agentName (stamp stored + 1)
            pure Continue
          else do
            ereplies <- try @SomeException (handlePost stored)
            flow <- case ereplies of
              Left e -> do
                let p = stamped stored
                    exc = T.pack (displayException e)
                    recipients = from p : maybe [] (pure . qcPitboss) mQuiesce
                hPutStrLn stderr $
                  "🔴 " ++ T.unpack agentName ++ " handler failed on post "
                    ++ show (stamp stored)
                    ++ ": "
                    ++ T.unpack exc
                _ <-
                  postLocal root $
                    mkPost agentName recipients (markGlyph Escalate <> " handler failed: " <> exc)
                -- handler failure: cursor advanced, seat keeps listening
                pure Continue
              Right replies -> do
                traverse_ (postLocal root) replies
                -- Self-halt: the seat's own 🔵, judged at commit time.
                pure $
                  if any ((== Just StandDown) . markOf) replies
                    then Halt
                    else Continue
            writeCursor root agentName (stamp stored + 1)
            pure flow
