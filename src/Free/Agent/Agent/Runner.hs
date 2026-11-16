{-# LANGUAGE OverloadedStrings #-}

-- | Generic batch-driven agent seat loop for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), presents each drained
-- mail batch to the supplied handler in one turn, and writes the returned
-- explicit post acts. A completed turn writes a transaction marker before the
-- cursor advances. This is the common runtime shared by the Hermes, Kimi,
-- direct-API, and external-command seats.
module Free.Agent.Agent.Runner
  ( runAgentLoop,
  )
where

import Circuit.Agent (Name)
import Circuit.Parser.Framing (Stored, decodeRow)
import Circuit.Post (Post (..), PostId, mkPost)
import Circuit.Shared (stamp, stamped)
import Circuit.Agent.Mark (Mark (..), controlMark, isEscalate, isHalt, markGlyph)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (void)
import Data.Foldable (forM_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
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
    recoverTransaction,
    tailLog,
    writeCursor,
    writeTransaction,
  )
import Free.Agent.Bus.Heartbeat (HbState (..), writeHeartbeat)
import Free.Agent.Delivery
  ( DeliveryFailure (..),
    DeliveryGrant,
    Turn,
    TurnCeiling (..),
    TurnResult (..),
    awaitTurn,
    deliveryFailureLabel,
    useDeliveryGrant,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.Process (getProcessID)
import Text.Read (readMaybe)

-- | Deliveries attempted for one drained batch before it is skipped.
--
-- A backend failure is retried once: the first attempt already contains the
-- CLI's own bounded retries, so a second delivery attempt is the last word on
-- that batch.
maxBatchAttempts :: Int
maxBatchAttempts = 2

-- | Consecutive skipped batches a seat tolerates before it stands down.
--
-- A seat that cannot deliver anything is broken as a seat, not as a
-- participant: it stops with its mail still behind the cursor.
maxConsecutiveSkips :: Int
maxConsecutiveSkips = 3

-- | Delivery-environment failures one seat tolerates before it stands down.
--
-- A resume or spawn failure leaves the batch behind an unmoved cursor, so the
-- next launch re-delivers it. Re-presenting it inside one process is a second
-- chance for an environment that blinked, not a repair for one that is down,
-- so the counter is small; at the ceiling the seat stops with its mail still
-- behind the cursor rather than advancing past work no turn ever read.
maxDeliveryFailures :: Int
maxDeliveryFailures = 3

-- | Pause before re-presenting a batch whose delivery environment failed, in
-- microseconds. Longer than 'batchBackoffMicros' because the cause is usually
-- a provider that is down rather than a turn that went wrong.
deliveryBackoffMicros :: Int -> Int
deliveryBackoffMicros n = 5_000_000 * n

-- | Pause before re-presenting a batch whose delivery failed, in microseconds.
batchBackoffMicros :: Int -> Int
batchBackoffMicros n = 1_000_000 * n

-- | Start a seat loop.
--
-- The caller supplies the subscription names, the bus root, an optional
-- quiescence config, a finite delivery grant, and a handler that turns one
-- drained batch of incoming 'Stored' into a live turn of explicit post
-- acts. The handler is responsible for decoration, scrubbing, response
-- interpretation, and any deliberate ancestry on those acts.
--
-- A batch is committed only after 'TurnResponse': its transaction marker is
-- written first, then its cursor. Cancellation writes neither and stops the
-- loop. A failed delivery is retried once, then relayed as an unaddressed
-- notice and skipped as a committed decision, so a backend failure cannot end
-- a seat; an explicit cancellation or a decided quiet still does. A failure of
-- the delivery environment itself — a backend that will not start, or a stored
-- session that will not resume after the CLI's own bounded retries — is
-- neither of those: no turn ran, so the batch is not committed, not skipped,
-- and not read past. It is re-presented after a pause and, once
-- 'maxDeliveryFailures' is reached, left behind an unmoved cursor for the next
-- launch. A turn the host stopped at its own tool-iteration budget is not
-- retried at all: it is committed once as a terminal decision, because
-- replaying it spends the budget again and can republish work the stopped turn
-- already did outside the runner. On restart, a marker ahead of the cursor
-- completes the interrupted cursor write; otherwise the uncemented mail is
-- presented again so the agent can inspect any effects already in the log and
-- continue.
--
-- Decided quiet: a delivered post in the control form carrying a halt mark
-- (🟢 or 🔵 — the glyph alone on its first line) stops the loop; an
-- escalation mark (🔴) in the control form is relayed to the pitboss (when a
-- quiescence config names one) and also stops the loop. A mark that leads
-- words is a label, not an order, and is delivered to the agent. The
-- quiescence counter is the observed-quiet bridge: after N empty cycles the
-- seat posts the verdict body from 'quiesceAgent' — a ⌛ timeout mark, an
-- inference from wall-clock silence, not a decided quiet — to the pitboss and
-- exits.
--
-- Self-halt: a 🔵 reply is the seat deciding its own quiet — the loop stops
-- after scribing it. The F2 self-post skip means the seat never reads its own
-- mark back, so the halt is judged here, at commit time. 🟢 stays
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
  -- | Handler: incoming mail batch -> turn of explicit post acts.
  ([Stored] -> IO (Turn [Post Text Text])) ->
  IO ()
runAgentLoop agentName names root mQuiesce grant handleBatch = do
  hSetBuffering stdout LineBuffering
  pid <- getProcessID
  TIO.putStrLn $ "🟢 free-agent seat starting: " <> T.intercalate "," names <> " (pid=" <> T.pack (show pid) <> ")"
  TIO.putStrLn $ "   root: " <> T.pack root
  let path = root </> "log.jsonl"
      onQuiesce d = do
        let qc = fromMaybe (error "quiesce action without config") mQuiesce
        _ <- postLocal root (mkPost agentName [qcPitboss qc] (decisionBody d))
        pure ()
      -- A mark is control, not content, and only in the control form: a
      -- glyph alone on the body's first line. A peer's status report
      -- (@🟢 tranche 1 done …@) leads with the same glyphs and is content —
      -- it reaches the handler. Halt and escalation marks in the control
      -- form are not handed to the seat. Silence follows the mark.
      stopping stored =
        case controlMark (stamped stored) of
          Just m -> isHalt m || isEscalate m
          Nothing -> False
      controlFlow stored = do
        case controlMark (stamped stored) of
          Just m
            | isEscalate m -> do
                let relay =
                      (mkPost agentName [] (markGlyph Escalate <> " escalation received; standing down"))
                        { thread = [snd (stamp stored)],
                          to = maybe [] (pure . qcPitboss) mQuiesce
                        }
                forM_ mQuiesce $ \_ -> postLocal root relay
          _ -> pure ()
        writeCursor root agentName (snd (stamp stored) + 1)
        pure Halt
      runBatch _ _ [] = pure Continue
      runBatch failures skips (stored : rest)
        | stopping stored = controlFlow stored
        | otherwise = do
            let (ordinary, remaining) = break stopping (stored : rest)
                addressed = filter ((/= agentName) . from . stamped) ordinary
                nextCursor = snd (stamp (last ordinary)) + 1
            if null addressed
              then writeCursor root agentName nextCursor >> runBatch failures skips remaining
              else
                deliverBatch failures skips addressed nextCursor >>= \case
                  Continue -> runBatch failures skips remaining
                  Halt -> pure Halt
      -- A batch is delivered at most 'maxBatchAttempts' times. A failed
      -- delivery is the backend's, not the exchange's: the cursor and the
      -- transaction stay untouched and the same batch is presented again after
      -- a pause. A batch that fails twice is one that cannot be done — it is
      -- skipped as a committed decision, so the seat keeps serving new mail
      -- instead of dying on one bad delivery. 'maxConsecutiveSkips' consecutive
      -- skips mean the seat is broken, not the mail: it stands down after the
      -- third skip is committed, and anything later stays behind the cursor.
      --
      -- A delivery-environment failure is not a turn failure and does not take
      -- that path: no turn ran, so nothing is known about the batch, and
      -- committing it would eat mail the seat still owes an answer to (#689).
      -- It gets its own small ceiling and an unmoved cursor instead.
      deliverBatch failures skips storedBatch nextCursor = attempt 1
        where
          batchIds = map (snd . stamp) storedBatch
          senders = nub [from p | p <- map stamped storedBatch, from p /= agentName]
          reached = T.pack (show (nextCursor - 1))
          ids = T.intercalate "," (map (T.pack . show) batchIds)
          senderNote
            | null senders = ""
            | otherwise = "; sender " <> T.intercalate "," senders
          attempt n = do
            granted <- useDeliveryGrant grant
            case granted of
              Left refusal -> do
                hPutStrLn stderr $
                  "delivery refused for "
                    ++ T.unpack agentName
                    ++ " on batch through post "
                    ++ show (nextCursor - 1)
                    ++ ": "
                    ++ show refusal
                pure Halt
              Right () -> do
                eresult <- try @SomeException (handleBatch storedBatch >>= awaitTurn)
                case eresult of
                  Right TurnCancelled -> pure Halt
                  Right (TurnResponse replies) -> do
                    writeIORef skips 0
                    writeIORef failures 0
                    let keep p =
                          let b = T.strip (body p)
                           in not (T.null b) && b /= "(empty)"
                    traverse_ (postLocal root) (filter keep replies)
                    writeTransaction root agentName nextCursor
                    writeCursor root agentName nextCursor
                    pure $
                      if any ((== Just StandDown) . controlMark) replies
                        then Halt
                        else Continue
                  Left e
                    | Just ceiling <- fromException e -> ceilingBatch ceiling
                    | Just df <- fromException e -> environmentFailed n df
                    | otherwise -> do
                        let exc = T.pack (displayException e)
                            failed = T.unpack agentName ++ " handler failed on batch through post " ++ show (nextCursor - 1)
                        hPutStrLn stderr $
                          "🔴 "
                            ++ failed
                            ++ " (attempt "
                            ++ show n
                            ++ "): "
                            ++ T.unpack (T.take 400 exc)
                            ++ if n < maxBatchAttempts
                              then "; retrying the same batch"
                              else "; skipping the batch"
                        if n < maxBatchAttempts
                          then threadDelay (batchBackoffMicros n) >> attempt (n + 1)
                          else skipBatch exc
          -- A ceiling is a terminal decision, not a failed attempt. The host
          -- ran the batch to the end of its own call budget, so re-invoking it
          -- would spend that budget again and could duplicate a reply the turn
          -- already published outside the runner. The batch is committed once
          -- — transaction, then cursor — and its senders are named in a notice
          -- rather than woken by one. It is a non-delivery, so three in a row
          -- still stand a broken seat down.
          ceilingBatch (TurnCeiling why) = do
            hPutStrLn stderr $
              "🔴 "
                ++ T.unpack agentName
                ++ " turn hit the tool-iteration ceiling on batch through post "
                ++ T.unpack reached
                ++ "; committed without a reply: "
                ++ T.unpack (T.take 400 why)
            notice $
              markGlyph Escalate
                <> " turn hit the tool-iteration ceiling on posts "
                <> ids
                <> ": "
                <> T.take 200 why
                <> "; batch closed without a reply — re-send if the work still matters"
            writeTransaction root agentName nextCursor
            writeCursor root agentName nextCursor
            seen <- readIORef skips
            writeIORef skips (seen + 1)
            pure $ if seen + 1 >= maxConsecutiveSkips then Halt else Continue
          -- The skip is a committed decision, so it writes the transaction
          -- marker before the cursor exactly as a settled batch does.
          skipBatch exc = do
            hPutStrLn stderr $
              "🔴 "
                ++ T.unpack agentName
                ++ " skipping the batch through post "
                ++ show (nextCursor - 1)
                ++ ": "
                ++ T.unpack (T.take 400 exc)
            notice $
              markGlyph Escalate
                <> " failed on posts "
                <> ids
                <> " twice: "
                <> T.take 200 exc
                <> "; batch skipped — re-send if the work still matters"
            writeTransaction root agentName nextCursor
            writeCursor root agentName nextCursor
            seen <- readIORef skips
            writeIORef skips (seen + 1)
            pure $ if seen + 1 >= maxConsecutiveSkips then Halt else Continue
          -- The delivery environment failed, not the turn: nothing about the
          -- batch was learned, so it is not judged. Neither marker moves, which
          -- is what leaves the batch for the next launch. One process gets
          -- 'maxDeliveryFailures' second chances — a pause for an environment
          -- that blinked — and then stops, still behind its cursor.
          environmentFailed n df = do
            seen <- readIORef failures
            let attemptNo = seen + 1
                label = deliveryFailureLabel (dfKind df)
                why = T.strip (T.take 240 (dfDetail df))
                lastWords
                  | attemptNo >= maxDeliveryFailures = "standing down with the batch behind an unmoved cursor"
                  | otherwise = "cursor unmoved, re-presenting the same batch"
            writeIORef failures attemptNo
            hPutStrLn stderr $
              "🔴 "
                ++ T.unpack agentName
                ++ " "
                ++ T.unpack label
                ++ " on batch through post "
                ++ show (nextCursor - 1)
                ++ " (attempt "
                ++ show attemptNo
                ++ " of "
                ++ show maxDeliveryFailures
                ++ "): "
                ++ T.unpack why
                ++ "; "
                ++ lastWords
            notice $
              markGlyph Escalate
                <> " "
                <> label
                <> " on posts "
                <> ids
                <> ": "
                <> why
                <> "; attempt "
                <> T.pack (show attemptNo)
                <> " of "
                <> T.pack (show maxDeliveryFailures)
                <> "; this attempt ran no turn and the cursor is unmoved — the batch stays for the next launch"
            if attemptNo >= maxDeliveryFailures
              then pure Halt
              else threadDelay (deliveryBackoffMicros attemptNo) >> attempt n
          -- A notice is evidence for whoever supervises the seat, not mail: it
          -- is unaddressed, so no seat can spend a turn answering it. The
          -- addressed form bought a turn per notice, which failed while the
          -- provider was down and bought another — #707-#712 is that loop, six
          -- posts of pure overhead. It names every post id in the batch rather
          -- than only the thread head, and carries what disk says about the
          -- turn so "published then died" and "never ran" stop looking alike.
          notice bodyLine = do
            evidence <- batchEvidence root agentName batchIds
            void $
              postLocal
                root
                ( mkPost
                    agentName
                    []
                    ( bodyLine
                        <> senderNote
                        <> "; evidence "
                        <> evidence
                        <> "; unaddressed on purpose — answering this would spend a turn on it"
                    )
                )
  recoverTransaction root agentName
  cursor <- readCursor root agentName
  TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
  -- Heartbeats: wake on start, working around each delivered batch, idle
  -- after it, dead on clean exit. The file carries state, timestamp, and
  -- pid. `bus status` probes the pid for idle liveness and uses
  -- staleness only for states that promise another transition.
  skips <- newIORef 0
  failures <- newIORef 0
  writeHeartbeat root agentName Wake
  tailLog path names cursor (fmap (\qc -> (qc, onQuiesce)) mQuiesce) $ \storedBatch -> do
    writeHeartbeat root agentName Working
    result <- runBatch failures skips storedBatch
    writeHeartbeat root agentName Idle
    pure result
  writeHeartbeat root agentName Dead

-- | What disk says about a batch at failure time: which of its posts this seat
-- already published against, and which transcript rows exist for them.
--
-- The two states that looked identical in the field — a turn that published and
-- then died, and a turn that never ran — differ exactly here. A published post
-- citing a batch id means the turn got far enough to answer it; transcript rows
-- mean the backend was invoked and say how it ended. Both are read from files
-- the seat already writes, so the notice costs no delivery.
batchEvidence ::
  -- | Bus root.
  FilePath ->
  -- | Seat name whose publications and transcript rows count.
  Name ->
  -- | Post ids in the batch.
  [PostId] ->
  IO Text
batchEvidence root agentName ids = do
  published <- publishedAgainst
  rows <- transcriptRows
  pure ("published " <> published <> "; transcript " <> rows)
  where
    batchEnd = maximum (0 : ids)
    publishedAgainst = do
      let path = root </> "log.jsonl"
      exists <- doesFileExist path
      if not exists
        then pure "none"
        else do
          ls <- T.lines <$> TIO.readFile path
          -- This seat publishes replies without ancestry, so a citation alone
          -- is too narrow a test. Appended order is the honest one: an
          -- addressed post by this seat above the batch's last id was written
          -- after the batch was delivered, i.e. by the turn that is now
          -- failing. A citation of a batch id counts too, for a turn that did
          -- set ancestry. Unaddressed posts by this seat are excluded: a
          -- failure notice is one of them, and counting it would make every
          -- notice the evidence that a turn had published.
          let hits =
                [ snd (stamp s)
                  | l <- ls,
                    Right s <- [decodeRow l],
                    let p = stamped s,
                    from p == agentName,
                    not (null (to p)),
                    snd (stamp s) > batchEnd || any (`elem` ids) (thread p)
                ]
          pure $ case hits of
            [] -> "none"
            xs -> T.intercalate "," (map (T.pack . show) xs)
    transcriptRows = do
      let path = root </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      exists <- doesFileExist path
      if not exists
        then pure "none"
        else do
          ls <- T.lines <$> TIO.readFile path
          let outcomes = [rowOutcome l | l <- ls, maybe False (`elem` ids) (rowPost l)]
          pure $ case outcomes of
            [] -> "none"
            xs -> T.pack (show (length xs)) <> " rows (" <> T.intercalate "," xs <> ")"
    rowPost l =
      readMaybe . T.unpack . T.takeWhile (/= ',')
        =<< T.stripPrefix "{\"post_id\":" l
    rowOutcome l = case T.breakOn "\"outcome\":\"" l of
      (_, rest)
        | T.null rest -> "?"
        | otherwise -> T.takeWhile (/= '"') (T.drop (T.length "\"outcome\":\"") rest)
