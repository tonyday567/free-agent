{-# LANGUAGE OverloadedStrings #-}

-- | Delivery and response settlement, separate from bus publication.
module Free.Agent.Delivery
  ( Invocation (..),
    Turn (..),
    TurnResult (..),
    TurnCeiling (..),
    DeliveryFailure (..),
    DeliveryFailureKind (..),
    deliveryFailureLabel,
    TurnMetrics (..),
    deliverTo,
    mapTurn,
    mapTurnIO,
    DeliveryGrant,
    DeliveryRefusal (..),
    newDeliveryGrant,
    useDeliveryGrant,
    directPostTo,
    replyAct,
    replyActIO,
    replyWarning,
    strandedNote,
    noteStrand,
    strandNotePath,
    strandBlock,
    takeStrandNotes,
    postTo,
  )
where

import Circuit.Agent (Name, Post (..))
import Circuit.Agent.Framing (PostBody, Stamped, stamp, stamped)
import Control.Applicative ((<|>))
import Control.Concurrent
  ( MVar,
    forkIO,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Exception (Exception, IOException, SomeException, catch, displayException, throwIO, try)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Free.Agent.Bus (postLocal)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO (stderr)

-- | One backend invocation and its cancellation request. Cancellation returns
-- 'True' only when it wins against backend completion; the winning side owns
-- the outcome evidence.
data Invocation a = Invocation
  { runInvocation :: IO (TurnResult a),
    cancelInvocation :: IO Bool
  }

-- | The memoized result of a turn.
data TurnResult a
  = TurnResponse a
  | TurnCancelled
  deriving (Eq, Show, Functor)

-- | A turn the host stopped at its own budget rather than at its work.
--
-- A tool-iteration ceiling is neither a host interruption (exit 130, which is
-- the host's event and recovers in a fresh session) nor a backend failure
-- (which is retried): the turn ran until its call budget was spent, produced
-- the work it produced, and would produce the same partial work if replayed.
-- Carrying it as its own exception is what lets the runner commit the batch as
-- a terminal decision instead of re-invoking the host.
newtype TurnCeiling = TurnCeiling Text
  deriving (Show)

instance Exception TurnCeiling

-- | A failure of the delivery environment rather than of the turn.
--
-- Nothing about the batch caused it: the backend could not be started
-- ('SpawnFailed'), or a stored session could not be resumed once the CLI's own
-- bounded retries were spent ('ResumeFailed'). It is a third class beside a
-- failed turn and a turn ceiling, because neither of their policies fits: no
-- turn ran, so committing the batch as skipped would discard mail the seat
-- still owes its senders an answer to, and re-running the batch in the same
-- process cannot repair an environment that is down. The class exists so the
-- runner can leave the batch behind its cursor for the next launch to
-- re-deliver instead.
data DeliveryFailure = DeliveryFailure
  { dfKind :: DeliveryFailureKind,
    dfDetail :: Text
  }
  deriving (Eq, Show)

-- | Which part of the environment failed: the launch, or the session the
-- launch was asked to resume.
data DeliveryFailureKind = ResumeFailed | SpawnFailed
  deriving (Eq, Show)

-- | The detail is the message: a notice and a stderr line both want the
-- reason, not a @Show@ rendering of a record.
instance Exception DeliveryFailure where
  displayException = T.unpack . dfDetail

-- | Short wire word for a failure kind, for notices and log lines.
deliveryFailureLabel :: DeliveryFailureKind -> Text
deliveryFailureLabel ResumeFailed = "resume failed"
deliveryFailureLabel SpawnFailed = "spawn failed"

-- | Live, backend-neutral delivery cost and status.
data TurnMetrics = TurnMetrics
  { tmInFlight :: Bool,
    tmCancelled :: Bool,
    tmElapsedMs :: Int
  }
  deriving (Eq, Show)

-- | A live delivery whose settlement can be awaited repeatedly. Cancellation
-- waits for backend cleanup so outcome evidence is durable before it returns.
data Turn a = Turn
  { awaitTurn :: IO (TurnResult a),
    cancelTurn :: IO (),
    turnMetrics :: IO TurnMetrics
  }

mapTurn :: (a -> b) -> Turn a -> Turn b
mapTurn f turn = turn {awaitTurn = fmap (fmap f) (awaitTurn turn)}

-- | Run an effect after settlement, never during turn construction or on cancellation.
mapTurnIO :: (a -> IO b) -> Turn a -> Turn b
mapTurnIO f turn =
  turn
    { awaitTurn =
        awaitTurn turn >>= \case
          TurnResponse a -> TurnResponse <$> f a
          TurnCancelled -> pure TurnCancelled
    }

-- | Invoke a backend for one open-caused input. Publication is not part of
-- delivery: a caller may settle quietly or explicitly use 'postTo' afterwards.
deliverTo :: (cause -> Text -> IO (Invocation a)) -> cause -> Text -> IO (Turn a)
deliverTo invoke cause input = do
  invocation <- invoke cause input
  started <- getCurrentTime
  settled <- newEmptyMVar
  done <- newEmptyMVar
  _ <- forkIO $ do
    result <- try @SomeException (runInvocation invocation)
    _ <- tryPutMVar settled result
    putMVar done ()
  let await = readMVar settled >>= either throwIO pure
      cancel = do
        won <- cancelInvocation invocation
        if won then void (tryPutMVar settled (Right TurnCancelled)) else pure ()
        readMVar done
      metrics = do
        now <- getCurrentTime
        result <- tryReadMVar settled
        pure
          TurnMetrics
            { tmInFlight = maybe True (const False) result,
              tmCancelled = case result of
                Just (Right TurnCancelled) -> True
                _ -> False,
              tmElapsedMs = floor (realToFrac (diffUTCTime now started) * (1000 :: Double))
            }
  pure (Turn await cancel metrics)

-- | A local, atomic count of direct deliveries. This is one adapter policy,
-- not a bus-wide authority schema.
newtype DeliveryGrant = DeliveryGrant (MVar Int)

data DeliveryRefusal = DeliveryBudgetExhausted
  deriving (Eq, Show)

newDeliveryGrant :: Int -> IO DeliveryGrant
newDeliveryGrant = fmap DeliveryGrant . newMVar . max 0

useDeliveryGrant :: DeliveryGrant -> IO (Either DeliveryRefusal ())
useDeliveryGrant (DeliveryGrant budget) =
  modifyMVar budget $ \remaining ->
    pure
      (if remaining > 0 then (remaining - 1, Right ()) else (0, Left DeliveryBudgetExhausted))

-- | Publish through an explicit direct connector, consuming at most one grant
-- unit. The post receipt is returned even when delivery is refused.
directPostTo ::
  DeliveryGrant ->
  (Stamped Text -> IO (Turn a)) ->
  FilePath ->
  Post Text ->
  IO (Stamped Text, Either DeliveryRefusal (Turn a))
directPostTo grant deliver root post = do
  stored <- postTo root post
  granted <- useDeliveryGrant grant
  result <- case granted of
    Right () -> Right <$> deliver stored
    Left refusal -> pure (Left refusal)
  pure (stored, result)

-- | Interpret a response as intentional publication. Only an explicit
-- address chooses a recipient and publishes; unaddressed prose, including a
-- first line of @LOCAL@, stays local. Publication carries no inferred cause,
-- channel, or ancestry. An agent that wants a thread cites a visible post id
-- through an explicit bus act. When a tool diff or other prefix precedes an
-- address at the start of a later line, only the last non-self addressed
-- suffix outside a fenced quotation is published; the prefix is not bus
-- content. Inline mentions and Markdown blockquote addresses are not explicit
-- addresses.
replyAct :: Name -> Text -> [Post Text]
replyAct agent response =
  case parseAddress response <|> parseLaterLineAddress agent response of
    Nothing -> []
    Just (recipient, replyBody) -> pure (Post agent Nothing [recipient] [] replyBody)

-- | Publish an intentional response after making a misplaced address visible.
replyActIO :: Name -> Text -> IO [Post Text]
replyActIO agent response = do
  traverse_ (TIO.hPutStrLn stderr) (replyWarning agent response)
  pure (replyAct agent response)

-- | A valid address at the start of a later line earns a warning with its
-- zero-based character offset. The addressed suffix publishes; the prefix does
-- not. Inline mentions and genuinely unaddressed replies produce no warning
-- and no publication.
replyWarning :: Name -> Text -> Maybe Text
replyWarning agent response =
  case parseAddress response of
    Just _ -> Nothing
    Nothing -> do
      (offset, recipient, _) <- laterLineAddress agent response
      pure
        ( "🟡 "
            <> agent
            <> ": @"
            <> recipient
            <> ": begins at character offset "
            <> T.pack (show offset)
            <> ", not 0; publishing addressed suffix"
        )

parseLaterLineAddress :: Name -> Text -> Maybe (Name, Text)
parseLaterLineAddress agent response =
  (\(_, recipient, replyBody) -> (recipient, replyBody))
    <$> laterLineAddress agent response

laterLineAddress :: Name -> Text -> Maybe (Int, Name, Text)
laterLineAddress agent response =
  listToMaybe
    ( reverse
        [ (T.length prefix + 1 + indent, recipient, replyBody)
        | (prefix, rest) <- newlineTails response,
          not (insideFence prefix),
          let (spaces, line) = T.span isSpace rest,
          let indent = T.length spaces,
          Just (recipient, replyBody) <- pure (parseAddress line),
          recipient /= agent
        ]
    )
  where
    newlineTails t = [(pre, T.drop 1 post) | (pre, post) <- T.breakOnAll "\n" t]

insideFence :: Text -> Bool
insideFence = odd . length . filter isFence . T.lines

isFence :: Text -> Bool
isFence line = "```" `T.isPrefixOf` T.stripStart line

-- | An address at the start of the response, allowing leading whitespace:
-- @\@@, a non-empty name without spaces, and a colon.
parseAddress :: Text -> Maybe (Name, Text)
parseAddress response = do
  rest <- T.stripPrefix "@" (T.stripStart response)
  let (name, afterName) = T.break (== ':') rest
      recipient = T.strip name
  if T.null recipient || T.null afterName || T.any (\c -> isSpace c || c == '@') recipient
    then Nothing
    else Just (recipient, T.strip (T.drop 1 afterName))

-- ---------------------------------------------------------------------------
-- Stranded replies, told on the next delivery
-- ---------------------------------------------------------------------------

-- | Where a seat's pending stranded-reply notes live under a bus root.
strandNotePath :: FilePath -> Name -> FilePath
strandNotePath root name = root </> (".strand-" <> T.unpack name)

-- | The delivered note for a stranded reply: publication recovered a later
-- line's addressed suffix and the text above the address stayed local. It is
-- a harness fact for the seat's next delivery, never formatting blame; a
-- prefix-first address, plain prose, and local responses leave no note.
strandedNote :: Name -> Stamped Text -> Text -> Maybe Text
strandedNote agent stored response =
  case parseAddress response of
    Just _ -> Nothing
    Nothing ->
      ( \(_, recipient, _) ->
          "🟡 your reply for delivery "
            <> T.pack (show (snd (stamp stored)))
            <> " published from its `@"
            <> recipient
            <> ":` address onward only; the text above the address did not reach the bus."
      )
        <$> laterLineAddress agent response

-- | Record a stranded reply for the seat's next delivery; silence otherwise.
-- A failed write is bookkeeping loss, not turn failure.
noteStrand :: FilePath -> Name -> Stamped Text -> Text -> IO ()
noteStrand root agent stored response =
  catch
    ( traverse_
        (\note -> TIO.appendFile (strandNotePath root agent) (note <> "\n"))
        (strandedNote agent stored response)
    )
    (\(_ :: IOException) -> pure ())

-- | Read and clear the pending stranded-reply notes for a seat.
takeStrandNotes :: FilePath -> Name -> IO [Text]
takeStrandNotes root agent =
  catch
    ( do
        contents <- TIO.readFile path
        catch (removeFile path) (\(_ :: IOException) -> pure ())
        pure (filter (not . T.null) (T.lines contents))
    )
    (\(_ :: IOException) -> pure [])
  where
    path = strandNotePath root agent

-- | Compose the delivered prompt block for stranded-reply notes; empty when
-- there are none. Begins with a blank line so callers place it after any
-- prompt prefix and before their own message header.
strandBlock :: [Text] -> Text
strandBlock [] = ""
strandBlock notes = "\n\nHarness notes:\n" <> T.intercalate "\n" notes

-- | Append a post to the public tape. This operation never invokes an agent.
postTo :: (PostBody a) => FilePath -> Post a -> IO (Stamped a)
postTo = postLocal
