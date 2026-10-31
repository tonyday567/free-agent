{-# LANGUAGE OverloadedStrings #-}

-- | Delivery and response settlement, separate from bus publication.
module Free.Agent.Delivery
  ( Invocation (..),
    Turn (..),
    TurnResult (..),
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
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Free.Agent.Bus (postLocal)
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

-- | Interpret an explicitly addressed response as a reply act. Plain response
-- text stays local. When a tool diff or other prefix precedes an address at the
-- start of a later line, only that addressed suffix is published; the prefix is
-- not bus content. Inline mentions and addresses inside fenced quotations stay
-- local.
replyAct :: Name -> Stamped Text -> Text -> [Post Text]
replyAct agent stored response =
  case parseAddress response <|> parseLaterLineAddress response of
    Nothing -> []
    Just (recipient, replyBody) ->
      [ (Post agent (channel cause) [recipient] [snd (stamp stored)])
          replyBody
      ]
  where
    cause = stamped stored

-- | Publish a reply act after making a misplaced address visible. Publication
-- and ancestry are exactly 'replyAct'; the warning is the only added effect.
replyActIO :: Name -> Stamped Text -> Text -> IO [Post Text]
replyActIO agent stored response = do
  traverse_ (TIO.hPutStrLn stderr) (replyWarning agent response)
  pure (replyAct agent stored response)

-- | A valid address at the start of a later line earns a warning with its
-- zero-based character offset. The addressed suffix publishes; the prefix does
-- not. Inline mentions and genuinely unaddressed replies stay silent.
replyWarning :: Name -> Text -> Maybe Text
replyWarning agent response =
  case parseAddress response of
    Just _ -> Nothing
    Nothing -> do
      (offset, recipient, _) <- laterLineAddress response
      pure
        ( "🟡 "
            <> agent
            <> ": @"
            <> recipient
            <> ": begins at character offset "
            <> T.pack (show offset)
            <> ", not 0; publishing addressed suffix"
        )

parseLaterLineAddress :: Text -> Maybe (Name, Text)
parseLaterLineAddress response =
  (\(_, recipient, replyBody) -> (recipient, replyBody))
    <$> laterLineAddress response

laterLineAddress :: Text -> Maybe (Int, Name, Text)
laterLineAddress response =
  listToMaybe
    [ (T.length prefix + 1, recipient, replyBody)
    | (prefix, rest) <- T.breakOnAll "\n@" response,
      not (insideFence prefix),
      Just (recipient, replyBody) <- pure (parseAddress (T.drop 1 rest))
    ]

insideFence :: Text -> Bool
insideFence = odd . length . filter isFence . T.lines

isFence :: Text -> Bool
isFence line = "```" `T.isPrefixOf` T.stripStart line

parseAddress :: Text -> Maybe (Name, Text)
parseAddress response = do
  rest <- T.stripPrefix "@" response
  let (name, afterName) = T.break (== ':') rest
      recipient = T.strip name
  if T.null recipient || T.null afterName
    then Nothing
    else Just (recipient, T.strip (T.drop 1 afterName))

-- | Append a post to the public tape. This operation never invokes an agent.
postTo :: (PostBody a) => FilePath -> Post a -> IO (Stamped a)
postTo = postLocal
