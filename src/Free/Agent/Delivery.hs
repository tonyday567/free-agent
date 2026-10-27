{-# LANGUAGE OverloadedStrings #-}

-- | Delivery and response settlement, separate from bus publication.
module Free.Agent.Delivery
  ( Invocation (..),
    Turn (..),
    TurnResult (..),
    TurnMetrics (..),
    deliverTo,
    mapTurn,
    DeliveryGrant,
    DeliveryRefusal (..),
    newDeliveryGrant,
    useDeliveryGrant,
    directPostTo,
    replyAct,
    postTo,
  )
where

import Circuit.Agent (Name, Post (..))
import Circuit.Agent.Framing (PostBody, Stamped, stamp, stamped)
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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Free.Agent.Bus (postLocal)

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
-- text stays local; no particular quiet string receives special treatment.
replyAct :: Name -> Stamped Text -> Text -> [Post Text]
replyAct agent stored response =
  case T.stripPrefix "@" response of
    Nothing -> []
    Just rest ->
      let (name, afterName) = T.break (== ':') rest
          recipient = T.strip name
          cause = stamped stored
       in if T.null recipient || T.null afterName
            then []
            else
              [ (Post agent (channel cause) [recipient] [snd (stamp stored)])
                  (T.strip (T.drop 1 afterName))
              ]

-- | Append a post to the public tape. This operation never invokes an agent.
postTo :: (PostBody a) => FilePath -> Post a -> IO (Stamped a)
postTo = postLocal
