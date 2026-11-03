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
import Control.Exception (IOException, SomeException, catch, throwIO, try)
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

-- | Interpret a response as a reply act. An explicit address chooses the
-- recipient, and leading whitespace before it is tolerated. On a direct
-- delivery, otherwise, non-empty prose replies to the causing sender; a first
-- line of @LOCAL@ keeps the whole response local. Public and self-caused
-- deliveries retain the local default. When a tool diff or other prefix
-- precedes an address at the start of a later line, only the last non-self
-- addressed suffix outside a fenced quotation is published; the prefix is not
-- bus content. Inline mentions and Markdown blockquote addresses are not
-- explicit addresses.
replyAct :: Name -> Stamped Text -> Text -> [Post Text]
replyAct agent stored response =
  case parseAddress response <|> parseLaterLineAddress agent response of
    Nothing -> maybe [] pure (defaultReply agent stored response)
    Just (recipient, replyBody) -> pure (reply recipient replyBody)
  where
    cause = stamped stored
    reply recipient replyBody =
      Post agent (channel cause) [recipient] [snd (stamp stored)] replyBody

-- | The direct-delivery default: answer the sender, unless publication is
-- explicitly suppressed. Empty responses and public or self-caused turns stay
-- local.
defaultReply :: Name -> Stamped Text -> Text -> Maybe (Post Text)
defaultReply agent stored response
  | agent `notElem` to cause = Nothing
  | from cause == agent = Nothing
  | localResponse response = Nothing
  | T.null (T.strip response) = Nothing
  | otherwise =
      Just $ Post agent (channel cause) [from cause] [snd (stamp stored)] (T.strip response)
  where
    cause = stamped stored

localResponse :: Text -> Bool
localResponse response =
  T.strip (T.takeWhile (/= '\n') (T.stripStart response)) == "LOCAL"

-- | Publish a reply act after making a misplaced address visible. Publication
-- and ancestry are exactly 'replyAct'; the warning is the only added effect.
replyActIO :: Name -> Stamped Text -> Text -> IO [Post Text]
replyActIO agent stored response = do
  traverse_ (TIO.hPutStrLn stderr) (replyWarning agent response)
  pure (replyAct agent stored response)

-- | A valid address at the start of a later line earns a warning with its
-- zero-based character offset. The addressed suffix publishes; the prefix does
-- not. Inline mentions and genuinely unaddressed replies do not produce this
-- warning; the latter may still use the direct-delivery default.
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
