{-# LANGUAGE OverloadedStrings #-}

-- | One-shot host algebra without a hermes dependency.
--
-- A 'Host' is a named effectful seat that receives arguments drawn from the
-- body of an incoming post and produces lines of output.  It is intentionally
-- minimal: real session management stays in the host harness (hermes,
-- muster-agent, etc.).
module Free.Agent.Host
  ( BodyMode (..),
    Host (..),
    host,
    hostShard,
    processHost,
  )
where

import Circuit (Ends (..), endsK)
import Circuit.Agent (Post (..), Shard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (MonadState (..))
import Data.Text (Text)
import Data.Text qualified as T
import System.Process (readProcess)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Free.Agent.Host
-- >>> import Circuit.Agent

-- | How to turn a post body into arguments for 'hostRun'.
data BodyMode
  = -- | Split on whitespace (default).
    BodyWords
  | -- | Split on newlines.
    BodyLines
  | -- | Pass the whole body as a single argument.
    BodyWhole
  deriving (Show, Eq)

-- | A one-shot host seat.
--
-- The host receives arguments derived from the body of an incoming post and
-- produces output lines.  The caller decides how to turn those lines back into
-- posts; 'hostShard' uses the default mapping (reply to sender).
data Host m = Host
  { -- | Name used as the 'from' field of reply posts.
    hostName :: Text,
    -- | How to split the incoming post body before calling 'hostRun'.
    hostBodyMode :: BodyMode,
    -- | Run the host on the prepared arguments.
    hostRun :: [Text] -> m [Text]
  }

-- | Smart constructor with the default 'BodyWords' mode.
host :: Text -> ([Text] -> m [Text]) -> Host m
host name f = Host name BodyWords f

-- | Split a post body according to the host's 'BodyMode'.
bodyArgs :: BodyMode -> Text -> [Text]
bodyArgs BodyWords = T.words
bodyArgs BodyLines = T.lines
bodyArgs BodyWhole = (:[])

-- | Turn a host into a stateful shard that consumes every committed post.
--
-- The shard remembers the committed posts in its state.  On emit it runs the
-- host on each post's body (prepared by 'hostBodyMode'), in order, and emits
-- one reply post per output line per input post.  Each reply is addressed back
-- to the sender of its input post.
hostShard ::
  (MonadState [Post] m) =>
  Host m ->
  Shard m [Post] [Post]
hostShard h =
  endsK
    (\xs -> put xs)
    ( do
        xs <- get
        put []
        fmap concat $
          traverse
            ( \p -> do
                outs <- hostRun h (bodyArgs (hostBodyMode h) (body p))
                pure [Post (hostName h) [from p] o | o <- outs]
            )
            xs
    )

-- | Sketch of a real host backed by an external process.
--
-- The command receives the fixed @args@ followed by the prepared post body
-- (one argument when 'BodyWhole', whitespace-split words by default).  Output
-- lines become reply posts.  This uses 'System.Process.readProcess' and lives
-- in 'IO' lifted through whatever state transformer holds the shard buffer.
processHost ::
  (MonadIO m) =>
  -- | Host name.
  Text ->
  -- | Command to run.
  FilePath ->
  -- | Fixed command arguments.
  [String] ->
  Host m
processHost name cmd args =
  Host
    { hostName = name,
      hostBodyMode = BodyWhole,
      hostRun = \ws -> do
        out <- liftIO (readProcess cmd (args ++ map T.unpack ws) "")
        pure (map T.pack (lines out))
    }
