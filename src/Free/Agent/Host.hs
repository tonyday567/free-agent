{-# LANGUAGE OverloadedStrings #-}

-- | One-shot host algebra without a hermes dependency.
--
-- A 'Host' is a named effectful seat that receives command-line style arguments
-- and returns lines of output.  It is intentionally minimal: real session
-- management stays in the host harness (hermes, muster-agent, etc.).
module Free.Agent.Host
  ( Host (..),
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

-- | A one-shot host seat.
--
-- The host receives arguments drawn from the body of an incoming post and
-- produces output lines.  The caller decides how to turn those lines back into
-- posts; 'hostShard' uses the default mapping (reply to sender).
data Host m = Host
  { -- | Name used as the 'from' field of reply posts.
    hostName :: Text,
    -- | Run the host on arguments derived from the incoming post body.
    hostRun :: [Text] -> m [Text]
  }

-- | Turn a host into a stateful shard that consumes every committed post.
--
-- The shard remembers the committed posts in its state.  On emit it runs the
-- host on each post's body (split into words), in order, and emits one reply
-- post per output line per input post.  Each reply is addressed back to the
-- sender of its input post.
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
                outs <- hostRun h (T.words (body p))
                pure [Post (hostName h) [from p] o | o <- outs]
            )
            xs
    )

-- | Sketch of a real host backed by an external process.
--
-- The command receives the post body as stdin and its output lines become the
-- reply.  This uses 'System.Process.readProcess' and lives in 'IO' (lifted
-- through whatever state transformer holds the shard buffer).
processHost ::
  (MonadIO m) =>
  -- | Host name.
  Text ->
  -- | Command to run.
  FilePath ->
  -- | Command arguments.
  [String] ->
  Host m
processHost name cmd args =
  Host
    { hostName = name,
      hostRun = \ws -> do
        out <- liftIO (readProcess cmd (args ++ map T.unpack ws) "")
        pure (map T.pack (lines out))
    }
