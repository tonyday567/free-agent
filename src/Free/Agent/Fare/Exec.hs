{-# LANGUAGE OverloadedStrings #-}

-- | Shared process execution for fare items.
--
-- The cwd is the ROOT the item was asked about — never the caller's
-- cwd. (Caught in gate's smoke: ormolu first ran against fare's own
-- tree and threw an uncaught IOException on the scratch repo's
-- files. The trap lives here, once.)
module Free.Agent.Fare.Exec (runTool) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode)
import System.Process (cwd, proc, readCreateProcessWithExitCode)

-- | run a tool in ROOT, combining stdout and stderr. 'Left' carries
-- a could-not-run message (missing executable); the caller decides
-- how to report it.
runTool :: FilePath -> String -> [String] -> IO (Either Text (ExitCode, Text))
runTool root cmd args = do
  r <-
    try (readCreateProcessWithExitCode (proc cmd args) {cwd = Just root} "") ::
      IO (Either IOException (ExitCode, String, String))
  pure $ case r of
    Left e -> Left (T.pack (show e))
    Right (code, out, err) -> Right (code, T.pack out <> T.pack err)
