{-# LANGUAGE OverloadedStrings #-}

-- | Replay of the python acp-probe leg: spawn @kimi acp@, handshake, open a
-- session in a scratch cwd, switch to auto mode, prompt
-- @"reply with the word: beacon"@, assemble the @agent_message_chunk@ stream
-- and print the reply + sessionId.  Raw frames are logged to
-- @DIR/transcript.ndjson@.
--
-- Usage: free-agent-acp-probe [DIR]   (default /tmp/free-agent-acp-probe)
module Main (main) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Acp
  ( AcpConfig (..),
    defaultAcpConfig,
    openAcp,
    closeAcp,
    acpInitialize,
    acpNewSession,
    acpPrompt,
    acpReadStderr,
    acpSetModeAuto,
    PromptResult (..),
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Prelude

note :: Text -> IO ()
note = TIO.hPutStrLn stderr

failStep :: Text -> IO a
failStep step = do
  note ("FAIL at " <> step)
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  let root = fromMaybe "/tmp/free-agent-acp-probe" (listToMaybe args)
      sessCwd = root </> "session-cwd"
      transcript = root </> "transcript.ndjson"
  createDirectoryIfMissing True sessCwd
  tExists <- doesFileExist transcript
  if tExists then removeFile transcript else pure ()

  note "=== free-agent-acp-probe: starting kimi acp ==="
  c <-
    openAcp
      defaultAcpConfig
        { acpWorkDir = root </> "proc",
          acpTranscript = Just transcript
        }

  -- Step 1: initialize
  note "--- initialize ---"
  (mInit, _) <- acpInitialize c 15_000_000
  case mInit of
    Nothing -> failStep "initialize (no response)"
    Just _ -> note "  ok"

  -- Step 2: session/new (cwd pinned to scratch)
  note "--- session/new ---"
  (mSid, _) <- acpNewSession c 15_000_000 sessCwd
  sid <- case mSid of
    Nothing -> failStep "session/new (no sessionId)"
    Just s -> do
      note ("  sessionId: " <> s)
      pure s

  -- Step 3: auto mode (session/set_mode hangs; request_permission is broken
  -- in kimi 0.33.0 — auto routes file I/O via fs/* reverse-RPCs)
  note "--- session/set_config_option mode=auto ---"
  (mMode, _) <- acpSetModeAuto c 15_000_000 sid
  case mMode of
    Nothing -> failStep "session/set_config_option (no response)"
    Just _ -> note "  ok"

  -- Step 4: beacon prompt
  note "--- session/prompt ---"
  pr <- acpPrompt c 120_000_000 sid "reply with the word: beacon"
  note ("  stopReason: " <> maybe "N/A" id (prStopReason pr))
  note ("  chunks: " <> T.pack (show (length (prUpdates pr))))

  errLines <- acpReadStderr c
  note ("  stderr lines: " <> T.pack (show (length errLines)))

  closeAcp c

  -- Result on stdout
  TIO.putStrLn ("sessionId: " <> sid)
  TIO.putStrLn ("stopReason: " <> fromMaybe "N/A" (prStopReason pr))
  TIO.putStrLn ("reply: " <> prReply pr)
  if "beacon" `T.isInfixOf` prReply pr
    then TIO.putStrLn "beacon: FOUND"
    else do
      TIO.putStrLn "beacon: NOT FOUND"
      hPutStrLn stderr "probe failed: reply did not contain 'beacon'"
      exitFailure
