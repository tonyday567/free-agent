{-# LANGUAGE OverloadedStrings #-}

-- | fare state — what has happened recently in a repo.
--
-- Layer 1 is one page: a header line (branch and upstream position,
-- the last commit, the dirty count — always printed), then the body
-- under the item's own byte budget ('layer1Bound'): dirties (git
-- status --short, verbatim), recent activity (git log --oneline -5),
-- and a distilled diff (git diff --stat). --head/--tail choose which
-- end of the BODY the budget keeps; the header is never sliced —
-- it is the verdict.
--
-- Absorbs the week's git rituals (wave 1: 179 segments — git log
-- --oneline -1 ×59, git status --short ×54, git diff --stat ×26).
-- The deeper layer — a full diff — is the free-form lane's job
-- (fare bash git diff): state distills, bash expands.
--
-- Clean repos skip the dirties and diffstat sections; repos with no
-- commits yet say so. Not a git repository is a real error (stderr,
-- exit 1), as is a missing git executable.
module Free.Agent.Fare.State (runState) where

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Fare.Config (End (..), applyEndUnder)
import Free.Agent.Fare.Exec (runTool)
import System.Directory (doesDirectoryExist, getTemporaryDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import System.Posix.Process (getProcessID)

-- | the item's own layer-1 bound for the body, in bytes.
layer1Bound :: Int
layer1Bound = 2000

-- | how much recent activity the item distills to.
recentWindow :: Int
recentWindow = 5

-- | run a state read and print the distilled layer-1 page.
runState :: FilePath -> Maybe End -> IO ()
runState root mend = do
  dirOk <- doesDirectoryExist root
  unless dirOk (die (T.pack root <> ": no such directory"))
  repo <- tool root "git" ["-C", root, "rev-parse", "--is-inside-work-tree"]
  unless (fst repo == ExitSuccess) (die (T.pack root <> ": not a git repository"))
  statusOut <- tool root "git" ["-C", root, "status", "-sb"]
  logOut <- tool root "git" ["-C", root, "log", "--oneline", "-" <> show recentWindow]
  statOut <- tool root "git" ["-C", root, "diff", "--stat"]
  let statusLines = T.lines (snd statusOut)
      (branchLine, dirtyLines) = case statusLines of
        [] -> ("", [])
        (b : rest) -> (T.drop 3 (T.strip b), rest)
      dirties = [l | l <- dirtyLines, not (T.null (T.strip l))]
      lastCommit = case fst logOut of
        ExitSuccess -> case T.lines (snd logOut) of
          (l : _) -> T.strip l
          [] -> ""
        _ -> "(no commits yet)"
      recent = case fst logOut of
        ExitSuccess -> T.lines (snd logOut)
        _ -> []
      diffstat = case fst statOut of
        ExitSuccess -> T.lines (snd statOut)
        _ -> []
      dirtyCount = length dirties
      header =
        "state: "
          <> branchLine
          <> " · last "
          <> lastCommit
          <> " · "
          <> T.pack (show dirtyCount)
          <> " dirty"
      body =
        ( ["dirties (" <> T.pack (show dirtyCount) <> "):" | dirtyCount > 0]
            ++ dirties
        )
          ++ (["recent (" <> T.pack (show (length recent)) <> "):" | not (null recent)] ++ recent)
          ++ (["diffstat:" | not (null diffstat)] ++ diffstat)
      end = fromMaybe Head mend
      shown = applyEndUnder layer1Bound end body
  TIO.putStrLn header
  mapM_ TIO.putStrLn shown
  when (length shown < length body) $ do
    artifactPath <- writeArtifact root header body
    TIO.putStrLn ("next: fare read " <> T.pack artifactPath <> " 1,40")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- a tool call that dies with the state prefix on could-not-run.
tool :: FilePath -> String -> [String] -> IO (ExitCode, Text)
tool root cmd args = do
  r <- runTool root cmd args
  case r of
    Left e -> die ("could not run " <> T.pack cmd <> ": " <> e)
    Right ok -> pure ok

writeArtifact :: FilePath -> Text -> [Text] -> IO FilePath
writeArtifact root header body = do
  pid <- getProcessID
  tmp <- (</> ("fare-state-" <> show pid <> ".txt")) <$> getTemporaryDirectory
  TIO.writeFile tmp ("fare state — root: " <> T.pack root <> "\n" <> header <> "\n" <> T.unlines body)
  pure tmp

die :: Text -> IO a
die msg = do
  TIO.hPutStrLn stderr ("fare state: " <> msg)
  exitFailure
