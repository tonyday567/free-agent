{-# LANGUAGE OverloadedStrings #-}

-- | fare gate — distilled build/lint verdict.
--
-- Layer 1 is one verdict line. PASS names every tool with its time;
-- FAIL names the first failing tool, its time, and a signal line (for
-- build: the first line containing @rror@, case-folded; for ormolu
-- and hlint: the first non-empty output line — those tools print
-- nothing but findings). The failing tool's full output goes to a
-- temp artifact for @fare read@ to slice — deeper layers are one ask
-- away. Tools short-circuit at the first failure: the data's common
-- question is yes/no (wave 1: build is the top category and the
-- common read is a verdict), and a failing build makes the lint
-- gates moot.
--
-- Scoping decisions, recorded on cake/fare.md:
--
--   ⟜ docspec is opt-in: --docspec selects the repository package and
--     excludes linked local projects; --docspec-full selects the whole
--     linked plan. Both test the last build, which a fresh gate run
--     just changed (the verify-card trap)
--   ⟜ no --head/--tail knob: a verdict line has no slice to flip,
--     so the flag would be a lie on this item
--   ⟜ ormolu uses git ls-files (the house form, absorbing the
--     ritual); untracked .hs files are not checked, as in the house
--     gate — pre-commit seats check those explicitly
--   ⟜ exit 1 on FAIL — a gate verdict is actionable, and exit codes
--     are the honest channel (the rituals used echo sentinels)
module Free.Agent.Fare.Gate
  ( DocspecScope (..),
    docspecArgs,
    docspecPackage,
    runGate,
  )
where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Free.Agent.Fare.Exec (runTool)
import System.Directory (doesDirectoryExist, getTemporaryDirectory, listDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (stderr)
import System.Posix.Process (getProcessID)
import Text.Printf (printf)

-- | Which doctest population an opt-in docspec gate selects.
data DocspecScope
  = DocspecRepository
  | DocspecFull
  deriving (Eq, Show)

-- | Arguments passed to cabal-docspec. Its package target selects only
-- that package's modules from the cabal plan; no target retains the full
-- linked-project sweep.
docspecArgs :: DocspecScope -> String -> [String]
docspecArgs DocspecRepository package = [package]
docspecArgs DocspecFull _ = []

-- | Select the package name from the repository's single cabal file.
docspecPackage :: [FilePath] -> Either Text String
docspecPackage entries = case filter ((== ".cabal") . takeExtension) entries of
  [cabalFile] -> Right (takeBaseName cabalFile)
  [] -> Left "no .cabal file at repository root"
  _ -> Left "multiple .cabal files at repository root; repository docspec needs one package"

-- | a named gate check: run it, get an exit code and combined output.
type Check = IO (ExitCode, Text)

data DocspecSelection
  = SelectRepository String
  | SelectFull

-- | run the gate suite over ROOT and print the layer-1 verdict.
runGate :: FilePath -> Maybe DocspecScope -> IO ()
runGate root scope = do
  dirOk <- doesDirectoryExist root
  unless dirOk (die (T.pack root <> ": no such directory"))
  repo <- readTool root "git" ["-C", root, "rev-parse", "--is-inside-work-tree"]
  unless (fst repo == ExitSuccess) (die (T.pack root <> ": not a git repository (the ormolu gate needs git ls-files)"))
  docspec <- resolveDocspec scope root
  result <- go (checks docspec root) [] 0
  case result of
    Right (timed, total) -> do
      let parts = [name <> " " <> secs <> "s" | (name, secs) <- reverse timed]
      TIO.putStrLn ("GATE PASS (" <> packSecs total <> "s): " <> T.intercalate ", " parts <> "; " <> verdictScope docspec)
    Left (name, secs, out) -> do
      TIO.putStrLn ("GATE FAIL — " <> name <> " (" <> secs <> "s): " <> signal name (T.lines out) <> "; " <> verdictScope docspec)
      artifactPath <- writeArtifact root out
      TIO.putStrLn ("full log: " <> T.pack artifactPath)
      exitFailure
  where
    go [] timed acc = pure (Right (timed, acc))
    go ((name, action) : rest) timed acc = do
      start <- getCurrentTime
      (code, out) <- action
      stop <- getCurrentTime
      let secs = packSecs (diffUTCTime stop start)
      if code == ExitSuccess
        then go rest ((name, secs) : timed) (acc + diffUTCTime stop start)
        else pure (Left (name, secs, out))

resolveDocspec :: Maybe DocspecScope -> FilePath -> IO (Maybe DocspecSelection)
resolveDocspec Nothing _ = pure Nothing
resolveDocspec (Just DocspecFull) _ = pure (Just SelectFull)
resolveDocspec (Just DocspecRepository) root = do
  entries <- listDirectory root
  package <- either die pure (docspecPackage entries)
  pure (Just (SelectRepository package))

-- ---------------------------------------------------------------------------
-- Checks
-- ---------------------------------------------------------------------------

checks :: Maybe DocspecSelection -> FilePath -> [(Text, Check)]
checks docspec root =
  [ ("build", buildC),
    ("ormolu", ormoluC),
    ("hlint", hlintC)
  ]
    ++ case docspec of
      Nothing -> []
      Just SelectFull -> [("docspec", readTool root "cabal-docspec" (docspecArgs DocspecFull ""))]
      Just (SelectRepository package) ->
        [("docspec", readTool root "cabal-docspec" (docspecArgs DocspecRepository package))]
  where
    buildC = readTool root "cabal" ["build", "all"]
    ormoluC = do
      (code, out) <- readTool root "git" ["-C", root, "ls-files", "*.hs"]
      if code /= ExitSuccess
        then pure (code, out)
        else case words (T.unpack out) of
          [] -> pure (ExitSuccess, "")
          files -> readTool root "ormolu" ("--mode" : "check" : files)
    hlintC = readTool root "hlint" ["."]

-- | run a tool in ROOT; a missing executable is a gate error, not a
-- silent skip.
readTool :: FilePath -> String -> [String] -> IO (ExitCode, Text)
readTool root cmd args = do
  r <- runTool root cmd args
  case r of
    Left e -> die ("could not run " <> T.pack cmd <> ": " <> e)
    Right ok -> pure ok

-- ---------------------------------------------------------------------------
-- Output
-- ---------------------------------------------------------------------------

verdictScope :: Maybe DocspecSelection -> Text
verdictScope Nothing = "excluded: docspec[repository], docspec[full]"
verdictScope (Just SelectFull) = "docspec scope: full (all linked local projects)"
verdictScope (Just (SelectRepository package)) =
  "docspec scope: repository=" <> T.pack package <> " (linked local projects excluded)"

-- | the one signal line a failure distills to.
signal :: Text -> [Text] -> Text
signal name ls
  | name == "build" = firstMatch (\l -> "rror" `T.isInfixOf` T.toCaseFold l) ls
  | otherwise = firstMatch (not . T.null . T.strip) ls

firstMatch :: (Text -> Bool) -> [Text] -> Text
firstMatch p xs = case filter p xs of
  (x : _) -> x
  [] -> "(no output)"

packSecs :: NominalDiffTime -> Text
packSecs d = T.pack (printf "%.1f" (realToFrac d :: Double))

writeArtifact :: FilePath -> Text -> IO FilePath
writeArtifact root out = do
  pid <- getProcessID
  tmp <- (</> ("fare-gate-" <> show pid <> ".log")) <$> getTemporaryDirectory
  TIO.writeFile tmp ("fare gate — cwd: " <> T.pack root <> "\n" <> out)
  pure tmp

die :: Text -> IO a
die msg = do
  TIO.hPutStrLn stderr ("fare gate: " <> msg)
  exitFailure
