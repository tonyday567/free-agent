{-# LANGUAGE OverloadedStrings #-}

-- | Same-name seat refresh: threshold and durable handoff preparation.
module Free.Agent.Refresh
  ( refreshDue,
    refreshRequestPath,
    writeRefreshRequest,
    preserveForRefresh,
  )
where

import Free.Agent.Agent (Name)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Free.Agent.Bus.File (readCursor)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath ((</>))

-- | A refresh is due at or above the configured latest-prompt token count.
--
-- >>> refreshDue 150000 149999
-- False
-- >>> refreshDue 150000 150000
-- True
refreshDue :: Int -> Int -> Bool
refreshDue threshold tokens = tokens >= threshold

refreshRequestPath :: FilePath -> Name -> FilePath
refreshRequestPath root name = root </> (".refresh-" <> T.unpack name)

writeRefreshRequest :: FilePath -> Name -> Int -> IO ()
writeRefreshRequest root name tokens =
  TIO.writeFile (refreshRequestPath root name) (T.pack (show tokens) <> "\n")

-- | Preserve the current session and write the one-time reboot note.
preserveForRefresh :: FilePath -> Name -> FilePath -> IO FilePath
preserveForRefresh root name sessionFile = do
  tokens <- T.strip <$> TIO.readFile (refreshRequestPath root name)
  sid <- T.strip <$> TIO.readFile sessionFile
  cursor <- readCursor root name
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S%Q" now
      archive = sessionFile <> ".pre-refresh-" <> stamp
      notePath = root </> "handoffs" </> (T.unpack name <> "-" <> stamp <> ".md")
  createDirectoryIfMissing True (root </> "handoffs")
  renameFile sessionFile archive
  TIO.writeFile notePath $
    T.unlines
      [ "# seat reboot",
        "previous session: " <> sid,
        "archived sid: " <> T.pack archive,
        "refresh prompt tokens: " <> tokens,
        "bus cursor preserved at: " <> T.pack (show cursor),
        "The initial prompt is unchanged. Resume from current bus posts and cards."
      ]
  renameFile (refreshRequestPath root name) (notePath <> ".request")
  pure notePath
