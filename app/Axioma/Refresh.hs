{-# LANGUAGE OverloadedStrings #-}

module Axioma.Refresh
  ( refreshTopic,
  )
where

import Axioma.Common (assert, tempRoot)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File (readCursor, writeCursor)
import Free.Agent.Delivery (Invocation (..), TurnResult (..), awaitTurn, deliverTo, mapTurnIO)
import Free.Agent.Refresh (preserveForRefresh, refreshDue, refreshRequestPath, writeRefreshRequest)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removePathForcibly)
import System.FilePath ((</>))

refreshTopic :: IO ()
refreshTopic = do
  putStrLn "Automatic same-name refresh"
  assert "below threshold does not refresh" $ not (refreshDue 150000 149999)
  assert "threshold is inclusive" $ refreshDue 150000 150000
  assert "above threshold refreshes" $ refreshDue 150000 150001
  root <- tempRoot "free-agent-refresh"
  let sessionFile = root </> ".sessions" </> "seat.sid"
  createDirectoryIfMissing True (root </> ".sessions")
  TIO.writeFile sessionFile "old-session\n"
  writeCursor root "seat" 41
  writeRefreshRequest root "seat" 150000
  notePath <- preserveForRefresh root "seat" sessionFile
  sessionGone <- not <$> doesFileExist sessionFile
  requestGone <- not <$> doesFileExist (refreshRequestPath root "seat")
  archives <- listDirectory (root </> ".sessions")
  archiveName <- case archives of
    [name] -> pure name
    _ -> fail "refresh did not leave exactly one sid archive"
  assert "refresh archives the old sid" $ sessionGone && "seat.sid.pre-refresh-" `T.isPrefixOf` T.pack archiveName
  assert "refresh consumes the request" requestGone
  archive <- TIO.readFile (root </> ".sessions" </> archiveName)
  note <- TIO.readFile notePath
  cursor <- readCursor root "seat"
  assert "archived sid preserves the old session" $ T.strip archive == "old-session"
  assert "reboot note carries measured tokens and old sid" $ "150000" `T.isInfixOf` note && "old-session" `T.isInfixOf` note
  assert "refresh preserves cursor" $ cursor == 41
  assert "reboot note names no objectives" $ not ("JEV" `T.isInfixOf` note)
  removePathForcibly root

  events <- newIORef []
  turn <-
    deliverTo
      (\() _ -> pure Invocation {runInvocation = modifyIORef' events (<> ["settled"]) >> pure (TurnResponse "ok"), cancelInvocation = pure False})
      ()
      ""
  result <- awaitTurn $ mapTurnIO (\response -> modifyIORef' events (<> ["metered"]) >> pure response) turn
  assert "post-settlement mapper returns the response" $ result == TurnResponse "ok"
  order <- readIORef events
  assert "usage metering happens after CLI settlement" $ order == ["settled", "metered"]

  cancelled <-
    deliverTo
      (\() _ -> pure Invocation {runInvocation = pure TurnCancelled, cancelInvocation = pure False})
      ()
      ""
  cancelledResult <- awaitTurn $ mapTurnIO (\response -> modifyIORef' events (<> ["wrong"]) >> pure response) cancelled :: IO (TurnResult T.Text)
  assert "post-settlement mapper preserves cancellation" $ cancelledResult == TurnCancelled
  cancelledOrder <- readIORef events
  assert "cancelled turns are not metered" $ cancelledOrder == ["settled", "metered"]
