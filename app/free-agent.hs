{-# LANGUAGE OverloadedStrings #-}

-- | Unified free-agent CLI.
--
-- Default command runs a Hermes-backed bus seat:
--
--   free-agent --root ROOT --name NAME --prompt PROMPT.md
--
-- Other forms:
--
--   free-agent --backend kimi --root ROOT --name NAME --prompt PROMPT.md
--   free-agent llm --root ROOT --name NAME --prompt PROMPT.md
--   free-agent cmd --root ROOT --name NAME --cmd CMD [ARG...]
--   free-agent bus [SUBCOMMAND]
--   free-agent status [ROOT] [--threshold SECS]
module Main (main) where

import Circuit.Parser.Framing (Stored)
import Circuit.Post (Post (..), mkPost)
import Circuit.Shared (stamp, stamped)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Free.Agent.Acp (AcpConfig (..), defaultAcpConfig)
import Free.Agent.Agent (Name)
import Free.Agent.Backend (acpBackend, commandBackend, directHttpBackend, dshAcpBackend, dshBackend, gatewayBackend, hermesBackend, kimiBackend)
import Free.Agent.Bus.Cli (BusCommand, Depth (..), busParser, depthOpt, orgOpt, runBusCommand, runStatus)
import Free.Agent.Bus.File (QuiesceConfig (..))
import Free.Agent.Cli.Config
  ( AcpCliConfig (..),
    Backend (..),
    CommandConfig (..),
    DshAcpConfig (..),
    DshConfig (..),
    GatewayCliConfig (..),
    HermesConfig (..),
    KimiConfig (..),
    LlmConfig (..),
    parseAcpConfig,
    parseCommandConfig,
    parseDshAcpConfig,
    parseDshConfig,
    parseHermesConfig,
    parseLlmConfig,
    swallowedSeatOption,
    wakeFromOpt,
  )
import Free.Agent.Gateway (GatewayConfig (..), defaultGatewayConfig)
import Free.Agent.Host (BareConfig (..), defaultBareConfig)
import Free.Agent.Mark (Mark (StandDown), markGlyph)
import Free.Agent.Protocol (defaultProtocol, protocolSummary)
import Free.Agent.Refresh (preserveForRefresh, refreshDue, refreshRequestPath, writeRefreshRequest)
import Free.Agent.Usage (readHermesUsage, readUsageIdentity, renderUsageIdentity)
import Options.Applicative
import Paths_free_agent (version)
import Seat.Runtime (Seat (..), runSeat)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, makeAbsolute)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, waitForProcess)

-- ---------------------------------------------------------------------------
-- Top-level command type
-- ---------------------------------------------------------------------------

data Command
  = AgentCmd Int (Maybe [Name]) Backend
  | BusCmd BusCommand
  | StatusCmd FilePath (Maybe NominalDiffTime) Depth Bool
  | UsageCmd HermesUsageSource
  | RefreshCmd Int Int (Maybe [Name]) HermesConfig
  | RefreshChildCmd Int Int (Maybe FilePath) (Maybe [Name]) HermesConfig
  deriving (Show)

data HermesUsageSource
  = HermesSessionFile FilePath
  | HermesUsageFile FilePath
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Backend tag for default agent command
-- ---------------------------------------------------------------------------

data BackendTag = HermesTag | KimiTag | GatewayTag
  deriving (Show, Eq)

backendFlag :: Parser BackendTag
backendFlag =
  option
    (eitherReader parseTag)
    ( long "backend"
        <> short 'b'
        <> metavar "BACKEND"
        <> value HermesTag
        <> showDefault
        <> help "Agent backend for the default seat: hermes | kimi | gateway"
    )
  where
    parseTag "hermes" = Right HermesTag
    parseTag "kimi" = Right KimiTag
    parseTag "gateway" = Right GatewayTag
    parseTag other = Left ("unknown backend: " ++ other)

toKimiConfig :: HermesConfig -> KimiConfig
toKimiConfig h =
  KimiConfig
    { kcRoot = hcRoot h,
      kcNames = hcNames h,
      kcPromptFile = hcPromptFile h,
      kcSessionFile = hcSessionFile h,
      kcModel = hcModel h,
      kcProvider = hcProvider h,
      kcQuiesce = hcQuiesce h,
      kcPitboss = hcPitboss h
    }

toGatewayConfig :: HermesConfig -> GatewayCliConfig
toGatewayConfig h =
  GatewayCliConfig
    { gcwRoot = hcRoot h,
      gcwNames = hcNames h,
      gcwPromptFile = hcPromptFile h,
      gcwBaseUrl = gwBaseUrl defaultGatewayConfig,
      gcwKeyEnv = gwKeyEnv defaultGatewayConfig,
      gcwQuiesce = hcQuiesce h,
      gcwPitboss = hcPitboss h
    }

deliveryBudgetOpt :: Parser Int
deliveryBudgetOpt =
  option
    auto
    ( long "delivery-budget"
        <> metavar "N"
        <> value 1_000_000
        <> showDefault
        <> help "Maximum backend deliveries before this seat stands down"
    )

refreshTokensOpt :: Parser Int
refreshTokensOpt =
  option
    auto
    ( long "refresh-tokens"
        <> metavar "N"
        <> value 150_000
        <> showDefault
        <> help "Refresh after a settled turn at or above N latest-prompt tokens"
    )

rebootNoteOpt :: Parser (Maybe FilePath)
rebootNoteOpt =
  optional
    ( strOption
        ( long "reboot-note"
            <> metavar "FILE"
            <> internal
            <> help "One-time reboot note injected by the refresh supervisor"
        )
    )

defaultAgentParser :: Parser Command
defaultAgentParser =
  AgentCmd
    <$> deliveryBudgetOpt
    <*> wakeFromOpt
    <*> ((\tag cfg -> mkBackend tag cfg) <$> backendFlag <*> parseHermesConfig)
  where
    mkBackend HermesTag cfg = BackendHermes cfg
    mkBackend KimiTag cfg = BackendKimi (toKimiConfig cfg)
    mkBackend GatewayTag cfg = BackendGateway (toGatewayConfig cfg)

-- ---------------------------------------------------------------------------
-- Top-level parser
-- ---------------------------------------------------------------------------

commandParser :: Parser Command
commandParser =
  subparser
    ( command "bus" (info (BusCmd <$> busParser defaultProtocol <**> helper) (progDesc "Bus subcommands"))
        <> command "llm" (info (AgentCmd <$> deliveryBudgetOpt <*> wakeFromOpt <*> (BackendLlm <$> parseLlmConfig) <**> helper) (progDesc "Direct API seat"))
        <> command "cmd" (info (AgentCmd <$> deliveryBudgetOpt <*> wakeFromOpt <*> (BackendCommand <$> parseCommandConfig) <**> helper) (progDesc "External-command seat"))
        <> command "dsh" (info (AgentCmd <$> deliveryBudgetOpt <*> wakeFromOpt <*> (BackendDsh <$> parseDshConfig) <**> helper) (progDesc "DSH seat"))
        <> command "dsh-acp" (info (AgentCmd <$> deliveryBudgetOpt <*> wakeFromOpt <*> (BackendDshAcp <$> parseDshAcpConfig) <**> helper) (progDesc "Persistent DSH ACP seat"))
        <> command "acp" (info (AgentCmd <$> deliveryBudgetOpt <*> wakeFromOpt <*> (BackendAcp <$> parseAcpConfig) <**> helper) (progDesc "ACP seat (persistent repl)"))
        <> command "status" (info (StatusCmd <$> rootOpt <*> optional thresholdOpt <*> depthOpt (Depth 1) <*> orgOpt <**> helper) (progDesc "Unified bus seat census"))
        <> command
          "usage"
          ( info
              ( subparser
                  ( command
                      "hermes"
                      (info (UsageCmd <$> hermesUsageSourceParser <**> helper) (progDesc "Latest-prompt tokens or one-shot C/F/O identity for Hermes"))
                  )
                  <**> helper
              )
              (progDesc "Seat usage measurements")
          )
        <> command
          "refresh"
          ( info
              (RefreshCmd <$> deliveryBudgetOpt <*> refreshTokensOpt <*> wakeFromOpt <*> parseHermesConfig <**> helper)
              (progDesc "Supervise a Hermes seat with automatic same-name session refresh")
          )
        <> command
          "refresh-child"
          ( info
              (RefreshChildCmd <$> deliveryBudgetOpt <*> refreshTokensOpt <*> rebootNoteOpt <*> wakeFromOpt <*> parseHermesConfig <**> helper)
              (progDesc "Internal refresh-supervised Hermes child")
          )
    )
    <|> defaultAgentParser
  where
    rootOpt =
      option
        str
        ( long "root"
            <> short 'r'
            <> metavar "ROOT"
            <> value "bus/"
            <> showDefault
            <> help "Bus root directory"
        )
    thresholdOpt =
      option
        (eitherReader readSeconds)
        ( long "threshold"
            <> short 't'
            <> metavar "SECS"
            <> help "Seconds before an unverifiable heartbeat reads as non-fab"
        )
      where
        readSeconds s = case reads s of
          [(n, "")] -> Right (fromInteger n :: NominalDiffTime)
          _ -> Left ("expected a whole number of seconds, got: " ++ s)

hermesUsageSourceParser :: Parser HermesUsageSource
hermesUsageSourceParser =
  (HermesSessionFile <$> argument str (metavar "SESSION_FILE"))
    <|> ( HermesUsageFile
            <$> strOption
              ( long "usage-file"
                  <> metavar "PATH"
                  <> help "Read C/F/O from Hermes -z --usage-file; context history is reported unavailable"
              )
        )

opts :: ParserInfo Command
opts =
  info
    ((commandParser <**> helper) <**> versionOption)
    ( fullDesc
        <> progDesc "Unified free-agent CLI"
        <> header "free-agent - bus, seats, and status"
        <> footer (T.unpack (protocolSummary defaultProtocol))
    )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("free-agent " <> showVersion version)
    ( long "version"
        <> short 'V'
        <> help "Print the build version and exit"
    )

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    AgentCmd budget wakeFrom backend -> runBackend budget wakeFrom backend
    BusCmd busCmd -> runBusCommand defaultProtocol busCmd
    StatusCmd root mthreshold depth org -> runStatus root (fromMaybe 900 mthreshold) depth org
    UsageCmd source -> case source of
      HermesSessionFile sessionFile ->
        readHermesUsage sessionFile >>= \case
          Left err -> TIO.putStrLn ("🔴 " <> err) >> exitFailure
          Right (sid, tokens) -> TIO.putStrLn ("🟢 session=" <> sid <> " prompt_tokens=" <> T.pack (show tokens))
      HermesUsageFile usageFile ->
        readUsageIdentity usageFile >>= \case
          Left err -> TIO.putStrLn ("🔴 " <> err) >> exitFailure
          Right identity -> TIO.putStr (renderUsageIdentity identity)
    RefreshCmd budget threshold wakeFrom cfg -> runRefreshSupervisor budget threshold wakeFrom cfg
    RefreshChildCmd budget threshold note wakeFrom cfg -> runHermesAuto (Just threshold) note budget wakeFrom cfg

runBackend :: Int -> Maybe [Name] -> Backend -> IO ()
runBackend budget wakeFrom (BackendHermes cfg) = runHermes budget wakeFrom cfg
runBackend budget wakeFrom (BackendKimi cfg) = runKimi budget wakeFrom cfg
runBackend budget wakeFrom (BackendDsh cfg) = runDsh budget wakeFrom cfg
runBackend budget wakeFrom (BackendDshAcp cfg) = runDshAcp budget wakeFrom cfg
runBackend budget wakeFrom (BackendAcp cfg) = runAcp budget wakeFrom cfg
runBackend budget wakeFrom (BackendLlm cfg) = runLlm budget wakeFrom cfg
runBackend budget wakeFrom (BackendCommand cfg) = runCommand budget wakeFrom cfg
runBackend budget wakeFrom (BackendGateway cfg) = runGateway budget wakeFrom cfg

-- ---------------------------------------------------------------------------
-- Shared seat configuration
-- ---------------------------------------------------------------------------

agentNameOf :: [Name] -> Text
agentNameOf [] = "agent"
agentNameOf (n : _) = n

defaultSessionFile :: FilePath -> [Name] -> FilePath
defaultSessionFile root names = root </> ".sessions" </> T.unpack (agentNameOf names) <> ".sid"

validatePrompt :: FilePath -> IO ()
validatePrompt path = do
  exists <- doesFileExist path
  unless exists $ do
    TIO.hPutStrLn stderr ("🔴 prompt file not found: " <> T.pack path)
    exitFailure

buildQuiesce :: Maybe Int -> Maybe Name -> Maybe QuiesceConfig
buildQuiesce Nothing _ = Nothing
buildQuiesce (Just n) mp = Just (QuiesceConfig n (fromMaybe "pitboss" mp) 1000000)

decoratePost :: Stored -> Stored
decoratePost stored =
  let p = stamped stored
      pid = T.pack (show (snd (stamp stored)))
   in stored {stamped = p {body = "[post " <> pid <> "] " <> from p <> ": " <> body p}}

renderMail :: [Stored] -> Text
renderMail = T.intercalate "\n\n" . map (body . stamped . decoratePost)

runHermes :: Int -> Maybe [Name] -> HermesConfig -> IO ()
runHermes = runHermesAuto Nothing Nothing

runHermesAuto :: Maybe Int -> Maybe FilePath -> Int -> Maybe [Name] -> HermesConfig -> IO ()
runHermesAuto threshold rebootFile budget wakeFrom cfg = do
  validatePrompt (hcPromptFile cfg)
  systemPrompt <- TIO.readFile (hcPromptFile cfg)
  rebootNote <- maybe (pure "") TIO.readFile rebootFile
  firstDelivery <- newIORef True
  let names = hcNames cfg
      agentName = agentNameOf names
      root = hcRoot cfg
      sessionFile = fromMaybe (defaultSessionFile root names) (hcSessionFile cfg)
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      backend = hermesBackend (hcModel cfg) (hcProvider cfg) (hcReasoning cfg) (hcYolo cfg) sessionFile
      mQuiesce = buildQuiesce (hcQuiesce cfg) (hcPitboss cfg)
      automaticRefresh =
        case threshold of
          Nothing -> pure []
          Just limit ->
            readHermesUsage sessionFile >>= \case
              Left err -> do
                hPutStrLn stderr ("refresh usage unavailable: " <> T.unpack err)
                pure []
              Right (_, tokens)
                | not (refreshDue limit tokens) -> pure []
                | otherwise -> do
                    writeRefreshRequest root agentName tokens
                    pure
                      [ mkPost
                          agentName
                          []
                          (markGlyph StandDown <> "\nautomatic refresh at " <> T.pack (show tokens) <> " prompt tokens")
                      ]
      promptFor storedBatch = do
        isFirst <- readIORef firstDelivery
        writeIORef firstDelivery False
        let note = if isFirst then rebootNote else ""
        pure (framedPrompt <> note <> "\n\nUser messages:\n" <> renderMail storedBatch)
  createDirectoryIfMissing True (takeDirectory sessionFile)
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runSeat (Seat budget agentName names wakeFrom root mQuiesce backend promptFor automaticRefresh)
  requested <- doesFileExist (refreshRequestPath root agentName)
  if isJust threshold && requested then exitWith (ExitFailure 75) else pure ()

runRefreshSupervisor :: Int -> Int -> Maybe [Name] -> HermesConfig -> IO ()
runRefreshSupervisor budget threshold wakeFrom cfg = supervise Nothing []
  where
    names = hcNames cfg
    agentName = agentNameOf names
    sessionFile = fromMaybe (defaultSessionFile (hcRoot cfg) names) (hcSessionFile cfg)
    childArgs rebootFile =
      ["refresh-child", "--delivery-budget", show budget, "--refresh-tokens", show threshold]
        <> maybe [] (\path -> ["--reboot-note", path]) rebootFile
        <> ["--root", hcRoot cfg]
        <> concatMap (\name -> ["--name", T.unpack name]) names
        <> ["--prompt", hcPromptFile cfg]
        <> maybe [] (concatMap (\sender -> ["--wake-from", T.unpack sender])) wakeFrom
        <> maybe [] (\path -> ["--session", path]) (hcSessionFile cfg)
        <> maybe [] (\model -> ["--model", T.unpack model]) (hcModel cfg)
        <> maybe [] (\provider -> ["--provider", T.unpack provider]) (hcProvider cfg)
        <> ["--no-yolo" | not (hcYolo cfg)]
        <> maybe [] (\seconds -> ["--quiesce", show seconds]) (hcQuiesce cfg)
        <> maybe [] (\pitboss -> ["--pitboss", T.unpack pitboss]) (hcPitboss cfg)
    supervise rebootFile recent = do
      executable <- getExecutablePath
      child <- spawnProcess executable (childArgs rebootFile)
      code <- waitForProcess child
      case code of
        ExitFailure 75 -> do
          now <- getCurrentTime
          let current = now : filter (addUTCTime (-60) now <) recent
          if length current > 10
            then hPutStrLn stderr "refresh supervisor stopped: more than 10 refreshes within 60 seconds" >> exitFailure
            else do
              notePath <- preserveForRefresh (hcRoot cfg) agentName sessionFile
              supervise (Just notePath) current
        ExitSuccess -> pure ()
        ExitFailure _ -> exitWith code

runKimi :: Int -> Maybe [Name] -> KimiConfig -> IO ()
runKimi budget wakeFrom cfg = do
  validatePrompt (kcPromptFile cfg)
  systemPrompt <- TIO.readFile (kcPromptFile cfg)
  let names = kcNames cfg
      agentName = agentNameOf names
      sessionFile = fromMaybe (defaultSessionFile (kcRoot cfg) names) (kcSessionFile cfg)
      backend = kimiBackend (kcModel cfg) (kcProvider cfg) sessionFile
      mQuiesce = buildQuiesce (kcQuiesce cfg) (kcPitboss cfg)
      promptFor = pure . ((systemPrompt <> "\n\nUser messages:\n") <>) . renderMail
  createDirectoryIfMissing True (takeDirectory sessionFile)
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runSeat (Seat budget agentName names wakeFrom (kcRoot cfg) mQuiesce backend promptFor (pure []))

runDsh :: Int -> Maybe [Name] -> DshConfig -> IO ()
runDsh budget wakeFrom cfg = do
  validatePrompt (dcPromptFile cfg)
  systemPrompt <- TIO.readFile (dcPromptFile cfg)
  let names = dcNames cfg
      agentName = agentNameOf names
      sessionPath = dcRoot cfg </> ".transcripts" </> T.unpack agentName <> ".sid"
      backend = dshBackend (Just systemPrompt) (dcDshPatch cfg) (dcDshModelPatch cfg) sessionPath
      mQuiesce = buildQuiesce (dcQuiesce cfg) (dcPitboss cfg)
  createDirectoryIfMissing True (takeDirectory sessionPath)
  TIO.putStrLn $ "   patch: " <> T.pack (dcDshPatch cfg)
  forM_ (dcDshModelPatch cfg) $ \path ->
    TIO.putStrLn $ "   model patch: " <> T.pack path
  runSeat (Seat budget agentName names wakeFrom (dcRoot cfg) mQuiesce backend (pure . renderMail) (pure []))

runDshAcp :: Int -> Maybe [Name] -> DshAcpConfig -> IO ()
runDshAcp budget wakeFrom cfg = do
  validatePrompt (dacPromptFile cfg)
  systemPrompt <- TIO.readFile (dacPromptFile cfg)
  root <- makeAbsolute (dacRoot cfg)
  workspace <- makeAbsolute (dacWorkspace cfg)
  dshHome <- makeAbsolute (dacDshHome cfg)
  basePatch <- makeAbsolute (dacDshPatch cfg)
  modelPatch <- traverse makeAbsolute (dacDshModelPatch cfg)
  let names = dacNames cfg
      agentName = agentNameOf names
      sessionPath = defaultSessionFile root names
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      backend =
        dshAcpBackend
          (Just framedPrompt)
          basePatch
          modelPatch
          workspace
          dshHome
          sessionPath
      mQuiesce = buildQuiesce (dacQuiesce cfg) (dacPitboss cfg)
  createDirectoryIfMissing True workspace
  createDirectoryIfMissing True dshHome
  createDirectoryIfMissing True (takeDirectory sessionPath)
  TIO.putStrLn "   protocol: dsh acp"
  TIO.putStrLn $ "   workspace: " <> T.pack workspace
  TIO.putStrLn $ "   DSH_HOME: " <> T.pack dshHome
  TIO.putStrLn $ "   patch: " <> T.pack basePatch
  forM_ modelPatch $ \path ->
    TIO.putStrLn $ "   model patch: " <> T.pack path
  runSeat (Seat budget agentName names wakeFrom root mQuiesce backend (pure . renderMail) (pure []))

acpTurnMicros :: Int
acpTurnMicros = 600_000_000

runAcp :: Int -> Maybe [Name] -> AcpCliConfig -> IO ()
runAcp budget wakeFrom cfg = do
  validatePrompt (acPromptFile cfg)
  systemPrompt <- TIO.readFile (acPromptFile cfg)
  let names = acNames cfg
      agentName = agentNameOf names
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      backend =
        acpBackend
          defaultAcpConfig {acpCommand = acAcpCommand cfg, acpWorkDir = acRoot cfg}
          acpTurnMicros
          framedPrompt
      mQuiesce = buildQuiesce (acQuiesce cfg) (acPitboss cfg)
  TIO.putStrLn $ "   protocol: acp (" <> T.pack (acAcpCommand cfg) <> ")"
  runSeat (Seat budget agentName names wakeFrom (acRoot cfg) mQuiesce backend (pure . renderMail) (pure []))

runLlm :: Int -> Maybe [Name] -> LlmConfig -> IO ()
runLlm budget wakeFrom cfg = do
  validatePrompt (lcPromptFile cfg)
  systemPrompt <- TIO.readFile (lcPromptFile cfg)
  key <- lookupEnv (lcKeyEnv cfg)
  case key of
    Nothing -> do
      TIO.hPutStrLn stderr ("🔴 " <> T.pack (lcKeyEnv cfg) <> " environment variable not set")
      exitFailure
    Just k -> do
      let names = lcNames cfg
          agentName = agentNameOf names
          bareCfg =
            defaultBareConfig
              { agentName = agentName,
                baseUrl = lcBaseUrl cfg,
                model = fromMaybe (model defaultBareConfig) (lcModel cfg),
                key = T.pack k
              }
          backend = directHttpBackend bareCfg systemPrompt
          mQuiesce = buildQuiesce (lcQuiesce cfg) (lcPitboss cfg)
      TIO.putStrLn $ "   base: " <> baseUrl bareCfg
      TIO.putStrLn $ "   model: " <> model bareCfg
      runSeat (Seat budget agentName names wakeFrom (lcRoot cfg) mQuiesce backend (pure . renderMail) (pure []))

runGateway :: Int -> Maybe [Name] -> GatewayCliConfig -> IO ()
runGateway budget wakeFrom cfg = do
  validatePrompt (gcwPromptFile cfg)
  systemPrompt <- TIO.readFile (gcwPromptFile cfg)
  home <- getHomeDirectory
  let names = gcwNames cfg
      agentName = agentNameOf names
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      gwCfg = defaultGatewayConfig {gwBaseUrl = gcwBaseUrl cfg, gwKeyEnv = gcwKeyEnv cfg, gwCwd = home </> "pit"}
      backend = gatewayBackend gwCfg framedPrompt
      mQuiesce = buildQuiesce (gcwQuiesce cfg) (gcwPitboss cfg)
  TIO.putStrLn $ "   gateway: " <> gwBaseUrl gwCfg
  runSeat (Seat budget agentName names wakeFrom (gcwRoot cfg) mQuiesce backend (pure . renderMail) (pure []))

runCommand :: Int -> Maybe [Name] -> CommandConfig -> IO ()
runCommand budget wakeFrom cfg = do
  let names = ccNames cfg
      agentName = agentNameOf names
      backend = commandBackend (ccCmd cfg) (ccArgs cfg)
      mQuiesce = buildQuiesce (ccQuiesce cfg) (ccPitboss cfg)
  TIO.putStrLn $ "   command: " <> T.pack (ccCmd cfg) <> " " <> T.intercalate " " (map T.pack (ccArgs cfg))
  case swallowedSeatOption (ccArgs cfg) of
    [] -> pure ()
    shadowed ->
      TIO.putStrLn $
        "⚠ "
          <> T.intercalate ", " (map T.pack shadowed)
          <> " came after `--`, so it is the command's argv and the seat never saw "
          <> (if length shadowed == 1 then "it." else "them.")
          <> " Put the seat's own flags before --cmd: `--quiesce 2 --cmd /bin/sh -- -c ...`"
  runSeat (Seat budget agentName names wakeFrom (ccRoot cfg) mQuiesce backend (pure . renderMail) (pure []))
