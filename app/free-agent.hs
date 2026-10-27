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

import Circuit.Agent (Name, Post (..), mkPost)
import Circuit.Agent.Framing (Stamped, stamp, stamped)
import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime)
import Free.Agent.Acp (AcpConfig (..), acpHost, acpInitialize, acpNewSession, closeAcp, defaultAcpConfig, openAcp)
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus.Cli (BusCommand, Depth (..), busParser, depthOpt, liveOpt, runBusCommand, runStatus)
import Free.Agent.Bus.File (QuiesceConfig (..))
import Free.Agent.Cli (cliDeliverTo, dshCli, kimiCli)
import Free.Agent.Cli.Config
  ( AcpCliConfig (..),
    Backend (..),
    CommandConfig (..),
    DshConfig (..),
    GatewayCliConfig (..),
    HermesConfig (..),
    KimiConfig (..),
    LlmConfig (..),
    parseAcpConfig,
    parseCommandConfig,
    parseDshConfig,
    parseHermesConfig,
    parseLlmConfig,
  )
import Free.Agent.Delivery (Invocation (..), Turn, TurnResult (..), deliverTo, mapTurn, newDeliveryGrant, replyAct)
import Free.Agent.Gateway
  ( GatewayClient (..),
    GatewayConfig (..),
    defaultGatewayConfig,
    gatewayHost,
    openGateway,
  )
import Free.Agent.Host (BareConfig (..), BodyMode (..), Host (..), bareHost, defaultBareConfig, hermesCli, hostResponse, processHost)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

-- ---------------------------------------------------------------------------
-- Top-level command type
-- ---------------------------------------------------------------------------

data Command
  = AgentCmd Int Backend
  | BusCmd BusCommand
  | StatusCmd FilePath (Maybe NominalDiffTime) Bool Depth
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

-- | Convert a parsed Hermes-style config into a Kimi config by dropping yolo.
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

-- | Convert a parsed Hermes-style config into a gateway config (api_server
-- defaults for base URL and key env var).
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

-- | Parse the default agent command. Defaults to Hermes; --backend switches.
defaultAgentParser :: Parser Command
defaultAgentParser =
  AgentCmd
    <$> deliveryBudgetOpt
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
    ( command "bus" (info (BusCmd <$> busParser <**> helper) (progDesc "Bus subcommands"))
        <> command "llm" (info (AgentCmd <$> deliveryBudgetOpt <*> (BackendLlm <$> parseLlmConfig) <**> helper) (progDesc "Direct API seat (old bus-bare)"))
        <> command "cmd" (info (AgentCmd <$> deliveryBudgetOpt <*> (BackendCommand <$> parseCommandConfig) <**> helper) (progDesc "External-command seat"))
        <> command "dsh" (info (AgentCmd <$> deliveryBudgetOpt <*> (BackendDsh <$> parseDshConfig) <**> helper) (progDesc "DSH / local ollama seat"))
        <> command "acp" (info (AgentCmd <$> deliveryBudgetOpt <*> (BackendAcp <$> parseAcpConfig) <**> helper) (progDesc "ACP seat (kimi acp — persistent repl)"))
        <> command "status" (info (StatusCmd <$> rootOpt <*> optional thresholdOpt <*> liveOpt <*> depthOpt (Depth 1) <**> helper) (progDesc "Bus log statistics"))
    )
    <|> defaultAgentParser
  where
    rootOpt =
      option
        str
        ( long "root"
            <> short 'r'
            <> metavar "ROOT"
            <> value "."
            <> showDefault
            <> help "Bus root directory"
        )
    thresholdOpt =
      option
        (eitherReader readSeconds)
        ( long "threshold"
            <> short 't'
            <> metavar "SECS"
            <> help "Seconds since last post for the bus to be considered live"
        )
      where
        readSeconds s =
          case reads s of
            [(n, "")] -> Right (fromInteger n :: NominalDiffTime)
            _ -> Left ("expected a whole number of seconds, got: " ++ s)

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Unified free-agent CLI"
        <> header "free-agent - bus, seats, and status"
    )

-- ---------------------------------------------------------------------------
-- Main dispatch
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cmd <- execParser opts
  case cmd of
    AgentCmd budget backend -> runBackend budget backend
    BusCmd busCmd -> runBusCommand busCmd
    StatusCmd root mthreshold live depth -> runStatus root (fromMaybe 900 mthreshold) live depth

-- ---------------------------------------------------------------------------
-- Backend dispatch
-- ---------------------------------------------------------------------------

runBackend :: Int -> Backend -> IO ()
runBackend budget (BackendHermes cfg) = runHermes budget cfg
runBackend budget (BackendKimi cfg) = runKimi budget cfg
runBackend budget (BackendDsh cfg) = runDsh budget cfg
runBackend budget (BackendAcp cfg) = runAcp budget cfg
runBackend budget (BackendLlm cfg) = runLlm budget cfg
runBackend budget (BackendCommand cfg) = runCommand budget cfg
runBackend budget (BackendGateway cfg) = runGateway budget cfg

-- ---------------------------------------------------------------------------
-- Hermes / Kimi helpers
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

-- | Parse a leading @name: prefix from a reply body.
routeReply :: Post Text -> Post Text
routeReply p =
  case T.stripPrefix "@" (body p) of
    Nothing -> p
    Just rest ->
      let (name, afterName) = T.break (== ':') rest
          name' = T.strip name
       in if T.null name' || T.null afterName
            then p
            else p {to = [name'], body = T.strip (T.drop 1 afterName)}

-- | Decorate an incoming post body with its sender.
decorateSender :: Stamped Text -> Stamped Text
decorateSender stored =
  let p = stamped stored
   in stored {stamped = p {body = from p <> ": " <> body p}}

runSeat :: Int -> Text -> [Name] -> FilePath -> Maybe QuiesceConfig -> (Stamped Text -> IO (Turn [Post Text])) -> IO ()
runSeat budget agentName names root mQuiesce handle = do
  grant <- newDeliveryGrant budget
  runAgentLoop agentName names root mQuiesce grant handle

hostTurn :: Host -> Stamped Text -> IO (Turn [Post Text])
hostTurn host =
  deliverTo
    ( \cause _ ->
        pure
          Invocation
            { runInvocation = TurnResponse <$> hostResponse host (stamped cause),
              cancelInvocation = pure False
            }
    )
    <*> pure ""

defaultReply :: Text -> Stamped Text -> Text -> [Post Text]
defaultReply agentName stored response =
  let cause = stamped stored
   in [routeReply ((mkPost agentName [from cause] response) {channel = channel cause})]

runHermes :: Int -> HermesConfig -> IO ()
runHermes budget cfg = do
  validatePrompt (hcPromptFile cfg)
  systemPrompt <- TIO.readFile (hcPromptFile cfg)
  let names = hcNames cfg
      agentName = agentNameOf names
      sessionFile = fromMaybe (defaultSessionFile (hcRoot cfg) names) (hcSessionFile cfg)
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      transcriptPath = hcRoot cfg </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      cli = hermesCli (hcModel cfg) (hcProvider cfg) (hcYolo cfg) sessionFile
      mQuiesce = buildQuiesce (hcQuiesce cfg) (hcPitboss cfg)
      handle stored =
        let delivered = decorateSender stored
            prompt = framedPrompt <> "\n\nUser messages:\n" <> body (stamped delivered)
         in mapTurn (replyAct agentName stored)
              <$> cliDeliverTo cli transcriptPath (snd (stamp stored)) prompt
  createDirectoryIfMissing True (takeDirectory sessionFile)
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runSeat budget agentName names (hcRoot cfg) mQuiesce handle

runKimi :: Int -> KimiConfig -> IO ()
runKimi budget cfg = do
  validatePrompt (kcPromptFile cfg)
  systemPrompt <- TIO.readFile (kcPromptFile cfg)
  let names = kcNames cfg
      agentName = agentNameOf names
      sessionFile = fromMaybe (defaultSessionFile (kcRoot cfg) names) (kcSessionFile cfg)
      transcriptPath = kcRoot cfg </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      cli = kimiCli (kcModel cfg) (kcProvider cfg) sessionFile
      mQuiesce = buildQuiesce (kcQuiesce cfg) (kcPitboss cfg)
      handle stored =
        let delivered = decorateSender stored
            prompt = systemPrompt <> "\n\nUser message:\n" <> body (stamped delivered)
         in mapTurn (defaultReply agentName stored)
              <$> cliDeliverTo cli transcriptPath (snd (stamp stored)) prompt
  createDirectoryIfMissing True (takeDirectory sessionFile)
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runSeat budget agentName names (kcRoot cfg) mQuiesce handle

-- ---------------------------------------------------------------------------
-- DSH / local ollama seat
-- ---------------------------------------------------------------------------

runDsh :: Int -> DshConfig -> IO ()
runDsh budget cfg = do
  validatePrompt (dcPromptFile cfg)
  systemPrompt <- TIO.readFile (dcPromptFile cfg)
  let names = dcNames cfg
      agentName = agentNameOf names
      transcriptPath = dcRoot cfg </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      sessionPath = dcRoot cfg </> ".transcripts" </> T.unpack agentName <> ".sid"
      cli = dshCli (Just systemPrompt) (dcDshPatch cfg) (dcDshModelPatch cfg) sessionPath
      mQuiesce = buildQuiesce (dcQuiesce cfg) (dcPitboss cfg)
      handle stored =
        let delivered = decorateSender stored
         in mapTurn (replyAct agentName stored)
              <$> cliDeliverTo cli transcriptPath (snd (stamp stored)) (body (stamped delivered))
  createDirectoryIfMissing True (takeDirectory transcriptPath)
  TIO.putStrLn $ "   patch: " <> T.pack (dcDshPatch cfg)
  forM_ (dcDshModelPatch cfg) $ \p ->
    TIO.putStrLn $ "   model patch: " <> T.pack p
  runSeat budget agentName names (dcRoot cfg) mQuiesce handle

-- ---------------------------------------------------------------------------
-- ACP seat (persistent repl)
-- ---------------------------------------------------------------------------

-- | One prompt turn against a live ACP child.
acpTurnMicros :: Int
acpTurnMicros = 600_000_000

runAcp :: Int -> AcpCliConfig -> IO ()
runAcp budget cfg = do
  validatePrompt (acPromptFile cfg)
  systemPrompt <- TIO.readFile (acPromptFile cfg)
  let names = acNames cfg
      agentName = agentNameOf names
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      mQuiesce = buildQuiesce (acQuiesce cfg) (acPitboss cfg)
  client <-
    openAcp
      defaultAcpConfig
        { acpCommand = acAcpCommand cfg,
          acpWorkDir = acRoot cfg
        }
  flip finally (closeAcp client) $ do
    _ <- acpInitialize client acpTurnMicros
    (msid, _) <- acpNewSession client acpTurnMicros (acRoot cfg)
    sid <- case msid of
      Just s -> pure s
      Nothing -> do
        TIO.hPutStrLn stderr "🔴 acp: session/new returned no session id"
        exitFailure
    let host = acpHost agentName acpTurnMicros framedPrompt client sid
        handle = fmap (mapTurn (map routeReply)) . hostTurn host . decorateSender
    TIO.putStrLn $ "   protocol: acp (" <> T.pack (acAcpCommand cfg) <> ")"
    TIO.putStrLn $ "   session: " <> sid
    runSeat budget agentName names (acRoot cfg) mQuiesce handle

-- ---------------------------------------------------------------------------
-- Direct API (llm)
-- ---------------------------------------------------------------------------

runLlm :: Int -> LlmConfig -> IO ()
runLlm budget cfg = do
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
          host = bareHost bareCfg systemPrompt
          mQuiesce = buildQuiesce (lcQuiesce cfg) (lcPitboss cfg)
      TIO.putStrLn $ "   base: " <> baseUrl bareCfg
      TIO.putStrLn $ "   model: " <> model bareCfg
      runSeat budget agentName names (lcRoot cfg) mQuiesce (hostTurn host)

-- ---------------------------------------------------------------------------
-- Gateway seat (hermes api_server)
-- ---------------------------------------------------------------------------

runGateway :: Int -> GatewayCliConfig -> IO ()
runGateway budget cfg = do
  validatePrompt (gcwPromptFile cfg)
  systemPrompt <- TIO.readFile (gcwPromptFile cfg)
  let names = gcwNames cfg
      agentName = agentNameOf names
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      gwCfg = defaultGatewayConfig {gwBaseUrl = gcwBaseUrl cfg, gwKeyEnv = gcwKeyEnv cfg}
  client <- openGateway gwCfg
  let host = gatewayHost agentName framedPrompt client
      mQuiesce = buildQuiesce (gcwQuiesce cfg) (gcwPitboss cfg)
      handle = fmap (mapTurn (map routeReply)) . hostTurn host
  TIO.putStrLn $ "   gateway: " <> gwBaseUrl gwCfg
  TIO.putStrLn $ "   session: " <> gcSessionId client
  runSeat budget agentName names (gcwRoot cfg) mQuiesce handle

-- ---------------------------------------------------------------------------
-- External command seat
-- ---------------------------------------------------------------------------

runCommand :: Int -> CommandConfig -> IO ()
runCommand budget cfg = do
  let names = ccNames cfg
      agentName = agentNameOf names
      host = (processHost agentName (ccCmd cfg) (ccArgs cfg)) {hostBodyMode = BodyWhole}
      mQuiesce = buildQuiesce (ccQuiesce cfg) (ccPitboss cfg)
  TIO.putStrLn $ "   command: " <> T.pack (ccCmd cfg) <> " " <> T.intercalate " " (map T.pack (ccArgs cfg))
  runSeat budget agentName names (ccRoot cfg) mQuiesce (hostTurn host)
