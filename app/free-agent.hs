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
import Circuit.Agent.Mark (Mark (StandDown), markGlyph)
import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Data.Char (isAlphaNum)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Free.Agent.Acp (AcpConfig (..), acpHost, acpInitialize, acpNewSession, closeAcp, defaultAcpConfig, openAcp)
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus.Cli (BusCommand, Depth (..), busParser, depthOpt, orgOpt, runBusCommand, runStatus)
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
    swallowedSeatOption,
  )
import Free.Agent.Delivery (Invocation (..), Turn, TurnResult (..), deliverTo, mapTurnIO, newDeliveryGrant, noteStrand, replyActIO, strandBlock, takeStrandNotes)
import Free.Agent.Gateway
  ( GatewayClient (..),
    GatewayConfig (..),
    defaultGatewayConfig,
    gatewayHost,
    openGateway,
  )
import Free.Agent.Host (BareConfig (..), BodyMode (..), Host (..), bareHost, defaultBareConfig, hermesCli, processHost)
import Free.Agent.Refresh (preserveForRefresh, refreshDue, refreshRequestPath, writeRefreshRequest)
import Free.Agent.Usage (readHermesUsage, readUsageIdentity, renderUsageIdentity)
import Options.Applicative
import Paths_free_agent (version)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, makeAbsolute)
import System.Environment (getExecutablePath, lookupEnv, setEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Process (spawnProcess, waitForProcess)

-- ---------------------------------------------------------------------------
-- Top-level command type
-- ---------------------------------------------------------------------------

data Command
  = AgentCmd Int Backend
  | BusCmd BusCommand
  | StatusCmd FilePath (Maybe NominalDiffTime) Depth Bool
  | UsageCmd HermesUsageSource
  | RefreshCmd Int Int HermesConfig
  | RefreshChildCmd Int Int (Maybe FilePath) HermesConfig
  deriving (Show)

-- | Existing session-file measurement or an explicit one-shot usage report.
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
              (RefreshCmd <$> deliveryBudgetOpt <*> refreshTokensOpt <*> parseHermesConfig <**> helper)
              (progDesc "Supervise a Hermes seat with automatic same-name session refresh")
          )
        <> command
          "refresh-child"
          ( info
              (RefreshChildCmd <$> deliveryBudgetOpt <*> refreshTokensOpt <*> rebootNoteOpt <*> parseHermesConfig <**> helper)
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
        readSeconds s =
          case reads s of
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
    )

-- | The build witness. A verdict is worth what the tool that produced it can
-- name about itself, so @--version@ answers with the package version the
-- binary was linked from and exits 0 without touching a bus.
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
  hSetBuffering stdout LineBuffering
  cmd <- execParser opts
  case cmd of
    AgentCmd budget backend -> runBackend budget backend
    BusCmd busCmd -> runBusCommand busCmd
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
    RefreshCmd budget threshold cfg -> runRefreshSupervisor budget threshold cfg
    RefreshChildCmd budget threshold note cfg -> runHermesAuto (Just threshold) note budget cfg

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

-- | Decorate incoming mail with the ids available for deliberate citation and
-- its senders. A batch is rendered without dropping or reordering any post.
decoratePost :: Stamped Text -> Stamped Text
decoratePost stored =
  let p = stamped stored
      pid = T.pack (show (snd (stamp stored)))
   in stored {stamped = p {body = "[post " <> pid <> "] " <> from p <> ": " <> body p}}

renderMail :: [Stamped Text] -> Text
renderMail = T.intercalate "\n\n" . map (body . stamped . decoratePost)

batchBoundary :: [Stamped Text] -> Stamped Text
batchBoundary = last

runSeat :: Int -> Text -> [Name] -> FilePath -> Maybe QuiesceConfig -> ([Stamped Text] -> IO (Turn [Post Text])) -> IO ()
runSeat budget agentName names root mQuiesce handle = do
  -- The launch root is the single source for delivery, cursor, publish, and
  -- heartbeat. Export it absolutely so every child process — the CLI backend
  -- and any `free-agent bus` subprocess the model spawns — resolves its
  -- default root to the launch root instead of its own cwd. Without this, a
  -- turn's default-root post escapes to whatever bus sits in the child's cwd
  -- (B11): the stale alternate root receives the DONE. `--root` stays a human
  -- CLI tool; the seat's default root is authoritative.
  absRoot <- makeAbsolute root
  setEnv "FREE_AGENT_BUS_ROOT" absRoot
  grant <- newDeliveryGrant budget
  runAgentLoop agentName names root mQuiesce grant handle

-- | A host seat's turn: the publication rule the model handlers use, and the
-- same stranded-reply note. The note is a harness fact for the next delivery,
-- so a surface whose next turn cannot read prose (a script) still carries it
-- rather than needing a second rule about which seats are told.
-- | A turn that published nothing but carries an @name:@ somewhere that is
-- not a line start has lost its address rather than its intent: the parser
-- reads a non-leading address as an inline mention, so the reply is dropped
-- whole and silently. The hint is printed only when the turn published
-- nothing, which is the one case where a seat has no other signal and where a
-- warning cannot add noise to a productive turn.
midLineAddress :: Text -> Maybe (Int, Text)
midLineAddress t = go 0 t
  where
    go off s =
      let (pre, rest) = T.breakOn "@" s
       in if T.null rest
            then Nothing
            else
              let after = T.drop 1 rest
                  (nm, colon) = T.breakOn ":" after
                  named =
                    not (T.null nm)
                      && T.length nm <= 32
                      && T.all (\c -> isAlphaNum c || c == '-' || c == '_') nm
               in if named && not (T.null colon) && not (T.null pre)
                    then Just (off + T.length pre, nm)
                    else go (off + T.length pre + 1) after

hostTurn :: FilePath -> Host -> [Stamped Text] -> IO (Turn [Post Text])
hostTurn root host storedBatch = do
  notes <- takeStrandNotes root (hostName host)
  deliverTo
    ( \_ input ->
        pure
          Invocation
            { runInvocation = do
                outputs <- hostRun host [input]
                mapM_ (noteStrand root (hostName host) (batchBoundary storedBatch)) outputs
                posts <- concat <$> mapM (replyActIO (hostName host)) outputs
                case posts of
                  [] -> case foldr (\o acc -> acc <|> midLineAddress o) Nothing outputs of
                    Just (off, nm) ->
                      TIO.hPutStrLn stderr $
                        "⚠ "
                          <> hostName host
                          <> ": nothing published — @"
                          <> nm
                          <> ": appears at character offset "
                          <> T.pack (show off)
                          <> ", not at the start of a line, so it read as a mention. Put the address first, or on its own line."
                    Nothing -> pure ()
                  _ -> pure ()
                pure (TurnResponse posts),
              cancelInvocation = pure False
            }
    )
    storedBatch
    (strandBlock notes <> (if null notes then "" else "\n\n") <> renderMail storedBatch)

runHermes :: Int -> HermesConfig -> IO ()
runHermes = runHermesAuto Nothing Nothing

runHermesAuto :: Maybe Int -> Maybe FilePath -> Int -> HermesConfig -> IO ()
runHermesAuto threshold rebootFile budget cfg = do
  validatePrompt (hcPromptFile cfg)
  systemPrompt <- TIO.readFile (hcPromptFile cfg)
  rebootNote <- maybe (pure "") TIO.readFile rebootFile
  firstDelivery <- newIORef True
  let names = hcNames cfg
      agentName = agentNameOf names
      root = hcRoot cfg
      sessionFile = fromMaybe (defaultSessionFile root names) (hcSessionFile cfg)
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      transcriptPath = root </> ".transcripts" </> T.unpack agentName <> ".jsonl"
      cli = hermesCli (hcModel cfg) (hcProvider cfg) (hcReasoning cfg) (hcYolo cfg) sessionFile
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
                      -- Control form: the glyph alone on the first line
                      -- stands the seat down for the refresh; the reason
                      -- rides on the line below it. A batch has no single
                      -- sender or channel to inherit for this harness act.
                      [ mkPost
                          agentName
                          []
                          (markGlyph StandDown <> "\nautomatic refresh at " <> T.pack (show tokens) <> " prompt tokens")
                      ]
      handle storedBatch = do
        isFirst <- readIORef firstDelivery
        writeIORef firstDelivery False
        strandNotes <- takeStrandNotes root agentName
        let boundary = batchBoundary storedBatch
            note = if isFirst then rebootNote else ""
            prompt = framedPrompt <> note <> strandBlock strandNotes <> "\n\nUser messages:\n" <> renderMail storedBatch
        turn <- cliDeliverTo cli transcriptPath (snd (stamp boundary)) prompt
        pure $
          mapTurnIO
            ( \response -> do
                posts <- replyActIO agentName response
                noteStrand root agentName boundary response
                refreshPosts <- automaticRefresh
                pure (posts <> refreshPosts)
            )
            turn
  createDirectoryIfMissing True (takeDirectory sessionFile)
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runSeat budget agentName names root mQuiesce handle
  requested <- doesFileExist (refreshRequestPath root agentName)
  if isJust threshold && requested then exitWith (ExitFailure 75) else pure ()

runRefreshSupervisor :: Int -> Int -> HermesConfig -> IO ()
runRefreshSupervisor budget threshold cfg = supervise Nothing []
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
      handle storedBatch = do
        strandNotes <- takeStrandNotes (kcRoot cfg) agentName
        let boundary = batchBoundary storedBatch
            prompt = systemPrompt <> strandBlock strandNotes <> "\n\nUser messages:\n" <> renderMail storedBatch
        mapTurnIO
          ( \response -> do
              posts <- replyActIO agentName response
              noteStrand (kcRoot cfg) agentName boundary response
              pure posts
          )
          <$> cliDeliverTo cli transcriptPath (snd (stamp boundary)) prompt
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
      handle storedBatch = do
        strandNotes <- takeStrandNotes (dcRoot cfg) agentName
        let boundary = batchBoundary storedBatch
            block = strandBlock strandNotes
            message = renderMail storedBatch
            prompt = if T.null block then message else block <> "\n\n" <> message
        mapTurnIO
          ( \response -> do
              posts <- replyActIO agentName response
              noteStrand (dcRoot cfg) agentName boundary response
              pure posts
          )
          <$> cliDeliverTo cli transcriptPath (snd (stamp boundary)) prompt
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
        handle = hostTurn (acRoot cfg) host
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
      runSeat budget agentName names (lcRoot cfg) mQuiesce (hostTurn (lcRoot cfg) host)

-- ---------------------------------------------------------------------------
-- Gateway seat (hermes api_server)
-- ---------------------------------------------------------------------------

runGateway :: Int -> GatewayCliConfig -> IO ()
runGateway budget cfg = do
  validatePrompt (gcwPromptFile cfg)
  systemPrompt <- TIO.readFile (gcwPromptFile cfg)
  home <- getHomeDirectory
  let names = gcwNames cfg
      agentName = agentNameOf names
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ".\n\n" <> systemPrompt
      gwCfg = defaultGatewayConfig {gwBaseUrl = gcwBaseUrl cfg, gwKeyEnv = gcwKeyEnv cfg, gwCwd = home </> "pit"}
  client <- openGateway gwCfg
  let host = gatewayHost agentName framedPrompt client
      mQuiesce = buildQuiesce (gcwQuiesce cfg) (gcwPitboss cfg)
      handle = hostTurn (gcwRoot cfg) host
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
  case swallowedSeatOption (ccArgs cfg) of
    [] -> pure ()
    shadowed ->
      -- the warning shares the banner's stream: a reader under `> out 2> err`
      -- otherwise gets the explanation and not the line it explains
      TIO.putStrLn $
        "⚠ "
          <> T.intercalate ", " (map T.pack shadowed)
          <> " came after `--`, so it is the command's argv and the seat never saw "
          <> (if length shadowed == 1 then "it." else "them.")
          <> " Put the seat's own flags before --cmd: `--quiesce 2 --cmd /bin/sh -- -c ...`"

  runSeat budget agentName names (ccRoot cfg) mQuiesce (hostTurn (ccRoot cfg) host)
