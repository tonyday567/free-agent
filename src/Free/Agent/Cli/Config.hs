{-# LANGUAGE OverloadedStrings #-}

-- | CLI configuration types and parsers for the unified @free-agent@ executable.
module Free.Agent.Cli.Config
  ( Backend (..),
    HermesConfig (..),
    KimiConfig (..),
    DshConfig (..),
    DshAcpConfig (..),
    LlmConfig (..),
    CommandConfig (..),
    GatewayCliConfig (..),
    AcpCliConfig (..),
    defaultLlmConfig,
    parseHermesConfig,
    parseDshConfig,
    parseDshAcpConfig,
    parseLlmConfig,
    parseCommandConfig,
    swallowedSeatOption,
    parseAcpConfig,
    rootOpt,
    namesOpt,
    promptOpt,
    sessionOpt,
    modelOpt,
    providerOpt,
    reasoningOpt,
    quiesceOpt,
    pitbossOpt,
    wakeFromOpt,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Agent (Name)
import Options.Applicative

-- ---------------------------------------------------------------------------
-- Backend sum type
-- ---------------------------------------------------------------------------

data Backend
  = BackendHermes HermesConfig
  | BackendKimi KimiConfig
  | BackendDsh DshConfig
  | BackendDshAcp DshAcpConfig
  | BackendLlm LlmConfig
  | BackendCommand CommandConfig
  | BackendGateway GatewayCliConfig
  | BackendAcp AcpCliConfig
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Config records
-- ---------------------------------------------------------------------------

data HermesConfig = HermesConfig
  { hcRoot :: FilePath,
    hcNames :: [Name],
    hcPromptFile :: FilePath,
    hcSessionFile :: Maybe FilePath,
    hcModel :: Maybe Text,
    hcProvider :: Maybe Text,
    hcReasoning :: Maybe Text,
    hcYolo :: Bool,
    hcQuiesce :: Maybe Int,
    hcPitboss :: Maybe Name
  }
  deriving (Show)

data KimiConfig = KimiConfig
  { kcRoot :: FilePath,
    kcNames :: [Name],
    kcPromptFile :: FilePath,
    kcSessionFile :: Maybe FilePath,
    kcModel :: Maybe Text,
    kcProvider :: Maybe Text,
    kcQuiesce :: Maybe Int,
    kcPitboss :: Maybe Name
  }
  deriving (Show)

data DshConfig = DshConfig
  { dcRoot :: FilePath,
    dcNames :: [Name],
    dcPromptFile :: FilePath,
    dcDshPatch :: FilePath,
    dcDshModelPatch :: Maybe FilePath,
    dcQuiesce :: Maybe Int,
    dcPitboss :: Maybe Name
  }
  deriving (Show)

-- | Persistent dsh ACP seat configuration. Bus root, execution workspace,
-- and dsh session store are deliberately independent and explicit.
data DshAcpConfig = DshAcpConfig
  { dacRoot :: FilePath,
    dacNames :: [Name],
    dacPromptFile :: FilePath,
    dacDshPatch :: FilePath,
    dacDshModelPatch :: Maybe FilePath,
    dacDshHome :: FilePath,
    dacWorkspace :: FilePath,
    dacQuiesce :: Maybe Int,
    dacPitboss :: Maybe Name
  }
  deriving (Show)

data LlmConfig = LlmConfig
  { lcRoot :: FilePath,
    lcNames :: [Name],
    lcPromptFile :: FilePath,
    lcModel :: Maybe Text,
    lcBaseUrl :: Text,
    lcKeyEnv :: String,
    lcQuiesce :: Maybe Int,
    lcPitboss :: Maybe Name
  }
  deriving (Show)

data CommandConfig = CommandConfig
  { ccRoot :: FilePath,
    ccNames :: [Name],
    ccCmd :: FilePath,
    ccArgs :: [String],
    ccQuiesce :: Maybe Int,
    ccPitboss :: Maybe Name
  }
  deriving (Show)

data GatewayCliConfig = GatewayCliConfig
  { gcwRoot :: FilePath,
    gcwNames :: [Name],
    gcwPromptFile :: FilePath,
    gcwBaseUrl :: Text,
    gcwKeyEnv :: String,
    gcwQuiesce :: Maybe Int,
    gcwPitboss :: Maybe Name
  }
  deriving (Show)

-- | ACP seat configuration: a persistent 'Free.Agent.StdPorts' child
-- speaking JSON-RPC over stdio (see "Free.Agent.Acp").
data AcpCliConfig = AcpCliConfig
  { acRoot :: FilePath,
    acNames :: [Name],
    acPromptFile :: FilePath,
    -- | The ACP executable (default @kimi@; invoked as @kimi acp@).
    acAcpCommand :: FilePath,
    acQuiesce :: Maybe Int,
    acPitboss :: Maybe Name
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Defaults
-- ---------------------------------------------------------------------------

defaultLlmConfig :: LlmConfig
defaultLlmConfig =
  LlmConfig
    { lcRoot = "bus/",
      lcNames = [],
      lcPromptFile = "",
      lcModel = Nothing,
      lcBaseUrl = "https://api.deepseek.com/v1",
      lcKeyEnv = "DEEPSEEK_API_KEY",
      lcQuiesce = Nothing,
      lcPitboss = Nothing
    }

-- ---------------------------------------------------------------------------
-- Common option parsers
-- ---------------------------------------------------------------------------

rootOpt :: Parser FilePath
rootOpt =
  strOption
    ( long "root"
        <> short 'r'
        <> metavar "ROOT"
        <> value "bus/"
        <> showDefault
        <> help "Bus root directory containing log.jsonl"
    )

namesOpt :: Parser [Name]
namesOpt =
  some
    ( option
        (T.pack <$> str)
        ( long "name"
            <> short 'n'
            <> metavar "NAME"
            <> help "Subscriber name (repeatable)"
        )
    )

promptOpt :: Parser FilePath
promptOpt =
  strOption
    ( long "prompt"
        <> short 'p'
        <> metavar "PROMPT.md"
        <> help "System prompt markdown file"
    )

sessionOpt :: Parser (Maybe FilePath)
sessionOpt =
  optional $
    strOption
      ( long "session"
          <> short 's'
          <> metavar "FILE"
          <> help "Session file (default: ROOT/.sessions/<name>.sid)"
      )

modelOpt :: Parser (Maybe Text)
modelOpt =
  optional $
    option
      (T.pack <$> str)
      ( long "model"
          <> short 'm'
          <> metavar "MODEL"
          <> help "Model passed to the backend"
      )

providerOpt :: Parser (Maybe Text)
providerOpt =
  optional $
    option
      (T.pack <$> str)
      ( long "provider"
          <> metavar "PROVIDER"
          <> help "Provider passed to the backend"
      )

-- | Hermes only: the reasoning level the seat runs at (low, high, xhigh, max,
-- ultra). Absent, the seat inherits @agent.reasoning_effort@ from its hermes
-- config. Forwarded verbatim as @--reasoning@; this CLI does not validate the set.
reasoningOpt :: Parser (Maybe Text)
reasoningOpt =
  optional $
    option
      (T.pack <$> str)
      ( long "reasoning"
          <> metavar "LEVEL"
          <> help "Hermes reasoning level (low|high|xhigh|max|ultra)"
      )

quiesceOpt :: Parser (Maybe Int)
quiesceOpt =
  optional $
    option
      auto
      ( long "quiesce"
          <> metavar "N"
          <> help "Exit after N empty one-second cycles"
      )

pitbossOpt :: Parser (Maybe Name)
pitbossOpt =
  optional $
    option
      (T.pack <$> str)
      ( long "pitboss"
          <> metavar "NAME"
          <> help "Recipient for the quiescence marker (default: pitboss)"
      )

-- | Restrict wake permission to posts from these senders. Absent means allow
-- every sender, preserving the existing seat behaviour. Admission of a
-- quarantined post is a separate explicit act through @bus admit@.
wakeFromOpt :: Parser (Maybe [Name])
wakeFromOpt =
  optional $
    some $
      option
        (T.pack <$> str)
        ( long "wake-from"
            <> metavar "NAME"
            <> help "Allow wakes from NAME (repeatable; default: all senders)"
        )

-- ---------------------------------------------------------------------------
-- Backend-specific parsers
-- ---------------------------------------------------------------------------

parseHermesConfig :: Parser HermesConfig
parseHermesConfig =
  HermesConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
    <*> sessionOpt
    <*> modelOpt
    <*> providerOpt
    <*> reasoningOpt
    <*> yoloOpt
    <*> quiesceOpt
    <*> pitbossOpt
  where
    yoloOpt = not <$> switch (long "no-yolo" <> help "Do not pass --yolo to hermes")

parseDshConfig :: Parser DshConfig
parseDshConfig =
  DshConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
    <*> dshPatchOpt
    <*> optional dshModelPatchOpt
    <*> quiesceOpt
    <*> pitbossOpt
  where
    dshPatchOpt =
      strOption
        ( long "dsh-patch"
            <> metavar "PATCH.yml"
            <> help "DSH base patch file"
        )
    dshModelPatchOpt =
      strOption
        ( long "dsh-model-patch"
            <> metavar "MODEL.yml"
            <> help "DSH model override patch file"
        )

parseDshAcpConfig :: Parser DshAcpConfig
parseDshAcpConfig =
  DshAcpConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
    <*> dshPatchOpt
    <*> optional dshModelPatchOpt
    <*> dshHomeOpt
    <*> workspaceOpt
    <*> quiesceOpt
    <*> pitbossOpt
  where
    dshPatchOpt =
      strOption
        ( long "dsh-patch"
            <> metavar "PATCH.yml"
            <> help "DSH base patch file"
        )
    dshModelPatchOpt =
      strOption
        ( long "dsh-model-patch"
            <> metavar "MODEL.yml"
            <> help "DSH model override patch file"
        )
    dshHomeOpt =
      strOption
        ( long "dsh-home"
            <> metavar "DIR"
            <> help "Isolated DSH_HOME for this seat"
        )
    workspaceOpt =
      strOption
        ( long "workspace"
            <> metavar "DIR"
            <> help "Execution workspace, separate from the bus root"
        )

parseLlmConfig :: Parser LlmConfig
parseLlmConfig =
  LlmConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
    <*> modelOpt
    <*> baseUrlOpt
    <*> keyEnvOpt
    <*> quiesceOpt
    <*> pitbossOpt
  where
    baseUrlOpt =
      option
        (T.pack <$> str)
        ( long "base-url"
            <> metavar "URL"
            <> value (lcBaseUrl defaultLlmConfig)
            <> showDefault
            <> help "OpenAI-compatible API base URL"
        )
    keyEnvOpt =
      strOption
        ( long "key-env"
            <> metavar "NAME"
            <> value (lcKeyEnv defaultLlmConfig)
            <> showDefault
            <> help "Environment variable holding the API key"
        )

parseCommandConfig :: Parser CommandConfig
parseCommandConfig =
  CommandConfig
    <$> rootOpt
    <*> namesOpt
    <*> cmdOpt
    <*> argsOpt
    <*> quiesceOpt
    <*> pitbossOpt
  where
    cmdOpt =
      strOption
        ( long "cmd"
            <> metavar "CMD"
            <> help "External command to invoke per post"
        )
    argsOpt =
      many
        ( argument
            str
            (metavar "ARG..." <> help "Arguments passed to CMD")
        )

-- | Seat options that lose their meaning when they are written after @--@.
--
-- Everything after @--@ is the command's argv, so a seat flag placed there is
-- handed to the command and never reaches the parser. The result is a seat
-- that runs without the flag it appears to carry, and the only trace is the
-- banner printing the resolved argv.
--
-- >>> swallowedSeatOption ["ack", "--quiesce", "2"]
-- ["--quiesce"]
-- >>> swallowedSeatOption ["--wake-from", "runner"]
-- ["--wake-from"]
-- >>> swallowedSeatOption ["-c", "echo hi"]
-- []
-- >>> swallowedSeatOption ("--root" : [])
-- ["--root"]
swallowedSeatOption :: [String] -> [String]
swallowedSeatOption = filter (`elem` seatFlags)
  where
    seatFlags =
      [ "-r",
        "--root",
        "-n",
        "--name",
        "--cmd",
        "--delivery-budget",
        "--quiesce",
        "--pitboss",
        "--wake-from"
      ]

parseAcpConfig :: Parser AcpCliConfig
parseAcpConfig =
  AcpCliConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
    <*> acpCmdOpt
    <*> quiesceOpt
    <*> pitbossOpt
  where
    acpCmdOpt =
      strOption
        ( long "acp-cmd"
            <> metavar "CMD"
            <> value "kimi"
            <> showDefault
            <> help "ACP executable (run as CMD acp)"
        )
