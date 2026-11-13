{-# LANGUAGE OverloadedStrings #-}

-- | CLI configuration types and parsers for the unified @free-agent@ executable.
module Free.Agent.Cli.Config
  ( Backend (..),
    HermesConfig (..),
    KimiConfig (..),
    DshConfig (..),
    LlmConfig (..),
    CommandConfig (..),
    GatewayCliConfig (..),
    AcpCliConfig (..),
    defaultHermesConfig,
    defaultKimiConfig,
    defaultDshConfig,
    defaultLlmConfig,
    defaultCommandConfig,
    parseHermesConfig,
    parseKimiConfig,
    parseDshConfig,
    parseLlmConfig,
    parseCommandConfig,
    swallowedSeatOption,
    parseGatewayConfig,
    parseAcpConfig,
    commonAgentOptions,
    rootOpt,
    namesOpt,
    promptOpt,
    sessionOpt,
    modelOpt,
    providerOpt,
    reasoningOpt,
    quiesceOpt,
    pitbossOpt,
  )
where

import Circuit.Agent (Name)
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative

-- ---------------------------------------------------------------------------
-- Backend sum type
-- ---------------------------------------------------------------------------

data Backend
  = BackendHermes HermesConfig
  | BackendKimi KimiConfig
  | BackendDsh DshConfig
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

-- | ACP seat configuration: a persistent 'Circuit.Agent.StdPorts' child
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

defaultHermesConfig :: HermesConfig
defaultHermesConfig =
  HermesConfig
    { hcRoot = "bus/",
      hcNames = [],
      hcPromptFile = "",
      hcSessionFile = Nothing,
      hcModel = Nothing,
      hcProvider = Nothing,
      hcReasoning = Nothing,
      hcYolo = True,
      hcQuiesce = Nothing,
      hcPitboss = Nothing
    }

defaultKimiConfig :: KimiConfig
defaultKimiConfig =
  KimiConfig
    { kcRoot = "bus/",
      kcNames = [],
      kcPromptFile = "",
      kcSessionFile = Nothing,
      kcModel = Nothing,
      kcProvider = Nothing,
      kcQuiesce = Nothing,
      kcPitboss = Nothing
    }

defaultDshConfig :: DshConfig
defaultDshConfig =
  DshConfig
    { dcRoot = "bus/",
      dcNames = [],
      dcPromptFile = "",
      dcDshPatch = "",
      dcDshModelPatch = Nothing,
      dcQuiesce = Nothing,
      dcPitboss = Nothing
    }

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

defaultCommandConfig :: CommandConfig
defaultCommandConfig =
  CommandConfig
    { ccRoot = "bus/",
      ccNames = [],
      ccCmd = "",
      ccArgs = [],
      ccQuiesce = Nothing,
      ccPitboss = Nothing
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

-- | Options shared by Hermes and Kimi configs.
commonAgentOptions ::
  Parser FilePath ->
  Parser (Maybe FilePath) ->
  Parser (Maybe Int) ->
  Parser (Maybe Name) ->
  Parser (FilePath, [Name], FilePath, Maybe FilePath, Maybe Text, Maybe Text, Maybe Int, Maybe Name)
commonAgentOptions rootP sessP quiesceP pitbossP =
  (,,,,,,,)
    <$> rootP
    <*> namesOpt
    <*> promptOpt
    <*> sessP
    <*> modelOpt
    <*> providerOpt
    <*> quiesceP
    <*> pitbossP

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

parseKimiConfig :: Parser KimiConfig
parseKimiConfig =
  KimiConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
    <*> sessionOpt
    <*> modelOpt
    <*> providerOpt
    <*> quiesceOpt
    <*> pitbossOpt

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
        "--pitboss"
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

parseGatewayConfig :: Parser GatewayCliConfig
parseGatewayConfig =
  GatewayCliConfig
    <$> rootOpt
    <*> namesOpt
    <*> promptOpt
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
            <> value "http://127.0.0.1:8642"
            <> showDefault
            <> help "Hermes gateway api_server base URL"
        )
    keyEnvOpt =
      strOption
        ( long "key-env"
            <> metavar "NAME"
            <> value "API_SERVER_KEY"
            <> showDefault
            <> help "Environment variable holding the api_server Bearer key"
        )
