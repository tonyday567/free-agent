{-# LANGUAGE OverloadedStrings #-}

-- | CLI configuration types and parsers for the unified @free-agent@ executable.
module Free.Agent.Cli.Config
  ( Backend (..),
    HermesConfig (..),
    KimiConfig (..),
    LlmConfig (..),
    CommandConfig (..),
    defaultHermesConfig,
    defaultKimiConfig,
    defaultLlmConfig,
    defaultCommandConfig,
    parseHermesConfig,
    parseKimiConfig,
    parseLlmConfig,
    parseCommandConfig,
    commonAgentOptions,
    rootOpt,
    namesOpt,
    promptOpt,
    sessionOpt,
    modelOpt,
    providerOpt,
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
  | BackendLlm LlmConfig
  | BackendCommand CommandConfig
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

-- ---------------------------------------------------------------------------
-- Defaults
-- ---------------------------------------------------------------------------

defaultHermesConfig :: HermesConfig
defaultHermesConfig =
  HermesConfig
    { hcRoot = ".",
      hcNames = [],
      hcPromptFile = "",
      hcSessionFile = Nothing,
      hcModel = Nothing,
      hcProvider = Nothing,
      hcYolo = True,
      hcQuiesce = Nothing,
      hcPitboss = Nothing
    }

defaultKimiConfig :: KimiConfig
defaultKimiConfig =
  KimiConfig
    { kcRoot = ".",
      kcNames = [],
      kcPromptFile = "",
      kcSessionFile = Nothing,
      kcModel = Nothing,
      kcProvider = Nothing,
      kcQuiesce = Nothing,
      kcPitboss = Nothing
    }

defaultLlmConfig :: LlmConfig
defaultLlmConfig =
  LlmConfig
    { lcRoot = ".",
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
    { ccRoot = ".",
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
        <> value "."
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
  optional
    $ strOption
      ( long "session"
          <> short 's'
          <> metavar "FILE"
          <> help "Session file (default: ROOT/.sessions/<name>.sid)"
      )

modelOpt :: Parser (Maybe Text)
modelOpt =
  optional
    $ option
      (T.pack <$> str)
      ( long "model"
          <> short 'm'
          <> metavar "MODEL"
          <> help "Model passed to the backend"
      )

providerOpt :: Parser (Maybe Text)
providerOpt =
  optional
    $ option
      (T.pack <$> str)
      ( long "provider"
          <> metavar "PROVIDER"
          <> help "Provider passed to the backend"
      )

quiesceOpt :: Parser (Maybe Int)
quiesceOpt =
  optional
    $ option
      auto
      ( long "quiesce"
          <> metavar "N"
          <> help "Exit after N empty one-second cycles"
      )

pitbossOpt :: Parser (Maybe Name)
pitbossOpt =
  optional
    $ option
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
