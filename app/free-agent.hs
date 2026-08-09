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

import Circuit (close, companion, conjoint)
import Circuit.Agent (Name, Post (..), mkPost, sortNub)
import Circuit.Agent.Framing ( stamp, stamped)
import Control.Applicative ((<|>))
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Monad (unless)
import Control.Monad.State (runStateT)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime)
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus.Cli (BusCommand, busParser, runBusCommand, runStatus)
import Free.Agent.Bus.File (QuiesceConfig (..), cursorPath)
import Free.Agent.Cli.Config
  ( Backend (..),
    CommandConfig (..),
    HermesConfig (..),
    KimiConfig (..),
    LlmConfig (..),
    parseCommandConfig,
    parseHermesConfig,
    parseLlmConfig,
  )
import Free.Agent.Host (BareConfig (..), BodyMode (..), Host (..), bareHost, defaultBareConfig, hermesHostBatch, kimiHost, processHost)
import Free.Agent.Seat (FreeSeat, hostSeat, interpretSeat)
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
  = AgentCmd Backend
  | BusCmd BusCommand
  | StatusCmd FilePath (Maybe NominalDiffTime)
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Backend tag for default agent command
-- ---------------------------------------------------------------------------

data BackendTag = HermesTag | KimiTag
  deriving (Show, Eq)

backendFlag :: Parser BackendTag
backendFlag =
  flag
    HermesTag
    KimiTag
    ( long "backend"
        <> short 'b'
        <> help "Agent backend for the default seat (default: hermes)"
    )

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

-- | Parse the default agent command. Defaults to Hermes; --backend switches to Kimi.
defaultAgentParser :: Parser Command
defaultAgentParser =
  (\tag cfg -> AgentCmd (mkBackend tag cfg)) <$> backendFlag <*> parseHermesConfig
  where
    mkBackend HermesTag cfg = BackendHermes cfg
    mkBackend KimiTag cfg = BackendKimi (toKimiConfig cfg)

-- ---------------------------------------------------------------------------
-- Top-level parser
-- ---------------------------------------------------------------------------

commandParser :: Parser Command
commandParser =
  subparser
    ( command "bus" (info (BusCmd <$> busParser <**> helper) (progDesc "Bus subcommands"))
        <> command "llm" (info (AgentCmd . BackendLlm <$> parseLlmConfig <**> helper) (progDesc "Direct API seat (old bus-bare)"))
        <> command "cmd" (info (AgentCmd . BackendCommand <$> parseCommandConfig <**> helper) (progDesc "External-command seat"))
        <> command "status" (info (StatusCmd <$> rootOpt <*> optional thresholdOpt <**> helper) (progDesc "Bus log statistics"))
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
    AgentCmd backend -> runBackend backend
    BusCmd busCmd -> runBusCommand busCmd
    StatusCmd root mthreshold -> runStatus root (fromMaybe 900 mthreshold)

-- ---------------------------------------------------------------------------
-- Backend dispatch
-- ---------------------------------------------------------------------------

runBackend :: Backend -> IO ()
runBackend (BackendHermes cfg) = runHermes cfg
runBackend (BackendKimi cfg) = runKimi cfg
runBackend (BackendLlm cfg) = runLlm cfg
runBackend (BackendCommand cfg) = runCommand cfg

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

-- | Drop Hermes/Kimi CLI noise lines that can precede the actual reply text.
scrubReply :: Post Text -> Post Text
scrubReply p =
  let ls = T.lines (body p)
      clean = filter (not . noise) ls
      noise l =
        T.null l
          || "↪" `T.isPrefixOf` l
          || "session_id:" `T.isPrefixOf` l
          || "Warning:" `T.isPrefixOf` l
          || "Resumed session" `T.isInfixOf` l
          || "Resume this session with:" `T.isInfixOf` l
          || "⚕" `T.isPrefixOf` l
          || "❯" `T.isPrefixOf` l
          || "kimi version" `T.isPrefixOf` l
          || "To resume this session:" `T.isPrefixOf` l
   in p {body = T.strip (T.unlines clean)}

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

-- | Run one stored post through a seat and produce reply posts with thread edges.
runOneSeat :: FreeSeat -> Stamped Text -> IO [Post Text]
runOneSeat seat stored = do
  let stored' = decorateSender stored
      p = stamped stored'
      parentId = stamp stored
      sh = interpretSeat seat
  (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
  pure [routeReply (scrubReply out) {thread = sortNub (parentId : thread out)} | out <- outs]

keepReplyHermes :: Post Text -> Bool
keepReplyHermes p =
  let b = T.strip (body p)
   in not (T.null b) && not ("⚠️" `T.isPrefixOf` b)

runHermes :: HermesConfig -> IO ()
runHermes cfg = do
  validatePrompt (hcPromptFile cfg)
  systemPrompt <- TIO.readFile (hcPromptFile cfg)
  let names = hcNames cfg
      agentName = agentNameOf names
      sessionFile = fromMaybe (defaultSessionFile (hcRoot cfg) names) (hcSessionFile cfg)
      -- F3: inject agent name so the LLM knows its posting identity
      framedPrompt = "Your name on the free-agent bus is " <> agentName <> ". Use this name for all --from fields and cursor checks.\n\n" <> systemPrompt
  createDirectoryIfMissing True (takeDirectory sessionFile)
  let host = hermesHostBatch agentName framedPrompt (hcModel cfg) (hcProvider cfg) (hcYolo cfg) sessionFile
      seat = hostSeat host
      mQuiesce = buildQuiesce (hcQuiesce cfg) (hcPitboss cfg)
      handle stored = filter keepReplyHermes <$> runOneSeat seat stored
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runAgentLoop agentName names (hcRoot cfg) mQuiesce handle

runKimi :: KimiConfig -> IO ()
runKimi cfg = do
  validatePrompt (kcPromptFile cfg)
  systemPrompt <- TIO.readFile (kcPromptFile cfg)
  let names = kcNames cfg
      agentName = agentNameOf names
      sessionFile = fromMaybe (defaultSessionFile (kcRoot cfg) names) (kcSessionFile cfg)
  createDirectoryIfMissing True (takeDirectory sessionFile)
  let host0 = kimiHost agentName (kcModel cfg) (kcProvider cfg) sessionFile
      host =
        host0
          { hostRun =
              \bodies -> hostRun host0 (map (\b -> systemPrompt <> "\n\nUser message:\n" <> b) bodies)
          }
      seat = hostSeat host
      mQuiesce = buildQuiesce (kcQuiesce cfg) (kcPitboss cfg)
      handle stored = filter keepReplyHermes <$> runOneSeat seat stored
  TIO.putStrLn $ "   session: " <> T.pack sessionFile
  runAgentLoop agentName names (kcRoot cfg) mQuiesce handle

-- ---------------------------------------------------------------------------
-- Direct API (llm)
-- ---------------------------------------------------------------------------

runLlm :: LlmConfig -> IO ()
runLlm cfg = do
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
          seat = hostSeat host
          mQuiesce = buildQuiesce (lcQuiesce cfg) (lcPitboss cfg)
          handle stored = do
            let p = stamped stored
                parentId = stamp stored
                sh = interpretSeat seat
            (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
            pure [out {thread = sortNub (parentId : thread out)} | out <- outs, not (T.null (T.strip (body out)))]
      TIO.putStrLn $ "   base: " <> baseUrl bareCfg
      TIO.putStrLn $ "   model: " <> model bareCfg
      runAgentLoop agentName names (lcRoot cfg) mQuiesce handle

-- ---------------------------------------------------------------------------
-- External command seat
-- ---------------------------------------------------------------------------

runCommand :: CommandConfig -> IO ()
runCommand cfg = do
  let names = ccNames cfg
      agentName = agentNameOf names
      host = (processHost agentName (ccCmd cfg) (ccArgs cfg)) {hostBodyMode = BodyWhole}
      seat = hostSeat host
      mQuiesce = buildQuiesce (ccQuiesce cfg) (ccPitboss cfg)
      handle stored = do
        let p = stamped stored
            parentId = stamp stored
            sh = interpretSeat seat
        (outs, _st) <- runStateT (runKleisli (close (conjoint sh) (companion sh)) [p]) []
        pure [out {thread = sortNub (parentId : thread out)} | out <- outs, not (T.null (T.strip (body out)))]
  TIO.putStrLn $ "   command: " <> T.pack (ccCmd cfg) <> " " <> T.intercalate " " (map T.pack (ccArgs cfg))
  runAgentLoop agentName names (ccRoot cfg) mQuiesce handle

