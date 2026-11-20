{-# LANGUAGE OverloadedStrings #-}

-- | The configurable text codec for free-agent's bus and check protocols.
--
-- Machine consumers take a 'Protocol' and use the parsers here rather than
-- growing private string matches. 'defaultProtocol' preserves the historical
-- wire exactly: kind words remain uppercase and case-sensitive, and no
-- compatibility reader is introduced.
module Free.Agent.Protocol
  ( Protocol (..),
    defaultProtocol,
    PostKind (..),
    kindWord,
    parsePostKind,
    hasPostKind,
    BlockLink (..),
    parseBlockLink,
    parseAddress,
    renderAddress,
    scrubPresentationBullet,
    isSuppressedReply,
    ResultKind (..),
    resultWord,
    protocolSummary,
  )
where

import Circuit.Post (PostId)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Every hardcoded text token used to classify bus posts, address a reply,
-- suppress an empty reply, or render an executable check result.
--
-- Fields are wire tokens, not prose templates. Callers that need another
-- vocabulary supply a different record; readers never accept both their
-- configured token and the default token implicitly.
data Protocol = Protocol
  { protocolTaskWord :: Text,
    protocolClaimWord :: Text,
    protocolDoneWord :: Text,
    protocolLinkWord :: Text,
    protocolLinkSeparator :: Text,
    protocolLinkVerb :: Text,
    protocolAddressPrefix :: Text,
    protocolAddressTerminator :: Text,
    protocolPresentationBullet :: Text,
    protocolEmptyReply :: Text,
    protocolPassWord :: Text,
    protocolFailWord :: Text
  }
  deriving (Eq, Show)

-- | The deployed wire protocol. Compatibility aliases deliberately absent.
defaultProtocol :: Protocol
defaultProtocol =
  Protocol
    { protocolTaskWord = "TASK",
      protocolClaimWord = "CLAIM",
      protocolDoneWord = "DONE",
      protocolLinkWord = "LINK",
      protocolLinkSeparator = "·",
      protocolLinkVerb = "blocks",
      protocolAddressPrefix = "@",
      protocolAddressTerminator = ":",
      protocolPresentationBullet = "• ",
      protocolEmptyReply = "(empty)",
      protocolPassWord = "PASS",
      protocolFailWord = "FAIL"
    }

-- | Kinds that change bus task/readiness state. Other all-caps words remain
-- presentation labels, not controls.
data PostKind = Task | Claim | Done | Link
  deriving (Eq, Ord, Show, Enum, Bounded)

kindWord :: Protocol -> PostKind -> Text
kindWord protocol = \case
  Task -> protocolTaskWord protocol
  Claim -> protocolClaimWord protocol
  Done -> protocolDoneWord protocol
  Link -> protocolLinkWord protocol

-- | Parse the first meaningful word as a configured kind. One leading reply
-- address is stripped, matching the historical announcement form.
parsePostKind :: Protocol -> Text -> Maybe PostKind
parsePostKind protocol text =
  case T.words unaddressed of
    [] -> Nothing
    (word : _) -> lookup word (map (\kind -> (kindWord protocol kind, kind)) [minBound .. maxBound])
  where
    unaddressed = maybe text snd (parseAddress protocol text)

hasPostKind :: Protocol -> PostKind -> Text -> Bool
hasPostKind protocol kind text = parsePostKind protocol text == Just kind

-- | A typed dependency edge decoded from the configured LINK grammar.
data BlockLink = BlockLink
  { linkBlocker :: PostId,
    linkTarget :: PostId
  }
  deriving (Eq, Show)

parseBlockLink :: Protocol -> Text -> Maybe BlockLink
parseBlockLink protocol text =
  case T.words text of
    (kind : separator : blocker : verb : target : _)
      | kind == protocolLinkWord protocol
          && separator == protocolLinkSeparator protocol
          && verb == protocolLinkVerb protocol ->
          BlockLink <$> readMaybe (T.unpack blocker) <*> readMaybe (T.unpack target)
    _ -> Nothing

-- | Decode the explicit reply address at the start of a response. The name is
-- non-empty, contains no whitespace or address prefix, and is followed by the
-- configured terminator. The response suffix is returned without the address.
parseAddress :: Protocol -> Text -> Maybe (Text, Text)
parseAddress protocol response = do
  rest <- T.stripPrefix (protocolAddressPrefix protocol) (T.stripStart response)
  let (name, afterName) = T.breakOn (protocolAddressTerminator protocol) rest
      recipient = T.strip name
  if T.null recipient
    || T.null afterName
    || T.any isSpace recipient
    || protocolAddressPrefix protocol `T.isInfixOf` recipient
    then Nothing
    else Just (recipient, T.strip (T.drop (T.length (protocolAddressTerminator protocol)) afterName))

renderAddress :: Protocol -> Text -> Text
renderAddress protocol name = protocolAddressPrefix protocol <> name <> protocolAddressTerminator protocol

-- | Remove one presentation bullet only when it wraps the configured reply
-- address prefix. This preserves the Kimi stdout scrub without owning a second
-- address grammar.
scrubPresentationBullet :: Protocol -> Text -> Text
scrubPresentationBullet protocol text =
  case T.stripPrefix (protocolPresentationBullet protocol) text of
    Just rest | protocolAddressPrefix protocol `T.isPrefixOf` rest -> rest
    _ -> text

-- | Empty responses and the configured empty marker are local, not bus posts.
isSuppressedReply :: Protocol -> Text -> Bool
isSuppressedReply protocol text =
  let stripped = T.strip text
   in T.null stripped || stripped == protocolEmptyReply protocol

-- | Stdout words used by the executable check harnesses.
data ResultKind = Pass | Fail
  deriving (Eq, Show)

resultWord :: Protocol -> ResultKind -> Text
resultWord protocol Pass = protocolPassWord protocol
resultWord protocol Fail = protocolFailWord protocol

-- | The help-facing account of the configured public wire surface.
protocolSummary :: Protocol -> Text
protocolSummary protocol =
  "Bus wire kinds "
    <> T.intercalate ", " (map (kindWord protocol) [Task, Claim, Done, Link])
    <> "; "
    <> renderAddress protocol "name"
    <> " addresses a reply; dependency links use "
    <> protocolLinkWord protocol
    <> " "
    <> protocolLinkSeparator protocol
    <> " A "
    <> protocolLinkVerb protocol
    <> " B"
