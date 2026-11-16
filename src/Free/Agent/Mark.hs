{-# LANGUAGE OverloadedStrings #-}

-- | The halt-mark grammar as a type.
--
-- Marks are the bus's boundary vocabulary: a finite set of glyphs that
-- prefix a post body and announce the post's role in an exchange.
--
-- A mark is control only in the control form — alone on the body's first
-- line, where the glyph is the whole message. A mark that leads words is a
-- label, and the words are the message; 'controlMark' is that judgement and
-- 'markOf' is the grammar without it.
module Free.Agent.Mark
  ( Mark (..),
    markGlyph,
    parseMark,
    markOf,
    controlMark,
    isHalt,
    isEscalate,
  )
where

import Circuit.Post (Post (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | A boundary mark.
data Mark
  = -- | 🟡 claim: "I pick this up".
    Motion
  | -- | ✓ no objection.
    Consent
  | -- | ↩ amendment offered.
    Amendment
  | -- | 🔴 escalation: "I can't move this".
    Escalate
  | -- | 🟢 halt: landed.
    Landed
  | -- | 🔵 halt: standing down / quiescent.
    StandDown
  | -- | ⌛ timeout stand-down: gave up waiting. Not a halt — loops keep spinning.
    Timeout
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The glyph that renders a mark.
markGlyph :: Mark -> Text
markGlyph = \case
  Motion -> "\x1F7E1" -- 🟡
  Consent -> "\x2713" -- ✓
  Amendment -> "\x21A9" -- ↩
  Escalate -> "\x1F534" -- 🔴
  Landed -> "\x1F7E2" -- 🟢
  StandDown -> "\x1F535" -- 🔵
  Timeout -> "\x231B" -- ⌛

-- | Parse a mark from the start of a body. Tolerates the emoji variation
-- selector (U+FE0F) between glyph and rest. Exact glyphs only — no fuzzy
-- matching, so a body that merely talks about marks is not one.
parseMark :: Text -> Maybe Mark
parseMark t = go [minBound .. maxBound]
  where
    go [] = Nothing
    go (m : ms)
      | glyph `T.isPrefixOf` t = Just m
      | (glyph <> "\xFE0F") `T.isPrefixOf` t = Just m
      | otherwise = go ms
      where
        glyph = markGlyph m

-- | The mark carried by a post's body, if any.
markOf :: Post Text Text -> Maybe Mark
markOf = parseMark . body

-- | The mark a post is controlled by, if it is written in the control form.
--
-- The glyphs do two jobs, and the body's first line says which. A mark
-- alone on that line is the whole message — a command: halt, escalate, the
-- envelope itself. A mark that leads words on that line is a label — the
-- status prefix the runner's cards and reports use (@🟢 tranche 1 done@,
-- @🟡 open header@) — and is content. Reading the label as a command retires
-- a seat that was only reporting.
--
-- The reason for a command rides below the glyph, on later lines, where it
-- costs the command nothing.
controlMark :: Post Text Text -> Maybe Mark
controlMark p =
  case parseMark line of
    Just m
      | line == markGlyph m || line == markGlyph m <> "\xFE0F" -> Just m
    _ -> Nothing
  where
    line = T.strip (T.takeWhile (/= '\n') (body p))

-- | Halt marks: the exchange is over. 'Landed' and 'StandDown'.
-- 'Timeout' is deliberately not one — a timeout decides nothing.
isHalt :: Mark -> Bool
isHalt = \case
  Landed -> True
  StandDown -> True
  _ -> False

-- | Escalation: leave the loop, wake the human.
isEscalate :: Mark -> Bool
isEscalate = (== Escalate)
