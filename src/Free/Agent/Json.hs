-- | Small helpers over 'Circuit.Parser.Json.Json' for the free-agent wire
-- codecs.  These are the shapes that 'Free.Agent.Acp', 'Free.Agent.Gateway',
-- and 'Free.Agent.BusStats' need; they are not a general JSON utility library.
module Free.Agent.Json
  ( -- * Lookup
    objLookup,
    arrToList,
    textAt,
    numAt,
    boolAt,
    intAt,

    -- * Construction
    jtext,
    jnum,
    jbool,
    jobject,
    jarray,

    -- * Encoding
    encodeJsonText,
  )
where

import Circuit.Parser.Json (Json (..), encodeJson)
import Data.Scientific (Scientific, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V

-- | Look up a key in a JSON object.
objLookup :: Text -> Json -> Maybe Json
objLookup k (JObject ps) = lookup k ps
objLookup _ _ = Nothing

-- | Convert a JSON array to a list.
arrToList :: Json -> [Json]
arrToList (JArray v) = V.toList v
arrToList _ = []

-- | Extract a text value from a JSON object field.
textAt :: Text -> Json -> Maybe Text
textAt k j = case objLookup k j of
  Just (JString t) -> Just t
  _ -> Nothing

-- | Extract a numeric value from a JSON object field.
numAt :: Text -> Json -> Maybe Scientific
numAt k j = case objLookup k j of
  Just (JNumber n) -> Just n
  _ -> Nothing

-- | Extract a boolean value from a JSON object field.
boolAt :: Text -> Json -> Maybe Bool
boolAt k j = case objLookup k j of
  Just (JBool b) -> Just b
  _ -> Nothing

-- | Extract a bounded integer value from a JSON object field.
intAt :: Text -> Json -> Maybe Int
intAt k j = numAt k j >>= toBoundedInteger

-- | JSON string value.
jtext :: Text -> Json
jtext = JString

-- | JSON number value from an integral literal.
jnum :: (Integral a) => a -> Json
jnum = JNumber . fromIntegral

-- | JSON boolean value.
jbool :: Bool -> Json
jbool = JBool

-- | JSON object from key/value pairs.
jobject :: [(Text, Json)] -> Json
jobject = JObject

-- | JSON array from a list.
jarray :: [Json] -> Json
jarray = JArray . V.fromList

-- | Render a 'Json' value to 'Text'.
encodeJsonText :: Json -> Text
encodeJsonText = decodeUtf8 . encodeJson
