{-# LANGUAGE OverloadedStrings #-}

-- | fare read — file slices, never whole files.
--
-- Layer 1 is a line-numbered slice (@line:content@, the grep -n shape,
-- so slices compose with @fare search@ output) under the item's own
-- byte budget ('layer1Bound'). With no range the slice is the file's
-- head; --tail flips to its last bytes. An explicit range (@40,80@ or
-- @+N@, 1-based inclusive) selects the window, and the byte bound
-- still applies inside it — --tail then keeps the window's last
-- bytes. Lines are display-capped ('lineDisplayCap') with a … marker.
--
-- Ranges clamp honestly: a window past end of file reports empty
-- rather than erroring, and a window is cut to what exists. Real
-- errors — missing file, non-UTF-8 content, unparseable range — go
-- to stderr with exit 1. Like search, success always exits 0.
--
-- A @-N@ tail-range syntax was considered and rejected: optparse
-- eats dash-leading arguments (use --tail instead).
module Free.Agent.Fare.Read (runRead, parseRange) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO qualified as TIO
import Free.Agent.Fare.Config (End (..), applyEndUnder)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Posix.Files (getFileStatus, isRegularFile)

-- | the item's own layer-1 bound, in bytes (same evidence as search:
-- wave 1 saw 78% of grep results under 2KB).
layer1Bound :: Int
layer1Bound = 2000

-- | display cap for a single line, as in search.
lineDisplayCap :: Int
lineDisplayCap = 240

-- | run a read and print the distilled layer-1 answer.
runRead :: FilePath -> Maybe Text -> Maybe End -> IO ()
runRead path mrange mend = do
  exists <- doesFileExist path
  unless exists $ die (T.pack path <> ": no such file")
  regular <- isRegularFile <$> getFileStatus path
  unless regular $ die (T.pack path <> ": not a regular file")
  bytes <- B.readFile path
  content <- case decodeUtf8' bytes of
    Left _ -> die (T.pack path <> ": not valid UTF-8")
    Right c -> pure c
  range <- case traverse parseRange mrange of
    Left err -> die err
    Right r -> pure r
  let numbered = zip [1 :: Int ..] (T.lines content)
      total = length numbered
      window = windowOf range total numbered
      end = fromMaybe Head mend
      shown = applyEndUnder layer1Bound end (map (formatLine . snd) window)
      k = length shown
      shownWindow = case end of
        Head -> take k window
        Tail -> drop (length window - k) window
  TIO.putStrLn (verdict total range shownWindow k (map fst window) end)
  mapM_ (TIO.putStrLn . formatLine . snd) shownWindow

-- ---------------------------------------------------------------------------
-- Ranges
-- ---------------------------------------------------------------------------

-- | a 1-based inclusive line window; 'Nothing' means the whole file.
type Range = Maybe (Int, Int)

-- | parse a range spec: @40,80@ (window) or @+N@ (first N lines).

-- $setup
-- >>> import Data.Text (pack)
--
-- >>> parseRange (pack "40,80")
-- Right (40,80)
-- >>> parseRange (pack "+12")
-- Right (1,12)
-- >>> parseRange (pack "80,40")
-- Left "range start after end: 80,40"

parseRange :: Text -> Either Text (Int, Int)
parseRange t = case T.split (== ',') t of
  [a, b] -> do
    sa <- posNum "start" a
    sb <- posNum "end" b
    if sa > sb
      then Left ("range start after end: " <> t)
      else Right (sa, sb)
  _ -> case T.uncons t of
    Just ('+', n) -> do
      sn <- posNum "count" n
      Right (1, sn)
    _ -> Left ("unrecognised range (want 40,80 or +40): " <> t)

posNum :: Text -> Text -> Either Text Int
posNum what t = case reads (T.unpack t) of
  [(n, "")]
    | n >= 1 -> Right n
    | otherwise -> Left (what <> " must be 1 or more: " <> t)
  _ -> Left ("bad " <> what <> " number: " <> t)

-- | select the window, clamped to what exists.
windowOf :: Range -> Int -> [(Int, Text)] -> [(Int, Text)]
windowOf Nothing _ = id
windowOf (Just (a, b)) total = takeWhile ((<= b') . fst) . dropWhile ((< a) . fst)
  where
    b' = min b total

-- ---------------------------------------------------------------------------
-- Output
-- ---------------------------------------------------------------------------

formatLine :: Text -> Text
formatLine l
  | T.length l > lineDisplayCap = T.take lineDisplayCap l <> " …"
  | otherwise = l

verdict :: Int -> Range -> [(Int, Text)] -> Int -> [Int] -> End -> Text
verdict total range shownWindow k windowNos end
  | total == 0 = "0 lines"
  | k == 0 = case range of
      Just (a, _)
        | a > total ->
            T.pack (show total) <> " lines, range starts at line " <> T.pack (show a) <> " — beyond end of file"
      _ -> T.pack (show total) <> " lines, nothing shown"
  | k >= length windowNos =
      T.pack (show total) <> " lines, showing lines " <> spanText
  | otherwise =
      T.pack (show total)
        <> " lines, showing lines "
        <> spanText
        <> " (bound "
        <> T.pack (show layer1Bound)
        <> " bytes, "
        <> endWord
        <> ")"
  where
    spanText = case (shownWindow, reverse shownWindow) of
      ((a, _) : _, (b, _) : _) -> T.pack (show a) <> "-" <> T.pack (show b)
      _ -> ""
    endWord = case end of
      Head -> "first"
      Tail -> "last"

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

die :: Text -> IO a
die msg = do
  TIO.hPutStrLn stderr ("fare read: " <> msg)
  exitFailure
