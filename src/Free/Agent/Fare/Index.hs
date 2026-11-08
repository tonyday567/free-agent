{-# LANGUAGE OverloadedStrings #-}

-- | fare index — the decayed-area query over kimi wires.
--
-- The verdict comes first: how many areas, how much of the store was
-- searched, how many records failed to parse. Rows are evidence, one per
-- area, headed and tab separated so a seat can slice them with awk.
--
-- The query is the one already proven by corpus @kw-index@, carried here as
-- library code rather than a shell-out: lowercase whitespace tokens, literal
-- substring match inside a single token, decayed recurrence
-- @s' = r*s + hit@, and maximal runs whose score stays strictly above the
-- cutoff. A score equal to the cutoff is not an area.
--
-- Evidence boundary: text inside a kimi wire's
-- @context.append_loop_event@ / @content.part@ record only. This is not a
-- whole-event or whole-dialect search, offsets are zero-based token indices,
-- and @fare read@ can page the report artifact but cannot resolve a part or
-- token locator back into transcript context.
module Free.Agent.Fare.Index
  ( Area (..),
    areas,
    defaultCutoff,
    decayRate,
    normalizeKeyword,
    renderRows,
    renderVerdict,
    runIndex,
    snippetWidth,
  )
where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), decodeStrict')
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO qualified as TIO
import Free.Agent.Fare.Search (collectFiles)
import System.Directory (doesDirectoryExist, getHomeDirectory, getTemporaryDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hClose, hPutStrLn, openTempFile, stderr)

-- | the house default cutoff for the area machine.
defaultCutoff :: Double
defaultCutoff = 0.5

-- | the fixed decay. Not a flag: the minimal slice keeps one visible rate.
decayRate :: Double
decayRate = 0.9

-- | how many tokens a row's snippet carries, starting at the run's first hit.
snippetWidth :: Int
snippetWidth = 14

-- | how many rows the verdict carries before the tail moves to an artifact.
rowBudget :: Int
rowBudget = 30

-- | default store: the kimi wire tree.
defaultRoot :: IO FilePath
defaultRoot = (</> ".kimi-code" </> "sessions") <$> getHomeDirectory

-- | one area: a maximal run of tokens whose decayed score stays above the
-- cutoff, with the peak it reached and a snippet from its start.
data Area = Area
  { arPath :: FilePath,
    arPart :: Int,
    arStart :: Int,
    arEnd :: Int,
    arPeak :: Double,
    arSnippet :: Text
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- the machine (pure: witnesses live here)
-- ---------------------------------------------------------------------------

-- | lowercase whitespace tokens; a token is the matching unit.
tokenize :: Text -> [Text]
tokenize = T.words . T.toLower

-- | the matcher's normalization: one lowercase, stripped keyword.
normalizeKeyword :: Text -> Text
normalizeKeyword = T.toLower . T.strip

-- | the decayed score at every token position.
scores :: Text -> [Text] -> [Double]
scores keyword = go 0
  where
    go _ [] = []
    go s (t : ts) = let s' = decayRate * s + (if keyword `T.isInfixOf` t then 1 else 0) in s' : go s' ts

-- | maximal runs of positions scoring strictly above the cutoff, each with its
-- start index, end index and peak.
runs :: Double -> Text -> [Text] -> [(Int, Int, Double)]
runs cutoff keyword toks = collect (zip [0 ..] (scores keyword toks))
  where
    collect [] = []
    collect ((i, s) : rest)
      | s > cutoff = emit i s (span ((> cutoff) . snd) rest)
      | otherwise = collect rest
    emit i s (inside, after) =
      let peak = maximum (s : map snd inside)
          endIx = maximum (i : map fst inside)
       in (i, endIx, peak) : collect after

-- | areas for one content part, in token order, with the part and path left
-- for the caller to stamp.
areas :: Double -> Text -> Text -> [Area]
areas cutoff keyword partText =
  [ Area
      { arPath = "",
        arPart = 0,
        arStart = start,
        arEnd = end,
        arPeak = peak,
        arSnippet = T.unwords (take snippetWidth (drop start toks))
      }
  | (start, end, peak) <- runs cutoff keyword toks
  ]
  where
    toks = tokenize partText

-- ---------------------------------------------------------------------------
-- reading wires
-- ---------------------------------------------------------------------------

-- | text parts of one wire, in order, with the number of records that did not
-- parse. A record outside the kept shape contributes no text and no failure.
parseWire :: B.ByteString -> ([Text], Int)
parseWire bytes =
  let steps = [step line | line <- BC.lines bytes, not (BC.null (BC.strip line))]
   in (concatMap fst steps, sum (map snd steps))
  where
    step line = case decodeStrict' line of
      Nothing -> ([], 1)
      Just value -> (textParts value, 0)

-- | the kept shape: @context.append_loop_event@ with a @content.part@ text part.
textParts :: Value -> [Text]
textParts (Object o) = case KM.lookup "type" o of
  Just (String "context.append_loop_event") -> case KM.lookup "event" o of
    Just (Object e) -> case KM.lookup "part" e of
      Just (Object p) -> case (KM.lookup "type" e, KM.lookup "type" p, KM.lookup "text" p) of
        (Just (String "content.part"), Just (String "text"), Just (String t)) -> [t]
        _ -> []
      _ -> []
    _ -> []
  _ -> []
textParts _ = []

-- ---------------------------------------------------------------------------
-- rendering
-- ---------------------------------------------------------------------------

formatPeak :: Double -> Text
formatPeak x = T.pack (show (fromIntegral (round (x * 1000) :: Integer) / 1000 :: Double))

-- | the verdict line: the whole answer, before any evidence row.
renderVerdict :: Text -> Int -> Int -> Int -> Int -> Double -> Text
renderVerdict keyword areaCount wiresWithAreas wires parseFails cutoff =
  T.concat
    [ "index ",
      keyword,
      ": ",
      T.pack (show areaCount),
      " areas in ",
      T.pack (show wiresWithAreas),
      "/",
      T.pack (show wires),
      " wires · parse fails ",
      T.pack (show parseFails),
      " · cutoff ",
      formatPeak cutoff,
      " · decay ",
      formatPeak decayRate
    ]

-- | headed TSV rows, deterministic: descending peak, then path, part, start.
renderRows :: [Area] -> [Text]
renderRows as = header : map row ordered
  where
    header = "path\tpart\tstart\tend\tpeak\tsnippet"
    ordered = sortBy (comparing (Down . arPeak) <> comparing arPath <> comparing arPart <> comparing arStart) as
    row a =
      T.intercalate
        "\t"
        [ T.pack (arPath a),
          T.pack (show (arPart a)),
          T.pack (show (arStart a)),
          T.pack (show (arEnd a)),
          formatPeak (arPeak a),
          arSnippet a
        ]

-- ---------------------------------------------------------------------------
-- the command
-- ---------------------------------------------------------------------------

-- | @fare index KEYWORD [ROOT] [--cutoff N]@.
runIndex :: Text -> FilePath -> Maybe Double -> IO ()
runIndex rawKeyword rawRoot mcutoff = do
  let keyword = normalizeKeyword rawKeyword
  when (T.null keyword) $ die "index: KEYWORD is required and must be nonempty"
  when (T.any isSpace keyword) $ die "index: KEYWORD must contain no whitespace"
  cutoff <- case mcutoff of
    Nothing -> pure defaultCutoff
    Just c
      | isNaN c || isInfinite c -> die "index: --cutoff must be finite"
      | c < 0 -> die "index: --cutoff must be nonnegative"
      | otherwise -> pure c
  root <- if null rawRoot then defaultRoot else pure rawRoot
  exists <- doesDirectoryExist root
  unless exists $ die ("index: root is not a directory: " <> T.pack root)
  files <- filter ((== "wire.jsonl") . takeFileName) <$> collectFiles root
  collected <- forM files $ \p -> do
    bytes <- B.readFile p
    pure $ case decodeUtf8' bytes of
      Left _ -> (0, [])
      Right _ ->
        let (parts, fails) = parseWire bytes
            stamped =
              concat
                [ [a {arPath = p, arPart = ix} | a <- areas cutoff keyword text]
                | (ix, text) <- zip [0 :: Int ..] parts
                ]
         in (fails, stamped)
  let parseFails = sum [ns | (ns, _) <- collected]
      allAreas = concat [as | (_, as) <- collected]
      wires = length files
      withAreas = length [() | (_, as) <- collected, not (null as)]
  TIO.putStrLn (renderVerdict keyword (length allAreas) withAreas wires parseFails cutoff)
  let rows = renderRows allAreas
  mapM_ TIO.putStrLn (take (rowBudget + 1) rows)
  when (length allAreas > rowBudget) $ do
    tmp <- getTemporaryDirectory
    -- Unique per run: a keyword-keyed name let two seats indexing the same
    -- keyword overwrite each other's tail, so the first seat's `next:` line
    -- handed it the second seat's rows. openTempFile allocates the name
    -- atomically and keeps the .tsv suffix.
    (artifact, handle) <- openTempFile tmp ("fare-index-" <> T.unpack keyword <> ".tsv")
    TIO.hPutStr handle (T.unlines rows)
    hClose handle
    TIO.putStrLn ("next: fare read " <> T.pack artifact <> " 1,40")

die :: Text -> IO a
die message = do
  hPutStrLn stderr ("🔴 " <> T.unpack message)
  exitFailure
