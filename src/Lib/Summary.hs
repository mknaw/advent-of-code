module Lib.Summary
  ( writeBenchmark,
  )
where

import Criterion.Measurement (secs)
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Lib.Parse (parseInt)
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

data PuzzleSummary = PuzzleSummary
  -- TODO probably should have a PuzzleSpec member
  { _year :: Int,
    _day :: Day,
    _aComplete :: Bool,
    _aBenchmark :: String,
    _bComplete :: Bool,
    _bBenchmark :: String
  }
  deriving (Show)

type PuzzleSummaryMap = M.Map (Int, Day) PuzzleSummary

readSummaryFile :: FilePath -> IO PuzzleSummaryMap
readSummaryFile f = do
  contents <- T.lines <$> T.readFile f
  let summaries = catMaybes $ parseMaybe parseTableRow <$> contents
  return $ M.fromList $ (\ps -> ((_year ps, _day ps), ps)) <$> summaries

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseTableRow :: Parser PuzzleSummary
parseTableRow = do
  string "| '"
  _year <- parseInt
  string " | "
  _day <- Day <$> parseInt
  string " | "
  _aComplete <- (== '✔') <$> anySingle
  string " |"
  _aBenchmark <- fmap trim . manyTill anySingle $ string " | "
  _bComplete <- (== '✔') <$> anySingle
  string " |"
  _bBenchmark <- fmap trim . manyTill anySingle $ string " |"
  return PuzzleSummary {..}

-- TODO there's some bug in here where it doesn't write the bench time when it adds new row
writeBenchmark :: PuzzleSpec -> Double -> IO ()
writeBenchmark spec meantime = do
  let newValue =
        PuzzleSummary
          { _year = unYear spec,
            _day = unDay spec,
            _aComplete = False,
            _aBenchmark = "",
            _bComplete = False,
            _bBenchmark = ""
          }
  summaries <- M.insertWith f (unYear spec, unDay spec) newValue <$> readSummaryFile "README.md"
  let table = T.pack . unlines . fmap (serializeSummary . snd) $ M.toAscList summaries
  writeReadme table
  where
    f :: PuzzleSummary -> PuzzleSummary -> PuzzleSummary
    f _ s
      | unPart spec == PartA = s {_aComplete = True, _aBenchmark = secs meantime}
      | otherwise = s {_bComplete = True, _bBenchmark = secs meantime}

serializeSummary :: PuzzleSummary -> String
serializeSummary PuzzleSummary {..} =
  concat
    [ "| '",
      show _year,
      " | ",
      show _day,
      " | ",
      if _aComplete then "✔" else " ",
      " | ", -- TODO bench time
      _aBenchmark,
      " | ",
      if _bComplete then "✔" else " ",
      " | ",
      _bBenchmark,
      " |"
    ]

writeReadme :: T.Text -> IO ()
writeReadme table = do
  preface <- T.readFile "assets/README_template.md"
  T.writeFile "README.md" $ preface <> table
