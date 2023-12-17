module Puzzles.Y23.Day15
  ( day15aSolve,
    day15bSolve,
  )
where

import Data.Char (ord)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.MemoTrie (memo)
import qualified Data.Text as T
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char

data Op = Pop | Set Int deriving (Eq, Show)

data Step = Step
  { label :: String,
    op :: Op
  }
  deriving (Show)

type Boxes = V.Vector [(String, Int)]

day15aSolve :: PuzzleSolve [String] Int
day15aSolve =
  PuzzleSolve
    { _parse = splitOn "," . T.unpack . T.strip,
      _solve = sum . fmap hash
    }

day15bSolve :: PuzzleSolve [Step] Int
day15bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day15b
    }

parse' :: T.Text -> [Step]
parse' = parseInput (parseStep `sepBy1` char ',') . T.strip
  where
    parseStep :: Parser Step
    parseStep = do
      label <- some lowerChar
      op <- choice [Pop <$ char '-', Set <$> (char '=' *> parseInt)]
      return Step {label, op}

hash' :: String -> Int
hash' = L.foldl' go 0
  where
    go :: Int -> Char -> Int
    go curr x = (17 * (curr + ord x)) `rem` 256

hash :: String -> Int
hash = memo hash'

score :: Boxes -> Int
score = sum . L.zipWith f [1..] . V.toList
  where
    f :: Int -> [(String, Int)] -> Int
    f k box = k * (sum . L.zipWith g [1..] $ box)

    g :: Int -> (String, Int) -> Int
    g k (_, x) = k * x

execPop :: Boxes -> String -> Boxes
execPop boxes label = boxes // [(i, box')]
  where
    i = hash label
    box' = filter ((/= label) . fst) $ boxes ! i

execSet :: Boxes -> String -> Int -> Boxes
execSet boxes label k = boxes // [(i, box')]
  where
    i = hash label
    updateBox :: [(String, Int)] -> [(String, Int)]
    updateBox ((l, x) : rest) =
      if l == label
        then (l, k) : rest
        else (l, x) : updateBox rest
    updateBox [] = [(label, k)]

    box' = updateBox $ boxes ! i

day15b :: [Step] -> Int
day15b = score . L.foldl' go (V.fromList . replicate 256 $ [])
  where
    go :: Boxes -> Step -> Boxes
    go boxes (Step label op) = case op of
      Pop -> execPop boxes label
      Set k -> execSet boxes label k
