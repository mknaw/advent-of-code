module Puzzles.Day03
  ( day03aSolve,
    day03bSolve,
  )
where

import Data.Char (ord)
import Data.List (foldl1')
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Utils
import Puzzles.Puzzles

type Rucksack = (Set Char, Set Char)

day03aSolve :: PuzzleSolve [Rucksack] Int
day03aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day03a
    }

day03bSolve :: PuzzleSolve [Rucksack] Int
day03bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day03b
    }

parse' :: T.Text -> [Rucksack]
parse' = map (parseRucksack . T.unpack) . T.lines
  where
    parseRucksack xs = pairMap S.fromList $ splitAt (length xs `div` 2) xs

day03a :: [Rucksack] -> Int
day03a = sum . map (prioritize . setHead . uncurry S.intersection)

day03b :: [Rucksack] -> Int
day03b =
  sum
    . map (prioritize . setHead . foldl1' S.intersection)
    . chunksOf 3
    . map (uncurry S.union)

setHead :: Set a -> a
setHead = S.elemAt 0

prioritize :: Char -> Int
prioritize c
  | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27
  | otherwise = error $ "received unexpected char: " ++ show c
