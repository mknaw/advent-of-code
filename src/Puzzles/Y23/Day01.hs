module Puzzles.Y23.Day01
  ( day01aSolve,
    day01bSolve,
  )
where

import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Puzzles.Puzzles

day01aSolve :: PuzzleSolve [[Int]] Int
day01aSolve =
  PuzzleSolve
    { _parse = makeParser parseA,
      _solve = solve
    }

day01bSolve :: PuzzleSolve [[Int]] Int
day01bSolve =
  PuzzleSolve
    { _parse = makeParser parseB,
      _solve = solve
    }

makeParser :: (String -> Maybe Int) -> T.Text -> [[Int]]
makeParser f = fmap p . T.lines
  where
    p = mapMaybe f . filter (/= "") . fmap T.unpack . T.tails

parseA :: String -> Maybe Int
parseA s = if isDigit (head s) then Just (digitToInt (head s)) else Nothing

-- TODO could be better to use a trie
parseB :: String -> Maybe Int
parseB s
  | "zero" `isPrefixOf` s = Just 0
  | "one" `isPrefixOf` s = Just 1
  | "two" `isPrefixOf` s = Just 2
  | "three" `isPrefixOf` s = Just 3
  | "four" `isPrefixOf` s = Just 4
  | "five" `isPrefixOf` s = Just 5
  | "six" `isPrefixOf` s = Just 6
  | "seven" `isPrefixOf` s = Just 7
  | "eight" `isPrefixOf` s = Just 8
  | "nine" `isPrefixOf` s = Just 9
  | otherwise = parseA s

solve :: [[Int]] -> Int
solve = sum . map (\xs -> 10 * head xs + last xs)
