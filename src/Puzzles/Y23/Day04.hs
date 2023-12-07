module Puzzles.Y23.Day04
  ( day04aSolve,
    day04bSolve,
  )
where

import Data.Bifunctor (Bifunctor (first))
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

type Card = (S.Set Int, S.Set Int)

day04aSolve :: PuzzleSolve [Card] Int
day04aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day04a
    }

day04bSolve :: PuzzleSolve [Card] Int
day04bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day04b
    }

parse' :: T.Text -> [Card]
parse' = parseInput $ many parseCard
  where
    parseCard = do
      string "Card"
      space
      many digitChar
      char ':'
      space
      winners <- S.fromList <$> parseInt `sepEndBy1` space
      char '|'
      space
      mine <- S.fromList <$> parseInt `sepEndBy1` space
      return (winners, mine)

matches :: Card -> Int
matches = S.size . uncurry S.intersection

day04a :: [Card] -> Int
day04a = sum . fmap ((\k -> if k > 0 then 2 ^ (k - 1) else 0) . matches)

day04b :: [Card] -> Int
day04b = go 0 . zip (repeat 1) . fmap matches
  where
    go :: Int -> [(Int, Int)] -> Int
    go acc [] = acc
    go acc ((i, x) : xs) = go (acc + i) xs'
      where
        (bonus, rest) = splitAt x xs
        xs' = (first (+ i) <$> bonus) ++ rest
