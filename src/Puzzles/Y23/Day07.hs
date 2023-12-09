module Puzzles.Y23.Day07
  ( day07aSolve,
    day07bSolve,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char

data Hand = HighCard | Pair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Eq, Ord, Show)

day07aSolve :: PuzzleSolve [([Int], Int)] Int
day07aSolve =
  PuzzleSolve
    { _parse = parse' False,
      _solve = day07
    }

day07bSolve :: PuzzleSolve [([Int], Int)] Int
day07bSolve =
  PuzzleSolve
    { _parse = parse' True,
      _solve = day07
    }

parse' :: Bool -> T.Text -> [([Int], Int)]
parse' playingWithJokers = parseInput $ parseLine `sepEndBy` newline
  where
    parseLine :: Parser ([Int], Int)
    parseLine = do
      cards <- many parseCard
      hspace
      bid <- parseInt
      return (cards, bid)

    parseCard :: Parser Int
    parseCard = do
      choice
        [ read . (: []) <$> digitChar,
          10 <$ char 'T',
          (if playingWithJokers then 0 else 11) <$ char 'J',
          12 <$ char 'Q',
          13 <$ char 'K',
          14 <$ char 'A'
        ]

classify :: [Int] -> Hand
classify cards =
  f
    . filter (> 1)
    . addJokers jokers
    . L.sortOn negate
    . fmap length
    . L.group
    . L.sort
    . filter (/= 0)
    $ cards
  where
    f :: [Int] -> Hand
    f [5] = FiveKind
    f [4] = FourKind
    f [3, 2] = FullHouse
    f [3] = ThreeKind
    f [2, 2] = TwoPair
    f [2] = Pair
    f _ = HighCard

    jokers :: Int
    jokers = length $ filter (== 0) cards

    -- Don't have to think too hard about the specifics of the combinations -
    -- in every case, you get the best hand by adding the jokers to the most numerous group.
    addJokers :: Int -> [Int] -> [Int]
    addJokers j (x : xs) = x + j : xs
    addJokers j [] = [j]

day07 :: [([Int], Int)] -> Int
day07 = sum . zipWith (\ix (_, _, b) -> ix * b) [1 ..] . L.sortBy f . fmap addHand
  where
    addHand :: ([Int], Int) -> (Hand, [Int], Int)
    addHand (cards, bid) = (classify cards, cards, bid)

    f :: (Hand, [Int], Int) -> (Hand, [Int], Int) -> Ordering
    f (h1, c1, _) (h2, c2, _) =
      case compare h1 h2 of
        EQ -> compare c1 c2
        _ -> compare h1 h2
