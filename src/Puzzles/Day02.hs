module Puzzles.Day02
  ( day02aSolve,
    day02bSolve,
  )
where

import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day02aSolve :: PuzzleSolve [Round] Int
day02aSolve =
  PuzzleSolve
    { _parse = parse' (zipParser [('X', Rock), ('Y', Paper), ('Z', Scissors)]),
      _solve = day02a
    }

day02bSolve :: PuzzleSolve [(Move, Outcome)] Int
day02bSolve =
  PuzzleSolve
    { _parse = parse' (zipParser [('X', Lose), ('Y', Draw), ('Z', Win)]),
      _solve = day02b
    }

zipParser :: [(Char, a)] -> Parser a
zipParser xs = choice [out <$ char inp | (inp, out) <- xs]

parse' :: Parser a -> T.Text -> [(Move, a)]
parse' parseSecondColumn = parseInput $ parseRound `sepEndBy1` newline
  where
    parseRound = do
      first <- zipParser [('A', Rock), ('B', Paper), ('C', Scissors)]
      _ <- char ' '
      second <- parseSecondColumn
      return (first, second)

data Move = Rock | Paper | Scissors deriving (Enum, Eq)

succ' :: Move -> Move
succ' Scissors = Rock
succ' m = succ m

pred' :: Move -> Move
pred' Rock = Scissors
pred' m = pred m

type Round = (Move, Move)

data Outcome = Lose | Draw | Win

score :: Round -> Int
score rnd = moveScore (snd rnd) + outcomeScore rnd
  where
    moveScore = \case
      Rock -> 1
      Paper -> 2
      Scissors -> 3

    outcomeScore (theirs, mine)
      | mine == succ' theirs = 6
      | mine == theirs = 3
      | otherwise = 0

day02a :: [Round] -> Int
day02a = sum . map score

day02b :: [(Move, Outcome)] -> Int
day02b = sum . map (score . ((,) <$> fst <*> followStrategy))
  where
    followStrategy :: (Move, Outcome) -> Move
    followStrategy (move, outcome) =
      case outcome of
        Lose -> pred' move
        Draw -> move
        Win -> succ' move
