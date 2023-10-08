module Puzzles.Y22.Day11
  ( day11aSolve,
    day11bSolve,
  )
where

import Control.Monad (void)
import qualified Data.List as L
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day11aSolve :: PuzzleSolve [Monkey] Int
day11aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day11a
    }

day11bSolve :: PuzzleSolve [Monkey] Int
day11bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day11b
    }

data Monkey = Monkey
  { _holds :: [Int],
    _op :: Operation,
    _divisbleBy :: Int,
    _trueMonkey :: Int,
    _falseMonkey :: Int,
    _count :: Int
  }
  deriving (Show)

data Var = Var | Const Int deriving (Show)

data Operator = Add | Mult deriving (Show)

data Operation = Operation
  { _operator :: Operator,
    _rhs :: Var
  }
  deriving (Show)

parse' :: T.Text -> [Monkey]
parse' = parseInput $ parseMonkey `sepEndBy1` newline
  where
    parseMonkey = do
      string "Monkey "
      digitChar
      char ':'
      newline
      string "  Starting items: "
      holds <- parseInt `sepBy` string ", "
      newline
      some spaceChar
      string "Operation: new = old "
      op <- parseOp
      -- TODO from here on out it seems like we could just try to isolate 3 ints
      newline
      string "  Test: divisible by "
      divisibleBy <- parseInt
      newline
      string "    If true: throw to monkey "
      trueMonkey <- parseInt
      newline
      string "    If false: throw to monkey "
      falseMonkey <- parseInt
      void newline <|> eof

      return
        Monkey
          { _holds = holds,
            _op = op,
            _divisbleBy = divisibleBy,
            _trueMonkey = trueMonkey,
            _falseMonkey = falseMonkey,
            _count = 0
          }

    parseOp :: Parser Operation
    parseOp = do
      operator <- choice [Add <$ char '+', Mult <$ char '*']
      spaceChar
      rhs <- choice [Var <$ string "old", Const <$> parseInt]
      return $ Operation operator rhs

eval :: Operation -> Int -> Int
eval (Operation Add (Const k)) x = x + k
eval (Operation Add Var) x = eval (Operation Add (Const x)) x
eval (Operation Mult (Const k)) x = x * k
eval (Operation Mult Var) x = eval (Operation Mult (Const x)) x

increaseCount :: Monkey -> Monkey
increaseCount m = m {_count = _count m + 1}

takeFrom :: Monkey -> Monkey
takeFrom m = m {_holds = tail (_holds m)}

throwTo :: Int -> Monkey -> Monkey
throwTo k m = m {_holds = _holds m ++ [k]}

processMonkey :: Int -> Int -> [Monkey] -> Int -> [Monkey]
processMonkey q worryFactor monkeys i
  | null $ _holds monkey = monkeys
  | otherwise = processMonkey q worryFactor next i
  where
    monkey = monkeys !! i
    processed = (`mod` q) . (`div` worryFactor) . eval (_op monkey) . head . _holds $ monkey
    target =
      if processed `mod` _divisbleBy monkey == 0
        then _trueMonkey monkey
        else _falseMonkey monkey
    next = applyToElem i (increaseCount . takeFrom) $ applyToElem target (throwTo processed) monkeys

processTurn :: Int -> Int -> [Monkey] -> [Monkey]
processTurn q worryFactor monkeys = foldl (processMonkey q worryFactor) monkeys [0 .. length monkeys - 1]

day11a :: [Monkey] -> Int
day11a monkeys = product . take 2 . reverse . L.sort . map _count . (!! 20) . iterate (processTurn q 3) $ monkeys
  where
    q = product . map _divisbleBy $ monkeys

day11b :: [Monkey] -> Int
day11b monkeys = product . take 2 . reverse . L.sort . map _count . (!! 10000) . iterate (processTurn q 1) $ monkeys
  where
    q = product . map _divisbleBy $ monkeys
