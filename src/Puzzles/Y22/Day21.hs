module Puzzles.Y22.Day21
  ( day21aSolve,
    day21bSolve,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (reverse)

type RawDatum = Either (String, Operator, String) Int

type RawData = M.Map String RawDatum

day21aSolve :: PuzzleSolve RawData Int
day21aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day21a
    }

day21bSolve :: PuzzleSolve RawData Int
day21bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day21b
    }

data Operator = Add | Sub | Mult | Div deriving (Eq, Show)

data Expression
  = Const Int
  | Operation
      { _lhs :: Expression,
        _operator :: Operator,
        _rhs :: Expression
      }
  | Unknown
  deriving (Eq, Show)

parse' :: T.Text -> RawData
parse' = parseInput . fmap M.fromList $ parseMonkey `sepEndBy1` newline
  where
    parseName :: Parser String
    parseName = count 4 letterChar

    parseMonkey :: Parser (String, RawDatum)
    parseMonkey = do
      name <- parseName
      string ": "
      expr <- (Right <$> try parseInt) <|> (Left <$> parseOperation)
      return (name, expr)

    parseOperation :: Parser (String, Operator, String)
    parseOperation = do
      lhs <- parseName
      spaceChar
      operator <- choice [Add <$ char '+', Sub <$ char '-', Mult <$ char '*', Div <$ char '/']
      spaceChar
      rhs <- parseName
      return (lhs, operator, rhs)

resolveRefs :: RawData -> Expression
resolveRefs raw = resolveRefs' raw (raw M.! "root")

resolveRefs' :: RawData -> RawDatum -> Expression
resolveRefs' _ (Right x) = Const x
resolveRefs' monkeys (Left (lhs, op, rhs)) =
  Operation
    { _lhs = resolve lhs,
      _operator = op,
      _rhs = resolve rhs
    }
  where
    resolve s = case s `M.lookup` monkeys of
      Just s' -> resolveRefs' monkeys s'
      Nothing -> Unknown

eval :: Expression -> Expression
eval Unknown = Unknown
eval (Const x) = Const x
eval (Operation (Const x) Add (Const y)) = Const $ x + y
eval (Operation (Const x) Sub (Const y)) = Const $ x - y
eval (Operation (Const x) Mult (Const y)) = Const $ x * y
eval (Operation (Const x) Div (Const y)) = Const $ x `div` y
eval (Operation lhs operator rhs) =
  -- Guard from infinitely recursing a non-resolvable operation.
  case (lhs', rhs') of
    (Const _, Const _) -> eval op
    _ -> op
  where
    lhs' = eval lhs
    rhs' = eval rhs
    op = Operation lhs' operator rhs'

day21a :: RawData -> Int
day21a raw =
  case eval (resolveRefs raw) of
    Const x -> x
    e -> error (show e)

modifyInstructions :: RawData -> RawData
modifyInstructions = M.delete "humn"

reverse :: Expression -> Expression -> Int
reverse (Const x) Unknown = x
reverse expr (Const x) = reverse (Const x) expr
-- Add
reverse (Const x) (Operation (Const a) Add expr) =
  reverse (eval $ Operation (Const x) Sub (Const a)) expr
reverse (Const x) (Operation expr Add (Const a)) =
  reverse (eval $ Operation (Const x) Sub (Const a)) expr
-- Sub
reverse (Const x) (Operation (Const a) Sub expr) =
  reverse (eval $ Operation (Const a) Sub (Const x)) expr
reverse (Const x) (Operation expr Sub (Const a)) =
  reverse (eval $ Operation (Const a) Add (Const x)) expr
-- Mult
reverse (Const x) (Operation (Const a) Mult expr) =
  reverse (eval $ Operation (Const x) Div (Const a)) expr
reverse (Const x) (Operation expr Mult (Const a)) =
  reverse (eval $ Operation (Const x) Div (Const a)) expr
-- Div
reverse (Const x) (Operation (Const a) Div expr) =
  reverse (eval $ Operation (Const a) Div (Const x)) expr
reverse (Const x) (Operation expr Div (Const a)) =
  reverse (eval $ Operation (Const x) Mult (Const a)) expr
reverse _ _ = error "Expect one side of the equation to be known"

day21b :: RawData -> Int
day21b raw = reverse (_lhs expr) (_rhs expr)
  where
    expr = eval $ resolveRefs (modifyInstructions raw)
