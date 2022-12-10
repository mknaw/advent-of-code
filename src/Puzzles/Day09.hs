module Puzzles.Day09
  ( day09aSolve,
    day09bSolve,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

day09aSolve :: PuzzleSolve [Move] Int
day09aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day09 2
    }

day09bSolve :: PuzzleSolve [Move] Int
day09bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day09 10
    }

type Move = (Int, Int)

parse' :: T.Text -> [Move]
parse' = parseInput . fmap concat $ parser `sepEndBy` eol
  where
    parser :: Parser [Move]
    parser = do
      d <-
        choice
          [ (0, 1) <$ char 'U',
            (0, -1) <$ char 'D',
            (-1, 0) <$ char 'L',
            (1, 0) <$ char 'R'
          ]
      space
      n <- parseInt
      return $ replicate n d

type Pos = (Int, Int)

type Rope = [Pos]

mkRope :: Int -> Rope
mkRope = flip replicate (0, 0)

moveRope :: Rope -> Move -> Rope
moveRope [] _ = undefined
moveRope ((x, y) : ts) (dx, dy) = h' : (snd . L.mapAccumL go h' $ ts)
  where
    h' = (x + dx, y + dy)
    go a b = let t = propagate a b in (t, t)

propagate :: Pos -> Pos -> Pos
propagate (x, y) t@(tx, ty)
  | max (abs (x - tx)) (abs (y - ty)) < 2 = t
  | otherwise = (moveToward x tx, moveToward y ty)
  where
    moveToward a b = b + signum (a - b)

day09 :: Int -> [Move] -> Int
day09 n = L.length . L.nub . map last . L.scanl' moveRope (mkRope n)
