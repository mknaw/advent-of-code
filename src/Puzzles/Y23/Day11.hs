module Puzzles.Y23.Day11
  ( day11aSolve,
    day11bSolve,
  )
where

import Control.Lens
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles

type Galaxies = S.Set Point

day11aSolve :: PuzzleSolve (V2 Int, Galaxies) Int
day11aSolve =
  PuzzleSolve
    { _parse = parse' . T.unpack,
      _solve = uncurry (day11 2)
    }

day11bSolve :: PuzzleSolve (V2 Int, Galaxies) Int
day11bSolve =
  PuzzleSolve
    { _parse = parse' . T.unpack,
      _solve = uncurry (day11 1000000)
    }

parse' :: String -> (Point, Galaxies)
parse' input = (dims, galaxies)
  where
    dims = V2 (length . head $ lines input) (length . lines $ input)
    galaxies = parseGridToSet (== '#') input

emptyAlongAxis :: (V2 Int -> Int) -> Int -> Galaxies -> [Int]
emptyAlongAxis f n galaxies = filter (\x -> not . S.member x . S.map f $ galaxies) [0 .. n - 1]

expand :: Int -> V2 Int -> Galaxies -> Galaxies
expand expFactor (V2 xn yn) galaxies =
  expand' (^. _x) (V2 expFactor 0) emptyXs
    . expand' (^. _y) (V2 0 expFactor) emptyYs
    $ galaxies
  where
    emptyXs = emptyAlongAxis (^. _x) xn galaxies
    emptyYs = emptyAlongAxis (^. _y) yn galaxies

    expand' :: (V2 Int -> Int) -> V2 Int -> [Int] -> Galaxies -> Galaxies
    expand' _ _ [] gs = gs
    expand' f d (x : xs) gs = expand' f d xs' gs'
      where
        (before, after) = S.partition ((< x) . f) gs
        xs' = (+ expFactor) <$> xs
        gs' = S.union before (S.map (+ d) after)

day11 :: Int -> V2 Int -> Galaxies -> Int
day11 expFactor dims galaxies = sum [manhattanDistance a b | (a : rest) <- L.tails galaxies', b <- rest]
  where
    galaxies' = S.toList . expand (expFactor - 1) dims $ galaxies
