module Puzzles.Y22.Day20
  ( day20aSolve,
    day20bSolve,
  )
where

import Data.Foldable (toList)
import qualified Data.Maybe as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

type Ints = Seq.Seq Int

day20aSolve :: PuzzleSolve Ints Int
day20aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day20a
    }

day20bSolve :: PuzzleSolve Ints Int
day20bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day20b
    }

parse' :: T.Text -> Ints
parse' = parseInput . fmap Seq.fromList $ parseInt `sepEndBy` newline

removeAt :: Int -> Ints -> (Int, Ints)
removeAt i xs = (xs `Seq.index` i, Seq.deleteAt i xs)

advance :: (Ints, Ints) -> Int -> (Ints, Ints)
advance (idxs, xs) idx = (Seq.insertAt i' idx idxs', Seq.insertAt i' x xs')
  where
    i = M.fromJust $ Seq.elemIndexL idx idxs
    (x, xs') = removeAt i xs
    i' = (i + x) `mod` Seq.length xs'
    idxs' = Seq.deleteAt i idxs

initialIdxs :: Ints -> Ints
initialIdxs xs = Seq.fromList [0 .. Seq.length xs - 1]

mix :: (Ints, Ints) -> (Ints, Ints)
mix (idxs, xs) = (idxs', xs')
  where
    n = Seq.length xs
    (idxs', xs') = foldl advance (idxs, xs) [0 .. n - 1]

groveSum :: Ints -> Int
groveSum xs = sum [xs' !! 1000, xs' !! 2000, xs' !! 3000]
  where
    xs' = dropWhile (/= 0) . cycle . toList $ xs

day20a :: Ints -> Int
day20a xs = groveSum . snd . mix $ (initialIdxs xs, xs)

key :: Int
key = 811589153

day20b :: Ints -> Int
day20b xs = groveSum . snd $ iterate mix (initialIdxs xs', xs') !! 10
  where
    xs' = fmap (* key) xs
