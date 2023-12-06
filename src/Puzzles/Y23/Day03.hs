module Puzzles.Y23.Day03
  ( day03aSolve,
    day03bSolve,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Char
import Data.IntervalMap hiding (filter)
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib.Utils.Geometry (Point)
import Linear
import Puzzles.Puzzles

type IMap = IM.IntervalMap (Interval Int) Int

type Symbols = [(Point, Char)]

day03aSolve :: PuzzleSolve (Symbols, V.Vector IMap) Int
day03aSolve =
  PuzzleSolve
    { _parse = parse',
      -- Earlier had a solution for this with a windowed iteration over rows that was faster for this part,
      -- but did not translate well to part b, so since removed. For elegance, I can tolerate slowness.
      _solve = day03a
    }

day03bSolve :: PuzzleSolve (Symbols, V.Vector IMap) Int
day03bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day03b
    }

parse' :: T.Text -> (Symbols, V.Vector IMap)
parse' input = (concat (zipWith makeMap [0 ..] symbols), V.fromList nums)
  where
    (symbols, nums) = unzip . fmap parseRow . lines . T.unpack $ input

    parseRow :: String -> ([(Int, Char)], IMap)
    parseRow = go 0 ([], IM.empty)
      where
        go :: Int -> ([(Int, Char)], IMap) -> String -> ([(Int, Char)], IMap)
        go _ acc "" = acc
        go i acc ('.' : cs) = go (i + 1) acc cs
        go i (syms, xs) (c : cs)
          | isDigit c =
            let (d, rest) = span isDigit (c : cs)
                n = read d
                i' = i + length d
                xs' = IM.insert (ClosedInterval i (i' - 1)) n xs
             in go i' (syms, xs') rest
          | otherwise = go (i + 1) ((i, c) : syms, xs) cs

    makeMap :: Int -> [(Int, Char)] -> Symbols
    makeMap i = fmap (first (V2 i))

-- TODO this stuff is still pretty gross ...

type HitHandler = Interval Int -> IMap -> IMap

collectAdjacent :: HitHandler -> V.Vector IMap -> [Point] -> [[Int]]
collectAdjacent h numbers = fst . L.foldl' go ([], numbers)
  where
    go :: ([[Int]], V.Vector IMap) -> Point -> ([[Int]], V.Vector IMap)
    go (acc, nums) (V2 x y) = (acc <> [concat hits], V.update nums (V.fromList $ zip rowIxs rows'))
      where
        rowIxs = filter (\k -> k >= 0 && k < V.length nums) [x - 1, x, x + 1]
        rows = (nums V.!) <$> rowIxs
        (hits, rows') = unzip $ flip search [y - 1, y, y + 1] <$> rows

    search :: IMap -> [Int] -> ([Int], IMap)
    search m = L.foldl' f ([], m) . IM.toList . L.foldl1' IM.union . fmap (IM.containing m)
      where
        f :: ([Int], IMap) -> (Interval Int, Int) -> ([Int], IMap)
        f (acc, m') (i', x) = (x : acc, h i' m')

day03a :: (Symbols, V.Vector IMap) -> Int
day03a (symbols, numbers) = sum . fmap sum . collectAdjacent IM.delete numbers . fmap fst $ symbols

day03b :: (Symbols, V.Vector IMap) -> Int
day03b (symbols, numbers) =
  sum
    . fmap product
    . filter ((== 2) . length)
    . collectAdjacent (\_ m -> m) numbers
    . fmap fst
    . filter ((== '*') . snd)
    $ symbols
