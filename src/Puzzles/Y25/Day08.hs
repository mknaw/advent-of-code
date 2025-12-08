module Puzzles.Y25.Day08
  ( day08aSolve,
    day08bSolve,
  )
where

import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import qualified Data.Heap as H
import Data.List (sortOn, unfoldr)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils (both, uniquePairs)
import Linear.V3
import Puzzles.Puzzles
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)

-- TODO test case is broken for this one, since it needs different param
-- vs even part A.
day08aSolve :: PuzzleSolve [V3 Int] Int
day08aSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = uncurry solveA . mkDataStructures
    }

day08bSolve :: PuzzleSolve [V3 Int] Int
day08bSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = uncurry solveB . mkDataStructures
    }

parse :: T.Text -> [V3 Int]
parse = parseInput $ parseV3 `sepEndBy` newline

-- Don't need to waste time normalizing (applying square root)
(<->) :: V3 Int -> V3 Int -> Int
V3 x1 y1 z1 <-> V3 x2 y2 z2 = sum . fmap (^ (2 :: Int)) $ [x1 - x2, y1 - y2, z1 - z2]

-- TODO probably best to take a stab at the correct forest implementation,
-- and not the hacky fwd/rev map approach

data UnionFind a = UnionFind
  { _fwd :: M.Map a Int,
    _rev :: M.Map Int (S.Set a)
  }
  deriving (Show)

mkUnionFind :: (Ord a) => [a] -> UnionFind a
mkUnionFind xs = UnionFind {_fwd = fwd, _rev = rev}
  where
    fwd = M.fromList $ zip xs [1 ..]
    rev = M.fromList $ zip [1 ..] (fmap S.singleton xs)

union :: (Ord a) => UnionFind a -> a -> a -> UnionFind a
union uf a b
  | ga == gb = uf -- Already in same group
  | otherwise = uf {_fwd = fwd', _rev = rev'}
  where
    ga = _fwd uf ! a
    gb = _fwd uf ! b
    -- Update all elements in group gb to point to ga
    elementsInGb = _rev uf ! gb
    fwd' = foldl' (\m e -> M.adjust (const ga) e m) (_fwd uf) elementsInGb
    rev' = M.delete gb . M.adjust (S.union elementsInGb) ga $ _rev uf

isContiguous :: UnionFind a -> Bool
isContiguous uf = M.size (_rev uf) == 1

type Pair = (V3 Int, V3 Int)

type PairHeap = H.MinPrioHeap Int Pair

mkPairHeap :: [V3 Int] -> PairHeap
mkPairHeap ps = foldl' (flip H.insert) H.empty (fmap (uncurry (<->) &&& id) . uniquePairs $ ps)

mkDataStructures :: [V3 Int] -> (UnionFind (V3 Int), PairHeap)
mkDataStructures = mkUnionFind &&& mkPairHeap

solveA :: UnionFind (V3 Int) -> PairHeap -> Int
solveA uf =
  product
    . take 3
    . sortOn Down
    . fmap S.size
    . M.elems
    . _rev
    -- Apply 1000 first pairs to the `UnionFind`
    . foldl' (\u (a, b) -> union u a b) uf
    . take 1000
    -- Pairs in order of increasing distance
    . fmap snd
    . unfoldr H.view

solveB :: UnionFind (V3 Int) -> PairHeap -> Int
solveB = ((uncurry (*) . both (^. _x)) .) . go
  where
    go :: UnionFind (V3 Int) -> PairHeap -> Pair
    go uf ph =
      let ((_, (a, b)), ph') = fromJust . H.view $ ph
          uf' = union uf a b
       in if isContiguous uf'
            then (a, b)
            else go uf' ph'
