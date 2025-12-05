module Puzzles.Y25.Day04
  ( day04aSolve,
    day04bSolve,
  )
where

import Control.Arrow ((&&&))
import Control.Monad (forM)
import Control.Monad.State
import qualified Data.Map as M hiding ((!))
import qualified Data.Matrix as M hiding (fromList)
import Data.Maybe
import qualified Data.Text as T
import Lib.Matrix hiding (countTrue)
import Puzzles.Puzzles

day04aSolve :: PuzzleSolve (M.Matrix Bool) Int
day04aSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = solveA
    }

day04bSolve :: PuzzleSolve (M.Matrix Bool) Int
day04bSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = solveB
    }

type AdjMap = M.Map Coord [Coord]

parse :: T.Text -> M.Matrix Bool
parse = M.fromLists . map (map (== '@')) . lines . T.unpack

makeAdjMap :: M.Matrix Bool -> AdjMap
makeAdjMap mat =
  M.fromList
    . map (id &&& (keepTrue . neighbors8 mat))
    . keepTrue
    . coords
    $ mat
  where
    keepTrue :: [Coord] -> [Coord]
    keepTrue = filter (mat M.!)

filterValid :: AdjMap -> AdjMap
filterValid = M.filter ((< 4) . length)

solveA :: M.Matrix Bool -> Int
solveA = M.size . filterValid . makeAdjMap

updateAndCollect :: Coord -> State AdjMap [Coord]
updateAndCollect c = do
  a <- get
  let neighbs = fromMaybe [] (M.lookup c a)
  modify' (M.delete c)
  fmap concat $ forM neighbs $ \nb -> do
    modify' (M.update (Just . filter (/= c)) nb)
    a' <- get
    return $ case M.lookup nb a' of
      Just v | length v < 4 -> [nb]
      _ -> []

solveB :: M.Matrix Bool -> Int
solveB mat = M.size adjMap - (M.size . go initQ $ adjMap)
  where
    adjMap :: AdjMap
    adjMap = makeAdjMap mat

    initQ :: [Coord]
    initQ = M.keys . filterValid $ adjMap

    go :: [Coord] -> AdjMap -> AdjMap
    go (c : cs) a = go (cs ++ cs') a'
      where
        (cs', a') = runState (updateAndCollect c) a
    go [] a = a
