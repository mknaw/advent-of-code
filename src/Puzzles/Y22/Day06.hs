module Puzzles.Y22.Day06
  ( day06aSolve,
    day06bSolve,
  )
where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Lib.Counter
import Puzzles.Puzzles

day06aSolve :: PuzzleSolve T.Text Int
day06aSolve =
  PuzzleSolve
    { _parse = id,
      _solve = day06 4
    }

day06bSolve :: PuzzleSolve T.Text Int
day06bSolve =
  PuzzleSolve
    { _parse = id,
      _solve = day06 14
    }

type SearchState = (Int, Counter Char)

anyDuplicates :: Counter Char -> Bool
anyDuplicates = any (> 1) . M.elems

go :: Int -> T.Text -> State SearchState Int
go _ "" = error "Marker not found!"
go n xs = do
  (i, counter) <- get
  if i < n
    then do
      put (i + 1, increment (xs `T.index` i) counter)
      go n xs
    else do
      if anyDuplicates counter
        then do
          let counter' = increment (xs `T.index` n) $ decrement (T.head xs) counter
          put (i + 1, counter')
          go n (T.tail xs)
        else return i

day06 :: Int -> T.Text -> Int
day06 n input = evalState (go n input) (0, M.empty)
