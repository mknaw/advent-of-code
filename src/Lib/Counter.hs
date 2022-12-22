module Lib.Counter
  ( (!~),
    Counter,
    decrement,
    decrementBy,
    increment,
    incrementBy,
  )
where

import qualified Data.Map as M

type Counter a = M.Map a Int

(!~) :: Ord a => Counter a -> a -> Int
c !~ k = M.findWithDefault 0 k c

incrementBy :: (Ord a) => Int -> a -> Counter a -> Counter a
incrementBy n k = M.insertWith (+) k n

increment :: Ord k => k -> Counter k -> Counter k
increment = incrementBy 1

decrementBy :: Ord k => Int -> k -> Counter k -> Counter k
decrementBy n = M.adjust (max 0 . subtract n)

decrement :: Ord k => k -> Counter k -> Counter k
decrement = decrementBy 1
