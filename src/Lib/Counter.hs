module Lib.Counter (
  Counter,
  decrement,
  increment,
) where

import qualified Data.Map as M

type Counter a = M.Map a Int

increment :: Ord k => k -> Counter k -> Counter k
increment k = M.insertWith (+) k 1

decrement :: Ord k => k -> Counter k -> Counter k
decrement = M.adjust (max 0 . subtract 1)
