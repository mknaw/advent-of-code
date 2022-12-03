module Lib.Utils
  ( pairMap,
  )
where

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)
