module Lib.Utils
  ( applyToElem,
    bindN,
    boolToInt,
    pairMap,
    trim,
  )
where

import Control.Monad
import Data.Char (isSpace)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- | Repeat monadic bind `n` times
bindN :: (Monad m, Integral n) => (a -> m a) -> n -> (a -> m a)
bindN f n = foldr (>=>) return (replicate (fromIntegral n) f)

-- | Like <$> for a 2-tuple
pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

-- | Trim whitespace from the beginning and end of a string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Apply a function to the element of a list
applyToElem :: Int -> (a -> a) -> [a] -> [a]
applyToElem n f xs = before ++ [f $ head after] ++ tail after
  where
    (before, after) = splitAt n xs
