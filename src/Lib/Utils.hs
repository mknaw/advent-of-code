module Lib.Utils
  ( applyToElem,
    bindN,
    boolToInt,
    both,
    indicesWhere,
    pairify,
    pairMap,
    trim,
    unpairify,
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

-- | Convert length 2 list to a pair tuple
pairify :: [a] -> (a, a)
pairify [x, y] = (x, y)
pairify _ = error "pairify: list must have exactly two elements"

-- | Convert a pair to a length 2 list
unpairify :: (a, a) -> [a]
unpairify (x, y) = [x, y]

-- | Apply a function to both elements in a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

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

-- | Get the indices of all elements that satisfy a predicate
indicesWhere :: (a -> Bool) -> [a] -> [Int]
indicesWhere f xs = map fst $ filter (f . snd) $ zip [0..] xs
