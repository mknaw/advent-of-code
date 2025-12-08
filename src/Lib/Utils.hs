module Lib.Utils
  ( allDisjoint,
    applyToElem,
    bindN,
    boolToInt,
    both,
    countTrue,
    dropSpaces,
    indicesWhere,
    invertMap,
    fixedPoint,
    pairify,
    roundUpDiv,
    shift,
    subsets,
    symmetricDifference,
    toSnd,
    trim,
    uncurry3,
    uniquePairs,
    unpairify,
  )
where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Set ((\\))
import qualified Data.Set as S

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

-- | Trim whitespace from the beginning and end of a string
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | Apply a function to the element of a list
applyToElem :: Int -> (a -> a) -> [a] -> [a]
applyToElem n f xs = before ++ [f $ head after] ++ tail after
  where
    (before, after) = splitAt n xs

-- | Get the indices of all elements that satisfy a predicate
indicesWhere :: (a -> Bool) -> [a] -> [Int]
indicesWhere f xs = map fst $ filter (f . snd) $ zip [0 ..] xs

-- | Generate all subsets of a certain size
subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

-- | Check if all sets in a list are disjoint
allDisjoint :: (Ord a) => [S.Set a] -> Bool
allDisjoint = go S.empty
  where
    go _ [] = True
    go s (x : xs) = S.disjoint s x && go (S.union s x) xs

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

roundUpDiv :: Int -> Int -> Int
roundUpDiv x y
  | x `mod` y == 0 = x `div` y
  | otherwise = x `div` y + 1

shift :: [a] -> [a]
shift [] = []
shift (x : xs) = xs ++ [x]

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

symmetricDifference :: (Ord a) => S.Set a -> S.Set a -> S.Set a
symmetricDifference x y = (x \\ y) <> (y \\ x)

invertMap :: (Ord k, Ord v) => M.Map k v -> M.Map v k
invertMap = M.fromList . map (\(k, v) -> (v, k)) . M.toList

countTrue :: (a -> Bool) -> [a] -> Int
countTrue = (length .) . filter

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x
  | x == f x = x
  | otherwise = fixedPoint f (f x)

dropSpaces :: String -> String
dropSpaces = filter $ not . isSpace

uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x : xs) = [(x, y) | y <- xs] ++ uniquePairs xs
