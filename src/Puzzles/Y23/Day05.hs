module Puzzles.Y23.Day05
  ( day05aSolve,
    day05bSolve,
  )
where

import Data.Functor hiding (unzip)
import qualified Data.IntegerInterval as II
import Data.Interval as I
import qualified Data.IntervalSet as IS
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import Lib.Utils (both)

type Maps a = [(Interval a, a)]

day05aSolve :: PuzzleSolve (IS.IntervalSet Integer, [Maps Integer]) Int
day05aSolve =
  PuzzleSolve
    { _parse = parse' (IS.fromList . fmap I.singleton),
      _solve = uncurry day05
    }

day05bSolve :: PuzzleSolve (IS.IntervalSet Integer, [Maps Integer]) Int
day05bSolve =
  PuzzleSolve
    { _parse = parse' intervalize,
      _solve = uncurry day05
    }
  where
    intervalize :: [Integer] -> IS.IntervalSet Integer
    intervalize (x : d : rest) = IS.insert (Finite x <=..< Finite (x + d)) (intervalize rest)
    intervalize _ = IS.empty

parse' ::
  ([Integer] -> IS.IntervalSet Integer) ->
  T.Text ->
  (IS.IntervalSet Integer, [Maps Integer])
parse' intervalizer = parseInput $ do
  string "seeds:"
  space
  seeds <- (toInteger <$> parseInt) `sepEndBy1` space
  maps <- many $ do
    optional (void . many . noneOf $ ['0' .. '9'])
    parseMap `sepEndBy1` newline
  return (intervalizer seeds, maps)
  where
    parseMap :: Parser (Interval Integer, Integer)
    parseMap = do
      y <- toInteger <$> parseInt
      hspace
      x <- toInteger <$> parseInt
      hspace
      d <- toInteger <$> parseInt
      return (Finite x <=..<= Finite (x + d - 1), y - x)

day05 :: IS.IntervalSet Integer -> [Maps Integer] -> Int
day05 seeds =
  fromInteger . head
    . mapMaybe (II.simplestIntegerWithin . II.fromInterval)
    . IS.toAscList
    . L.foldl' advance seeds
  where
    advance :: (Ord a, Num a) => IS.IntervalSet a -> Maps a -> IS.IntervalSet a
    advance ranges ms = IS.union toAdd $ IS.difference ranges toDrop
      where
        (toDrop, toAdd) = both IS.unions . unzip . fmap (apply ranges) $ ms

    apply :: (Ord a, Num a) => IS.IntervalSet a -> (Interval a, a) -> (IS.IntervalSet a, IS.IntervalSet a)
    apply ranges (src, offset) = (toDrop, toAdd)
      where
        toDrop = IS.intersection (IS.singleton src) ranges
        -- have to convert to and from list because `IntervalSet` is not a functor
        toAdd = IS.fromList ((+ I.singleton offset) <$> IS.toList toDrop)
