module Puzzles.Y23.Day14
  ( day14aSolve,
    day14bSolve,
  )
where

import Control.Lens
import qualified Data.List as L
import qualified Data.List.Safe as Safe
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple.Extra ((&&&))
import Lib.Utils (invertMap)
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles

data Rock = Rolling | Fixed deriving (Show, Eq)

type RockMap = M.Map Point Rock

day14aSolve :: PuzzleSolve (Point, RockMap) Int
day14aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = uncurry day14a
    }

day14bSolve :: PuzzleSolve (Point, RockMap) Int
day14bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = uncurry day14b
    }

parse' :: T.Text -> (Point, RockMap)
parse' = (gridSizeOf &&& parseGridToMap parseRock) . T.unpack
  where
    parseRock :: Char -> Maybe Rock
    parseRock '#' = Just Fixed
    parseRock 'O' = Just Rolling
    parseRock _ = Nothing

advance :: V2 Int -> Direction -> RockMap -> Point -> RockMap
advance dims dir rm p = M.insert p' Rolling rm'
  where
    rm' = M.delete p rm
    ps = takeWhile isValid . iterate (+ directionToV2 dir) $ p
    p' = fromMaybe p $ Safe.last ps

    isValid :: Point -> Bool
    isValid pt = inBounds dims pt && M.notMember pt rm'

advanceAll :: V2 Int -> RockMap -> Direction -> RockMap
advanceAll dims rm dir = L.foldl' (advance dims dir) rm (rolling dir rm)

rolling :: Direction -> RockMap -> [Point]
rolling dir = L.sortOn f . M.keys . M.filter (== Rolling)
  where
    f = case dir of
      N -> (^. _y)
      S -> negate . (^. _y)
      E -> negate . (^. _x)
      W -> (^. _x)

points :: RockMap -> S.Set Point
points = M.keysSet . M.filter (== Rolling)

score :: V2 Int -> S.Set Point -> Int
score (V2 _ y) = sum . fmap ((y -) . (^. _y)) . S.toList

day14a :: V2 Int -> RockMap -> Int
day14a dims rm = score dims . points $ advanceAll dims rm N

rollCycle :: V2 Int -> RockMap -> RockMap
rollCycle dims rm = L.foldl' (advanceAll dims) rm [N, W, S, E]

day14b :: V2 Int -> RockMap -> Int
day14b dims = go 0 M.empty
  where
    total :: Int
    total = 1000000000

    go :: Int -> M.Map (S.Set Point) Int -> RockMap -> Int
    go i seen rm
      | i == total = score dims . points $ rm'
      | otherwise = case M.lookup (M.keysSet . M.filter (== Rolling) $ rm) seen of
        Just j -> score dims . fromJust . M.lookup (j + ((total - i) `rem` (i - j))) $ inverted
        Nothing -> go (i + 1) seen' rm'
      where
        rm' = rollCycle dims rm
        seen' = M.insert (points rm) i seen
        inverted = invertMap seen
