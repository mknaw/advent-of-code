{-# LANGUAGE TupleSections #-}

module Lib.Utils.Geometry
  ( (==>),
    Direction (..),
    directionToV2,
    drawGridMap,
    drawGridSet,
    gridSizeOf,
    manhattanDistance,
    neighbors4,
    neighbors8,
    oppositeDir,
    parseGridToMap,
    parseGridToSet,
    Point,
    turnCW,
    turnCCW,
    v2toDirection,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Tuple.Extra ((&&&))
import Linear.V2

type Point = V2 Int

manhattanDistance :: Point -> Point -> Int
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

data Direction = N | E | S | W deriving (Eq, Show)

instance Ord Direction where
  compare N N = EQ
  compare N _ = LT
  compare _ N = GT
  compare E E = EQ
  compare E _ = LT
  compare _ E = GT
  compare S S = EQ
  compare S _ = LT
  compare _ S = GT
  compare W W = EQ

v2toDirection :: V2 Int -> Direction
v2toDirection (V2 0 (-1)) = N
v2toDirection (V2 1 0) = E
v2toDirection (V2 0 1) = S
v2toDirection (V2 (-1) 0) = W
v2toDirection _ = error "invalid direction"

directionToV2 :: Direction -> V2 Int
directionToV2 N = V2 0 (-1)
directionToV2 E = V2 1 0
directionToV2 S = V2 0 1
directionToV2 W = V2 (-1) 0

turnCW :: Direction -> Direction
turnCW N = E
turnCW E = S
turnCW S = W
turnCW W = N

turnCCW :: Direction -> Direction
turnCCW N = W
turnCCW E = N
turnCCW S = E
turnCCW W = S

(==>) :: V2 Int -> Direction -> V2 Int
v2 ==> dir = v2 + directionToV2 dir

oppositeDir :: Direction -> Direction
oppositeDir N = S
oppositeDir E = W
oppositeDir S = N
oppositeDir W = E

neighbors4 :: Point -> [Point]
neighbors4 p = [p ==> N, p ==> E, p ==> S, p ==> W]

neighbors8 :: Point -> [Point]
neighbors8 (V2 x y) =
  -- Probably nicer to iterate over some sort of `Direction8`...
  [ V2 (x - 1) (y - 1),
    V2 x (y - 1),
    V2 (x + 1) (y - 1),
    V2 (x - 1) y,
    V2 (x + 1) y,
    V2 (x - 1) (y + 1),
    V2 x (y + 1),
    V2 (x + 1) (y + 1)
  ]

gridSizeOf :: String -> Point
gridSizeOf = uncurry V2 . (length . head . lines &&& length . lines)

-- | Parse a grid of characters into a `Map` of V2 to arbitrary data.
-- TODO should add dims to the return, perhaps
parseGridToMap :: (Char -> Maybe a) -> String -> M.Map Point a
parseGridToMap f =
  M.mapMaybe f
    . M.fromList
    . concat
    . zipWith v2ize [0 ..]
    . fmap (zip [0 ..])
    . lines
  where
    v2ize :: Int -> [(Int, Char)] -> [(Point, Char)]
    v2ize y = fmap (\(x, c) -> (V2 x y, c))

-- | Helpful for debug
drawGridMap :: Point -> M.Map Point Char -> String
drawGridMap (V2 maxX maxY) pts = unlines $ drawRow <$> [0 .. maxY - 1]
  where
    drawRow y = drawCol <$> [0 .. maxX - 1]
      where
        drawCol x = fromMaybe '.' (M.lookup (V2 x y) pts)

-- | From a grid of characters, extract a `Set` of V2 where some condition is met.
parseGridToSet :: (Char -> Bool) -> String -> S.Set Point
parseGridToSet f = S.fromList . M.keys . parseGridToMap f'
  where
    f' c = if f c then Just () else Nothing

-- | Helpful for debug
drawGridSet :: Point -> S.Set Point -> String
drawGridSet dims pts = drawGridMap dims m
  where
    m = M.fromList . fmap (,'#') . S.toList $ pts
