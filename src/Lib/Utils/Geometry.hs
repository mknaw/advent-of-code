module Lib.Utils.Geometry
  ( (==>),
    Direction (..),
    directionToV2,
    neighbors4,
    neighbors8,
    parseGridToMap,
    parseGridToSet,
    Point,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Linear.V2

type Point = V2 Int

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

directionToV2 :: Direction -> V2 Int
directionToV2 N = V2 0 (-1)
directionToV2 E = V2 1 0
directionToV2 S = V2 0 1
directionToV2 W = V2 (-1) 0

(==>) :: V2 Int -> Direction -> V2 Int
v2 ==> dir = v2 + directionToV2 dir

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

-- | Parse a grid of characters into a `Map` of V2 to arbitrary data.
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

-- | From a grid of characters, extract a `Set` of V2 where some condition is met.
parseGridToSet :: (Char -> Bool) -> String -> S.Set Point
parseGridToSet f = S.fromList . M.keys . parseGridToMap f'
  where
    f' c = if f c then Just () else Nothing
