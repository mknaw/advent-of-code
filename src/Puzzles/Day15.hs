module Puzzles.Day15
  ( day15aSolve,
    day15bSolve,
  )
where

import Control.Applicative
import Control.Lens.Getter ((^.), Getting)
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day15aSolve :: PuzzleSolve (Int, [Sensor]) Int
day15aSolve =
  PuzzleSolve
    { _parse = parse' 2000000,
      _solve = day15a
    }

day15bSolve :: PuzzleSolve (Int, [Sensor]) Int
day15bSolve =
  PuzzleSolve
    { _parse = parse' 4000000,
      _solve = day15b
    }

parse' :: Int -> T.Text -> (Int, [Sensor])
parse' default' = parseInput $ (,) <$> parseTarget <*> (parseSensor `sepEndBy` newline)
  where
    parseTarget :: Parser Int
    parseTarget = do
      y <- optional . try $ do parseInt
      return $ M.fromMaybe default' y

    parseSensor :: Parser Sensor
    parseSensor = uncurry Sensor . pairify <$> count 2 parsePoint
      where
        parsePoint :: Parser Point
        parsePoint = do
          someTill anySingle (string "x=")
          x <- parseInt
          string ", y="
          V2 x <$> parseInt

data Sensor = Sensor
  { _pos :: Point,
    _beacon :: Point
  }

-- | Manhattan distance
(<->) :: Point -> Point -> Int
V2 x1 y1 <-> V2 x2 y2 = abs (x1 - x2) + abs (y1 - y2)

reach :: Sensor -> Int
reach sensor = _pos sensor <-> _beacon sensor

day15a :: (Int, [Sensor]) -> Int
day15a (t, sensors) =
  S.size
    . flip S.difference beaconsOnTarget
    . L.foldl1' S.union
    . map crossingY
    $ sensors
  where
    crossingY :: Sensor -> S.Set Int
    crossingY sensor = S.fromList [x - r .. x + r]
      where
        V2 x y = _pos sensor
        r = reach sensor - abs (t - y)

    beaconsOnTarget :: S.Set Int
    beaconsOnTarget = S.fromList . map (^. _x) . filter ((==) t . (^. _y)) . map _beacon $ sensors

-- Central idea: project squared onto diagonals, then using that can find a gap in between the
-- resulting squares (which are now in-line with the axes), reposition the point in that gap.
day15b :: (Int, [Sensor]) -> Int
day15b (bound, sensors) =
  tune
    . S.findMin
    . S.filter (inArea (V2 0 0, V2 bound bound))
    . S.map reposition
    . S.filter (\p -> not $ any (`inArea` p) projections)
    $ S.map (uncurry V2) $ S.cartesianProduct (gaps _x projections) (gaps _y projections)
  where
    projectedArea :: Sensor -> (Point, Point)
    projectedArea sensor = (proj - r, proj + r)
      where
        V2 x y = (* 2) <$> _pos sensor
        proj = (`div` 2) <$> V2 (x + y) (y - x)
        r = return . reach $ sensor

    getCloseGaps :: S.Set Int -> S.Set Int
    getCloseGaps xs = S.intersection (S.map (+ 1) xs) (S.map (subtract 1) xs)

    gaps :: Getting Int Point Int -> [(Point, Point)] -> S.Set Int
    gaps coord = getCloseGaps . S.fromList . concatMap (fmap (^. coord) . unpairify)

    inArea :: (Point, Point) -> Point -> Bool
    inArea (V2 x0 y0, V2 x1 y1) (V2 x y) = x0 <= x && x <= x1 && y0 <= y && y <= y1

    reposition :: Point -> Point
    reposition (V2 x y) = (`div` 2) <$> V2 (x - y) (x + y)

    tune :: Point -> Int
    tune (V2 x y) = 4000000 * x + y

    projections = projectedArea <$> sensors
