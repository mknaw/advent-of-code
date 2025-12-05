module Lib.Matrix
  ( Coord,
    coords,
    cols,
    countTrue,
    mapRowWise,
    mapColWise,
    neighbors,
    neighbors8,
    rows,
  )
where

import Data.Ix (inRange)
import qualified Data.List as L
import qualified Data.Matrix as M
import qualified Data.Vector as V
-- TODO do we _really_ need a `countTrue` in this module?
import Lib.Utils hiding (countTrue)

type Coord = (Int, Int)

coords :: M.Matrix a -> [Coord]
coords m = [(i, j) | i <- [1 .. M.nrows m], j <- [1 .. M.ncols m]]

rows :: M.Matrix a -> [V.Vector a]
rows m = (`M.getRow` m) <$> [1 .. M.nrows m]

mapRowWise :: (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
mapRowWise f = L.foldl1' (M.<->) . map (M.rowVector . f) . rows

cols :: M.Matrix a -> [V.Vector a]
cols m = (`M.getCol` m) <$> [1 .. M.ncols m]

mapColWise :: (V.Vector a -> V.Vector b) -> M.Matrix a -> M.Matrix b
mapColWise f = L.foldl1' (M.<|>) . map (M.colVector . f) . cols

countTrue :: M.Matrix Bool -> Int
countTrue = V.sum . V.map boolToInt . M.getMatrixAsVector

neighbors :: M.Matrix a -> Coord -> [Coord]
neighbors m (i, j) =
  filter
    (inRange ((1, 1), (M.nrows m, M.ncols m)))
    [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

neighbors8 :: M.Matrix a -> Coord -> [Coord]
neighbors8 m (i, j) =
  neighbors m (i, j)
    ++ ( filter
           (inRange ((1, 1), (M.nrows m, M.ncols m)))
           [(i - 1, j - 1), (i - 1, j + 1), (i + 1, j - 1), (i + 1, j + 1)]
       )
