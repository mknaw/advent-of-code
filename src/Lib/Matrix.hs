module Lib.Matrix
  ( coords,
    cols,
    countTrue,
    rows,
    mapRowWise,
    mapColWise,
  )
where

import qualified Data.List as L
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Lib.Utils

coords :: M.Matrix a -> [(Int, Int)]
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
