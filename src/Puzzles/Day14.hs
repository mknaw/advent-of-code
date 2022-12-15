module Puzzles.Day14
  ( day14aSolve,
    day14bSolve,
  )
where

import Control.Lens.Getter ((^.))
import qualified Data.List as L
import qualified Data.List.Safe as Safe
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Distribution.Utils.MapAccum
import Lib.Parse
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day14aSolve :: PuzzleSolve [Wall] Int
day14aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day14
    }

day14bSolve :: PuzzleSolve [Wall] Int
day14bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day14 . addFloor
    }

parse' :: T.Text -> [Wall]
parse' = parseInput $ parseWall `sepEndBy` newline
  where
    parsePoint :: Parser Point
    parsePoint = do
      x <- parseInt
      char ','
      V2 x <$> parseInt

    parseWall :: Parser Wall
    parseWall = parsePoint `sepBy` string " -> "

type Wall = [Point]

type Grid = S.Set Point

mkLine :: Point -> Point -> S.Set Point
mkLine (V2 x1 y1) (V2 x2 y2) =
  S.fromList [V2 x y | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

mkGrid :: [Wall] -> Grid
mkGrid = L.foldl' mkWall S.empty
  where
    mkWall :: Grid -> Wall -> Grid
    mkWall g = L.foldl' S.union g . (zipWith mkLine <*> tail)

simulate :: Grid -> Grid
simulate grid = simulate' [origin] grid
  where
    origin = V2 500 0
    !yMax = maximum . S.map (^. _y) $ grid

    simulate' :: [Point] -> Grid -> Grid
    simulate' ps g = M.fromMaybe g $ do
      (!g', !ps') <- mapAccumM move g ps
      return $ simulate' (M.catMaybes ps' ++ [origin]) g'

    move :: Grid -> Point -> Maybe (Grid, Maybe Point)
    move g p
      | (^. _y) p >= yMax = Nothing
      | S.member p g = if p == origin then Nothing else Just (g, Nothing)
      | otherwise = Just (g', next)
      where
        next = Safe.head . filter (`S.notMember` g) . fmap (p +) $ [V2 dx 1 | dx <- [0, -1, 1]]
        !g' = if M.isJust next then g else S.insert p g

addFloor :: [Wall] -> [Wall]
addFloor walls = [V2 x (maxY + 2) | x <- [-1, 1001]] : walls
  where
    maxY = maximum . fmap (^. _y) . concat $ walls

day14 :: [Wall] -> Int
day14 walls = S.size grid' - S.size grid
  where
    grid = mkGrid walls
    grid' = simulate grid
