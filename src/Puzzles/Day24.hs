module Puzzles.Day24
  ( day24aSolve,
    day24bSolve,
  )
where

import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles

data Blizzard = Blizz
  { _pos :: Point,
    _dir :: Direction
  }
  deriving (Eq, Ord, Show)

day24aSolve :: PuzzleSolve (V2 Int, S.Set Blizzard) Int
day24aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day24a
    }

day24bSolve :: PuzzleSolve (V2 Int, S.Set Blizzard) Int
day24bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day24b
    }

parse' :: T.Text -> (V2 Int, S.Set Blizzard)
parse' input = (dims, S.fromList . M.catMaybes . concat $ blizzards)
  where
    blizzards =
      zipWith f [0 ..]
        . ( fmap (zip [0 ..] . fmap parseDir . init . drop 1)
              . init
              . drop 1
              . lines
              . T.unpack
          )
        $ input

    f :: Int -> [(Int, Maybe Direction)] -> [Maybe Blizzard]
    f j = fmap (\(i, dir) -> Blizz (V2 i j) <$> dir)

    parseDir :: Char -> Maybe Direction
    parseDir '^' = Just N
    parseDir 'v' = Just S
    parseDir '>' = Just E
    parseDir '<' = Just W
    parseDir _ = Nothing

    dims = V2 (length . head $ blizzards) (length blizzards)

advance :: (V2 Int, S.Set Blizzard) -> (V2 Int, S.Set Blizzard)
advance (dims, blizzards) = (dims, S.map blow blizzards)
  where
    blow :: Blizzard -> Blizzard
    blow (Blizz pos dir) = Blizz pos' dir
      where
        pos' = mod <$> pos + directionToV2 dir <*> dims

topLeft :: V2 Int
topLeft = V2 0 (-1)

topRight :: V2 Int -> V2 Int
topRight (V2 m n) = V2 (m - 1) n

unoccupiedPts :: (V2 Int, S.Set Blizzard) -> S.Set Point
unoccupiedPts (dims@(V2 m n), blizzards) = S.difference allPts $ S.map _pos blizzards
  where
    allPts = S.fromList $ [topLeft, topRight dims] ++ [V2 x y | x <- [0 .. m - 1], y <- [0 .. n - 1]]

data BFSState = BFSState Point Int deriving (Eq, Ord, Show)

bfs :: V2 Int -> Int -> V2 Int -> [S.Set Point] -> S.Set Point -> Maybe Int
bfs tgt t dims@(V2 m n) unoccupieds frontier
  | S.null frontier = Nothing
  | tgt `S.member` frontier = Just t
  | otherwise = bfs tgt (t + 1) (V2 m n) unoccupieds frontier'
  where
    inBounds :: Point -> Bool
    inBounds pt@(V2 x y)
      | pt == topLeft = True
      | pt == topRight dims = True
      | otherwise = x >= 0 && x < m && y >= 0 && y < n

    unoccupied = cycle unoccupieds !! (t + 1)

    getNexts :: Point -> S.Set Point
    getNexts pt = S.intersection unoccupied . S.fromList . filter inBounds . (pt :) . neighbors4 $ pt

    frontier' = S.foldr S.union S.empty . S.map getNexts $ frontier

unoccupiedOverTime :: (V2 Int, S.Set Blizzard) -> [S.Set Point]
unoccupiedOverTime (dims@(V2 m n), blizzards) =
  fmap unoccupiedPts . take (lcm m n) . iterate advance $ (dims, blizzards)

day24a :: (V2 Int, S.Set Blizzard) -> Int
day24a (dims@(V2 m n), blizzards) =
  M.fromJust $ bfs (V2 (m - 1) n) 0 dims unoccupieds (S.singleton (V2 0 (-1)))
  where
    !unoccupieds = unoccupiedOverTime (dims, blizzards)

day24b :: (V2 Int, S.Set Blizzard) -> Int
day24b (dims@(V2 m n), blizzards) = thirdLeg
  where
    !unoccupieds = unoccupiedOverTime (dims, blizzards)
    bottomRight = V2 (m - 1) n
    firstLeg = M.fromJust $ bfs bottomRight 0 dims unoccupieds (S.singleton topLeft)
    secondLeg = M.fromJust $ bfs topLeft firstLeg dims unoccupieds (S.singleton bottomRight)
    thirdLeg = M.fromJust $ bfs bottomRight secondLeg dims unoccupieds (S.singleton topLeft)
