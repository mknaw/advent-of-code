module Puzzles.Day23
  ( day23aSolve,
    day23bSolve,
  )
where

import Control.Lens.Getter ((^.))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Counter
import Lib.Utils
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles

day23aSolve :: PuzzleSolve (S.Set Point) Int
day23aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day23a
    }

day23bSolve :: PuzzleSolve (S.Set Point) Int
day23bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day23b
    }

parse' :: T.Text -> S.Set Point
parse' = parseGridToSet (== '#') . T.unpack

-- TODO I feel like there is some method to skip past arbitrary steps to do the simulation
-- more efficiently... but also the naive way isn't so slow where it's unfeasible? Weird.

type Proposal = (Point, Point)

neighborsRay :: Point -> Direction -> [Point]
neighborsRay pt dir = (pt +) . v2 <$> [-1, 0, 1]
  where
    v2 = case dir of
      N -> (`V2` (-1))
      E -> V2 1
      S -> (`V2` 1)
      W -> V2 (-1)

propose :: [Direction] -> S.Set Point -> Point -> Point
propose dirs pts pt
  | not . any (`S.member` pts) $ neighbors = pt
  | otherwise = M.fromMaybe pt . M.listToMaybe . M.catMaybes $ proposals
  where
    neighbors = neighbors8 pt
    proposals = proposeDirection pts pt <$> dirs

proposeDirection :: S.Set Point -> Point -> Direction -> Maybe Point
proposeDirection pts pt dir =
  if not . any (`S.member` pts) $ neighborsRay pt dir
    then Just $ pt ==> dir
    else Nothing

avoidCollisions :: S.Set Proposal -> S.Set Proposal
avoidCollisions proposals = S.map avoid proposals
  where
    collisions = M.keys . M.filter (> 1) . S.foldr (increment . snd) mkCounter $ proposals
    avoid (from, to) = if to `elem` collisions then (from, from) else (from, to)

data State = State
  { _dirs :: [Direction],
    _pts :: S.Set Point
  }

advance :: State -> State
advance (State dirs pts) = State dirs' pts'
  where
    dirs' = shift dirs
    pts' = S.map snd . avoidCollisions . S.map ((,) <$> id <*> propose dirs pts) $ pts

score :: S.Set Point -> Int
score pts = (width * height) - S.size pts
  where
    width = S.findMax (S.map (^. _x) pts) - S.findMin (S.map (^. _x) pts) + 1
    height = S.findMax (S.map (^. _y) pts) - S.findMin (S.map (^. _y) pts) + 1

day23a :: S.Set Point -> Int
day23a = score . _pts . (!! 10) . iterate advance . State [N, S, W, E]

day23b :: S.Set Point -> Int
day23b pts = (1 +) . M.fromJust . L.findIndex id . zipWith (==) (_pts <$> tail states) $ (_pts <$> states)
  where
    states = iterate advance . State [N, S, W, E] $ pts
