module Puzzles.Y22.Day17
  ( day17aSolve,
    day17bSolve,
  )
where

import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((%~))
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Utils
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles

day17aSolve :: PuzzleSolve [Direction] Int
day17aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day17 2022
    }

day17bSolve :: PuzzleSolve [Direction] Int
day17bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day17 1000000000000
    }

day17 :: Int -> [Direction] -> Int
day17 k = propagate . mkState k

parse' :: T.Text -> [Direction]
parse' = fmap readDir . trim . T.unpack
  where
    readDir :: Char -> Direction
    readDir '<' = W
    readDir '>' = E
    readDir _ = error "Invalid input"

type Rock = [Point]

type Chamber = S.Set Point

rocks :: [Rock]
rocks =
  [ [V2 0 0, V2 1 0, V2 2 0, V2 3 0],
    [V2 1 0, V2 0 1, V2 1 1, V2 2 1, V2 1 2],
    [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2],
    [V2 0 0, V2 0 1, V2 0 2, V2 0 3],
    [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
  ]

inBounds :: Point -> Bool
inBounds p
  | p ^. _x < 0 = False
  | p ^. _x > 6 = False
  | p ^. _y < 0 = False
  | otherwise = True

move :: Chamber -> Rock -> Direction -> Rock
move chamber rock dir =
  if all isValidPosition rock' then rock' else rock
  where
    offset = case dir of
      N -> error "rock doesn't travel up"
      S -> V2 0 (-1)
      E -> V2 1 0
      W -> V2 (-1) 0

    rock' = map (+ offset) rock

    isValidPosition :: V2 Int -> Bool
    isValidPosition p
      | not . inBounds $ p = False
      | otherwise = not . S.member p $ chamber

heightOf :: Chamber -> Int
heightOf c = if S.null c then 0 else (+ 1) . maximum . S.map (^. _y) $ c

raiseTo :: Int -> Chamber -> Chamber
raiseTo y chamber = S.map (_y %~ (+ (y - heightOf chamber))) chamber

data Snapshot = Snapshot Int Int (S.Set Point) deriving (Eq, Ord)

data State = State
  { _chamber :: Chamber,
    _round :: Int,
    _jetIdx :: Int,
    -- round, height
    _snapshots :: M.Map Snapshot (Int, Int),
    _target :: Int,
    _jets :: [Direction]
  }

mkState :: Int -> [Direction] -> State
mkState = State mempty 0 0 mempty

-- Returns new `Chamber` and how many jets were consumed.
dropRock :: Chamber -> Rock -> [Direction] -> (Chamber, Int)
dropRock = dropRock' 1
  where
    dropRock' :: Int -> Chamber -> Rock -> [Direction] -> (Chamber, Int)
    dropRock' _ _ _ [] = error "malformed input"
    dropRock' i chamber rock (d : ds) =
      if rock'' == rock'
        then (foldl (flip S.insert) chamber rock', i)
        else dropRock' (i + 1) chamber rock'' ds
      where
        rock' = move chamber rock d
        rock'' = move chamber rock' S

propagate :: State -> Int
propagate state@State {..}
  | _round + 1 == _target = h
  | otherwise = propagate state'
  where
    h = heightOf chamber'

    rockIdx = _round `mod` length rocks
    rock = rocks !! rockIdx

    initRock :: Rock -> Rock
    initRock = map (+ V2 2 (heightOf _chamber + 3))

    (chamber', jetIdx) = dropRock _chamber (initRock rock) (drop _jetIdx . cycle $ _jets)
    jetIdx' = (_jetIdx + jetIdx) `mod` length _jets

    !fill = raiseTo 0 . bfs $ _chamber
    snapshot = Snapshot rockIdx jetIdx' fill

    defaultState =
      state
        { _chamber = chamber',
          _round = _round + 1,
          _jetIdx = jetIdx',
          _snapshots = M.insert snapshot (_round, h) _snapshots
        }

    state' = M.fromMaybe defaultState $ do
      (oldRockIdx, h') <- M.lookup snapshot _snapshots
      let hd = h - h'  -- Height diff
      let rd = _round - oldRockIdx  -- Round diff
      let k = (_target - _round) `div` rd  -- Cycles
      let hn = h + k * hd  -- Height after cycles
      let rn = _round + k * rd  -- Round after cycles
      return $
        state
          { _chamber = raiseTo hn chamber',
            _round = rn + 1,
            _jetIdx = jetIdx'
          }

bfs :: Chamber -> S.Set Point
bfs chamber = go (S.singleton (V2 0 h)) S.empty
  where
    h = heightOf chamber

    go :: S.Set Point -> S.Set Point -> S.Set Point
    go frontier visited
      | S.null frontier = visited
      | otherwise = go frontier' visited'
      where
        frontier' = S.filter (not . (`S.member` visited)) . S.unions . S.map neighbors $ frontier
        visited' = S.union visited frontier

    neighbors :: Point -> S.Set Point
    neighbors =
      S.fromList
        . filter ((<= h) . (^. _y))
        . filter inBounds
        . filter (not . (`S.member` chamber))
        . neighbors4
