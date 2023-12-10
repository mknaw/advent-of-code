module Puzzles.Y23.Day10
  ( day10aSolve,
    day10bSolve,
  )
where

import Control.Lens
import Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Utils
import Lib.Utils.Geometry
import Lib.Utils.Search
import Linear.V2
import Puzzles.Puzzles

type Pipe = (Direction, Direction)

data PipeMap = PipeMap {
  _dims :: Point,
  _pipes :: M.Map Point Pipe
}

day10aSolve :: PuzzleSolve (Point, PipeMap) Int
day10aSolve =
  PuzzleSolve
    { _parse = parse' . T.unpack,
      _solve = uncurry day10a
    }

day10bSolve :: PuzzleSolve (Point, PipeMap) Int
day10bSolve =
  PuzzleSolve
    { _parse = parse' . T.unpack,
      _solve = uncurry day10b
    }

parse' :: String -> (Point, PipeMap)
parse' input = ( animal, PipeMap dims (M.insert animal (determineStartPipe pipes animal) pipes))
  where
    dims = V2 (length . head $ lines input) (length . lines $ input)
    grid = parseGridToMap parseChar input

    animal = case M.keys . M.filter isRight $ grid of
      [a] -> a
      _ -> error "malformed input data"

    pipes = M.fromList $ do
      (pt, e) <- M.toList grid
      case e of
        Left pipe -> [(pt, pipe)]
        Right () -> []

    parseChar :: Char -> Maybe (Either Pipe ())
    parseChar '-' = Just (Left (W, E))
    parseChar '|' = Just (Left (N, S))
    parseChar 'L' = Just (Left (N, E))
    parseChar 'F' = Just (Left (S, E))
    parseChar 'J' = Just (Left (N, W))
    parseChar '7' = Just (Left (S, W))
    parseChar 'S' = Just (Right ())
    parseChar _ = Nothing

pipeOuts :: Point -> Pipe -> [Point]
pipeOuts pt = fmap (pt ==>) . unpairify

pipeFits :: (Point, Pipe) -> (Point, Pipe) -> Bool
pipeFits a b = fst a `elem` outs' && fst b `elem` outs
  where
    outs = uncurry pipeOuts a
    outs' = uncurry pipeOuts b

determineStartPipe :: M.Map Point Pipe -> Point -> Pipe
determineStartPipe pipes pt = pairify $ do
  dir <- [N, E, S, W]
  let pt' = pt ==> dir
  -- TODO maybe a good use case for `MaybeT`?
  case M.lookup pt' pipes of
    Just pipe' -> [dir | oppositeDir dir `elem` unpairify pipe']
    Nothing -> []

-- TODO probably could just use dfs and divide the end distance by 2
collectDistances :: M.Map Point Pipe -> Point -> M.Map Point Int
collectDistances pipes start = bfs next (M.fromList [(start, 0)])
  where
    next :: Point -> Int -> M.Map Point Int
    next pt dist = M.fromList $ do
      let pipe = pipes M.! pt
      pt' <- pipeOuts pt pipe
      -- TODO maybe a good use case for `MaybeT`?
      case M.lookup pt' pipes of
        Just pipe' -> [(pt', dist + 1) | pipeFits (pt, pipe) (pt', pipe')]
        Nothing -> []

day10a :: Point -> PipeMap -> Int
day10a animal pipeMap = maximum . M.elems $ collectDistances (_pipes pipeMap) animal

collectPath :: M.Map Point Pipe -> Point -> M.Map Point Direction
-- TODO have to somehow set the initial direction
collectPath pipes start = dfs next start (snd . head . next start $ N)
  where
    next :: Point -> Direction -> [(Point, Direction)]
    next pt _ = do
      let pipe = pipes M.! pt
      pt' <- pipeOuts pt pipe
      -- TODO maybe a good use case for `MaybeT`?
      case M.lookup pt' pipes of
        Just pipe' -> [(pt', v2toDirection (pt' - pt)) | pipeFits (pt, pipe) (pt', pipe')]
        Nothing -> []

fill :: Point -> S.Set Point -> [Point] -> S.Set Point
fill (V2 maxX maxY) pipeLoop = fill' S.empty
  where
    fill' :: S.Set Point -> [Point] -> S.Set Point
    fill' seen [] = seen
    fill' seen (pt : pts) = fill' seen' pts
      where
        seen'
          | pt `S.member` seen = seen
          | pt `S.member` pipeLoop = seen
          | otherwise = S.union seen (fillOne pt)

    fillOne :: Point -> S.Set Point
    fillOne start = S.fromList . M.keys $ bfs next (M.fromList [(start, ())])
      where
        next :: Point -> () -> M.Map Point ()
        next pt _ = M.fromList $ do
          pt' <- neighbors4 pt
          [(pt', ()) |
            not (S.member pt' pipeLoop)
            && pt' ^. _x >= 0
            && pt' ^. _y >= 0
            && pt' ^. _x < maxX
            && pt' ^. _y < maxY]

-- TODO Feels like there's a more elegant way to do this ...
touchesEdge :: Point -> Point -> Bool
touchesEdge (V2 maxX maxY) pt
  | pt ^. _x == 0 = True
  | pt ^. _y == 0 = True
  | pt ^. _x == maxX - 1 = True
  | pt ^. _y == maxY - 1 = True
  | otherwise = False

dirToChar :: Direction -> Char
dirToChar N = '^'
dirToChar E = '>'
dirToChar S = 'v'
dirToChar W = '<'

day10b :: Point -> PipeMap -> Int
-- day10b animal (PipeMap dims pipes) = if any (touchesEdge dims) cws then S.size ccws else S.size cws
day10b animal (PipeMap dims pipes) = error . show . drawGridMap dims $ debugMap
  where
    path = collectPath pipes animal
    occupied = S.fromList . M.keys $ path
    cws = fill dims occupied . fmap (uncurry (==>)) . M.toList . fmap turnCW $ path
    ccws = fill dims occupied . fmap (uncurry (==>)) . M.toList . fmap turnCCW $ path
    
    debugMap = M.union (dirToChar <$> path) (M.fromList $ (\p -> (p, '#')) <$> S.toList ccws)

