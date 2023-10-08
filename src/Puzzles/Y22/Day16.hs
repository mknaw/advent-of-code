module Puzzles.Y22.Day16
  ( day16aSolve,
    day16bSolve,
  )
where

import Control.Monad.State
import Data.Char (isUpper)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

data Valve = Valve
  { _name :: String,
    _flow :: Int,
    _tunnels :: [String]
  }
  deriving (Show)

instance Eq Valve where
  (==) a b = _name a == _name b

instance Ord Valve where
  compare a b = compare (_name a) (_name b)

day16aSolve :: PuzzleSolve [Valve] Int
day16aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day16 1 30
    }

day16bSolve :: PuzzleSolve [Valve] Int
day16bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day16 2 26
    }

parse' :: T.Text -> [Valve]
parse' = parseInput $ parseValve `sepEndBy` newline
  where
    parseValve :: Parser Valve
    parseValve = do
      string "Valve "
      name <- some upperChar
      someTill anySingle (char '=')
      flow <- parseInt
      takeWhileP (Just "") (not . isUpper)
      tunnels <- some upperChar `sepBy` string ", "
      return $ Valve name flow tunnels

type DistMap = M.Map (String, String) Int

sortTup :: Ord a => (a, a) -> (a, a)
sortTup (a, b) = (min a b, max a b)

dist :: DistMap -> Valve -> Valve -> Int
dist dm a b = (dm M.!) . sortTup . both _name $ (a, b)

-- Precompute node distances because only some traversals will be meaningful;
-- that is, we will never target a traversal to a flow 0 valve.
floydWarshall :: [Valve] -> State DistMap ()
floydWarshall valves = do
  sequence_ [initialize v1 v2 | v1 <- valves, v2 <- valves]
  sequence_ [go v1 v2 v3 | v1 <- valves, v2 <- valves, v3 <- valves]
  where
    initialize :: Valve -> Valve -> State DistMap ()
    initialize v1 v2 = do
      let d | v1 == v2 = 0
            | _name v1 `elem` _tunnels v2 = 1
            | otherwise = 99
      modify' $ M.insert (_name v1, _name v2) d

    go :: Valve -> Valve -> Valve -> State DistMap ()
    go v1 v2 v3 = do
      dists <- get
      let key = sortTup . both _name $ (v1, v3)
      modify' $ M.insertWith min key (dist dists v1 v2 + dist dists v2 v3)

dfs ::
  S.Set Valve ->
  DistMap ->
  Int -> -- current score
  Int -> -- time left
  Valve -> -- current valve
  S.Set Valve -> -- visited valves
  -- Save map of visited valves to best score
  -- Needed for part B to find the max sum of pairs of disjoint visited sets
  State (M.Map (S.Set Valve) Int) Int
dfs allCandidates dm s t v visited = do
  let rest = S.difference allCandidates visited
  modify' $ M.insertWith max visited s
  if S.null rest || t <= 1
    then return s
    else do
      maximum
        <$> mapM
          ( \v' ->
              let t' = t - dist dm v v' - 1
                  s' = s + t' * _flow v'
               in dfs allCandidates dm s' t' v' (S.insert v' visited)
          )
          (S.toList rest)

day16 :: Int -> Int -> [Valve] -> Int
day16 n t valves =
  maximum
    . map (sum . fmap snd)
    . filter (allDisjoint . fmap fst)
    $ subsets n (M.toList scores)
  where
    start = head $ filter (\v -> _name v == "AA") valves
    candidates = S.fromList . filter (\v -> _flow v > 0) $ valves
    dm = execState (floydWarshall valves) M.empty
    scores = execState (dfs candidates dm 0 t start S.empty) M.empty
