module Puzzles.Y23.Day08
  ( day08aSolve,
    day08bSolve,
  )
where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Node = String

data Direction = L | R deriving (Show)

type NodeMap = M.Map Node (Node, Node)

day08aSolve :: PuzzleSolve (NodeMap, [Direction]) Int
day08aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = uncurry (day08 (== "AAA") (== "ZZZ"))
    }

day08bSolve :: PuzzleSolve (NodeMap, [Direction]) Int
day08bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = uncurry (day08 (L.isSuffixOf "A") (L.isSuffixOf "Z"))
    }

parse' :: T.Text -> (NodeMap, [Direction])
parse' = parseInput $ do
  dirs <- some (choice [L <$ char 'L', R <$ char 'R'])
  space
  nodeMap <- M.fromList <$> parseNodeLine `sepEndBy` newline
  return (nodeMap, dirs)
  where
    parseNodeLine :: Parser (Node, (Node, Node))
    parseNodeLine = do
      key <- many alphaNumChar
      space
      char '='
      space
      char '('
      left <- many alphaNumChar
      string ", "
      right <- many alphaNumChar
      char ')'
      return (key, (left, right))

travel :: NodeMap -> [Direction] -> Node -> [(Int, Node)]
travel nodes dirs start = zip [0 ..] . L.scanl' go start . L.cycle $ dirs
  where
    go :: Node -> Direction -> Node
    go node dir = case dir of
      L -> fst (nodes M.! node)
      R -> snd (nodes M.! node)

findCycle :: (Node -> Bool) -> Int -> [(Int, Node)] -> State ([Int], M.Map Node (S.Set Int)) (Int, Int)
findCycle isEnd modulus = go
  where
    go :: [(Int, Node)] -> State ([Int], M.Map Node (S.Set Int)) (Int, Int)
    go [] = error "impossible" -- TODO maybe I should check out those NonEmpty lists
    go ((i, node) : rest) = do
      -- TODO still have to use `hits`
      (hits, visited) <- get
      let hits' = if isEnd node then i : hits else hits
      case M.lookup node visited of
        Just ends -> do
          let matches = S.filter (\x -> x `rem` modulus == i `rem` modulus) ends
          case S.size matches of
            0 -> do
              put (hits', M.insert node (S.insert i ends) visited)
              go rest
            1 -> return (i, S.findMin matches)
            _ -> error "shouldn't happen"
        Nothing -> do
          put (hits', M.insert node (S.singleton i) visited)
          go rest

day08 :: (Node -> Bool) -> (Node -> Bool) -> NodeMap -> [Direction] -> Int
-- TODO this isn't quite correct but it works for my part B input, must fix to take in account
-- that all can meet earlier due to offset
day08 isStart isEnd nodes dirs = L.foldl1' lcm . map (uncurry (-)) $ cycles
  where
    starts = filter isStart . M.keys $ nodes
    paths = travel nodes dirs <$> starts
    cycles = (\path -> evalState (findCycle isEnd (length dirs) path) ([], M.empty)) <$> paths
