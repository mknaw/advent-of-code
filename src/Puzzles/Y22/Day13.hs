module Puzzles.Y22.Day13
  ( day13aSolve,
    day13bSolve,
  )
where

import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day13aSolve :: PuzzleSolve [Node] Int
day13aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day13a
    }

day13bSolve :: PuzzleSolve [Node] Int
day13bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day13b
    }

parse' :: T.Text -> [Node]
parse' = parseInput $ parseNode `sepEndBy` some newline
  where
    parseNode :: Parser Node
    parseNode = I <$> parseInt <|> L <$> parseList
      where
        parseList :: Parser [Node]
        parseList = do
          char '['
          nodes <- parseNode `sepBy` char ','
          char ']'
          return nodes

data Node = I Int | L [Node] deriving (Show, Eq)

instance Ord Node where
  compare (I a) (I b) = compare a b
  compare (I a) (L bs) = compare (L [I a]) (L bs)
  compare (L as) (I b) = compare (L as) (L [I b])
  compare (L []) (L _) = LT
  compare (L _) (L []) = GT
  compare (L (a : as)) (L (b : bs))
    | a == b = compare (L as) (L bs)
    | otherwise = compare a b

day13a :: [Node] -> Int
day13a = sum . map (1 +) . indicesWhere (uncurry (<=)) . map pairify . LS.chunksOf 2

day13b :: [Node] -> Int
day13b = product . map (1 +) . indicesWhere (`elem` dividers) . L.sort . (++) dividers
  where
    dividers = [L [L [I x]] | x <- [2, 6]]
