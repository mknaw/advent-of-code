module Puzzles.Y22.Day19
  ( day19aSolve,
    day19bSolve,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Lib.Counter
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day19aSolve :: PuzzleSolve [Blueprint] Int
day19aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day19a
    }

day19bSolve :: PuzzleSolve [Blueprint] Int
day19bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day19b
    }

data Resource = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

type Cost = M.Map Resource Int

type Blueprint = M.Map Resource Cost

type Robots = Counter Resource

allResources :: [Resource]
allResources = [Ore, Clay, Obsidian, Geode]

parse' :: T.Text -> [Blueprint]
parse' = parseInput $ parseBlueprint `sepEndBy` newline
  where
    parseBlueprint :: Parser Blueprint
    parseBlueprint = do
      someTill anySingle (string "Each ore robot costs ")
      oreBot <- parseInt
      someTill anySingle (string "Each clay robot costs ")
      clayBot <- parseInt
      someTill anySingle (string "Each obsidian robot costs ")
      obsBot <- parseInt -- Ore
      string " ore and "
      obsBot' <- parseInt -- Clay
      someTill anySingle (string "Each geode robot costs ")
      geoBot <- parseInt -- Ore
      string " ore and "
      geoBot' <- parseInt -- Obsidian
      someTill anySingle (char '.')
      return $
        M.fromList
          [ (Ore, M.fromList [(Ore, oreBot)]),
            (Clay, M.fromList [(Ore, clayBot)]),
            (Obsidian, M.fromList [(Ore, obsBot), (Clay, obsBot')]),
            (Geode, M.fromList [(Ore, geoBot), (Obsidian, geoBot')])
          ]

data GameState = GameState
  { _robots :: Robots,
    _stock :: Stock,
    _blueprint :: Blueprint,
    _timeLeft :: Int
  }
  deriving (Show)

mkGameState :: Blueprint -> Int -> GameState
mkGameState = GameState (increment Ore M.empty) M.empty

type Stock = Counter Resource

maxCost :: Blueprint -> Resource -> Int
maxCost bp rsc = maximum $ map (!~ rsc) $ M.elems bp

canEmployProduction :: GameState -> Resource -> Bool
canEmployProduction _ Geode = True
canEmployProduction gs rsc = _robots gs !~ rsc < maxCost (_blueprint gs) rsc

advance :: GameState -> Int -> GameState
advance (GameState robots stock blueprint timeLeft) t =
  -- TODO just `max` it here?
  GameState robots stock' blueprint (timeLeft - t)
  where
    stock' = M.unionWith (+) stock (M.map (* t) robots)

buy :: GameState -> Resource -> GameState
buy gs robot = gs {_robots = robots, _stock = stock}
  where
    robots = increment robot $ _robots gs
    cost = _blueprint gs M.! robot
    stock =
      -- TODO maybe M.unionWith works? can try once rest of this works
      decrementBy (cost !~ Ore) Ore $
        decrementBy (cost !~ Clay) Clay $
          decrementBy (cost !~ Obsidian) Obsidian $
            _stock gs

score :: GameState -> Int
score = (!~ Geode) . _stock

timeTill :: GameState -> Resource -> Maybe Int
timeTill gs rsc
  | elem 0 $ map (robots !~) (M.keys cost) = Nothing
  | otherwise =
    Just
      . maximum
      . M.elems
      $ M.mapWithKey (\r c -> max (c - stock !~ r) 0 `roundUpDiv` (robots !~ r)) cost
  where
    stock = _stock gs
    robots = _robots gs
    cost = _blueprint gs M.! rsc

search :: (Int, [GameState]) -> (Int, [GameState])
search (best, []) = (best, [])
search (best, curr : rest)
  | timeLeft == 0 || best' >= maxScore = search (best', rest)
  | otherwise = search (best', go curr ++ rest)
  where
    go :: GameState -> [GameState]
    go gs = nexts
      where
        nexts = do
          rsc <- filter (canEmployProduction gs) allResources
          case timeTill gs rsc of
            Nothing -> []
            Just t ->
              if timeLeft - t > 0
                then return $ buy (advance gs (t + 1)) rsc
                else []

    robots = _robots curr
    timeLeft = _timeLeft curr
    idleScore = score curr + timeLeft * (robots !~ Geode)
    best' = max best idleScore
    maxScore = idleScore + (timeLeft * (timeLeft - 1)) `div` 2

play :: Int -> Blueprint -> Int
play time blueprint = fst . search $ (0, [mkGameState blueprint time])

day19a :: [Blueprint] -> Int
day19a = sum . zipWith (*) [1 ..] . map (play 24)

day19b :: [Blueprint] -> Int
day19b = product . map (play 32) . take 3
