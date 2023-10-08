module Puzzles.Y22.Day22
  ( day22aSolve,
    day22bSolve,
  )
where

import Control.Lens.Getter ((^.))
import Control.Monad (msum, void)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M hiding (mapMaybe)
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Linear.V2
import Puzzles.Puzzles
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

day22aSolve :: PuzzleSolve (Map, [Instruction]) Int
day22aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day22a
    }

day22bSolve :: PuzzleSolve (Map, [Instruction]) Int
day22bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day22b
    }

data Cell = Free | Wall deriving (Eq, Show)

type Map = M.Map (V2 Int) Cell

data Rotation = RotCCW | RotCW deriving (Show)

type Instruction = Either Int Rotation

parse' :: T.Text -> (Map, [Instruction])
parse' = parseInput $ do
  map_ <- parseMap
  newline
  instructions <- someTill parseInstruction (void newline <|> eof)
  return (map_, instructions)
  where
    parseMap :: Parser Map
    parseMap = fmap makeMap <$> many $ someTill parseCell newline
      where
        makeMap :: [[Maybe Cell]] -> Map
        makeMap cells = M.fromList $ do
          (y, row) <- zip [0 ..] cells
          (x, cell) <- zip [0 ..] row
          case cell of
            Just c -> return (V2 x y, c)
            Nothing -> []

    -- Overkill to use `Parser`, but whatever.
    parseCell :: Parser (Maybe Cell)
    parseCell =
      choice
        [ Nothing <$ char ' ',
          Just Free <$ char '.',
          Just Wall <$ char '#'
        ]

    parseInstruction :: Parser Instruction
    parseInstruction = choice [Right RotCCW <$ char 'L', Right RotCW <$ char 'R', Left <$> parseInt]

data State = State
  { _pos :: V2 Int,
    _dir :: Direction
  }
  deriving (Show)

getRow :: Int -> Map -> [V2 Int]
getRow y = filter ((== y) . (^. _y)) . M.keys

getCol :: Int -> Map -> [V2 Int]
getCol x = filter ((== x) . (^. _x)) . M.keys

mkState :: Map -> State
mkState m = State pos R
  where
    pos = L.minimumBy (compare `on` (^. _x)) . getRow 0 $ m

data Direction = U | D | L | R deriving (Eq, Show)

opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

directionToV2 :: Direction -> V2 Int
directionToV2 U = V2 0 (-1)
directionToV2 D = V2 0 1
directionToV2 L = V2 (-1) 0
directionToV2 R = V2 1 0

(==>) :: V2 Int -> Direction -> V2 Int
v2 ==> dir = v2 + directionToV2 dir

rotate :: Rotation -> Direction -> Direction
rotate RotCCW U = L
rotate RotCW U = R
rotate RotCCW D = R
rotate RotCW D = L
rotate RotCCW L = D
rotate RotCW L = U
rotate RotCCW R = U
rotate RotCW R = D

type WrapFn = Map -> State -> State

step :: State -> State
step s = s {_pos = _pos s ==> _dir s}

advance :: WrapFn -> Map -> State -> [Instruction] -> State
advance _ _ state [] = state
advance wrap map_ state (Left 0 : is) = advance wrap map_ state is
advance wrap map_ state (Right rot : is) = advance wrap map_ (state {_dir = rotate rot (_dir state)}) is
advance wrap map_ state (Left n : is) =
  case map_ M.! _pos state' of
    Wall -> advance wrap map_ state is
    Free -> advance wrap map_ state' (Left (n - 1) : is)
  where
    naive = step state
    wrapped = wrap map_ state
    state' = if _pos naive `elem` M.keys map_ then naive else wrapped

score :: State -> Int
score (State (V2 x y) dir) = 1000 * (y + 1) + 4 * (x + 1) + dirPoints dir
  where
    dirPoints R = 0
    dirPoints D = 1
    dirPoints L = 2
    dirPoints U = 3

day22 :: WrapFn -> (Map, [Instruction]) -> Int
day22 wrap (map_, instructions) = score $ advance wrap map_ (mkState map_) instructions

day22a :: (Map, [Instruction]) -> Int
day22a = day22 wrap
  where
    wrap :: WrapFn
    wrap m s@(State (V2 x y) dir) = s {_pos = pos'}
      where
        pos' = case dir of
          U -> L.maximumBy (compare `on` (^. _y)) . getCol x $ m
          D -> L.minimumBy (compare `on` (^. _y)) . getCol x $ m
          L -> L.maximumBy (compare `on` (^. _x)) . getRow y $ m
          R -> L.minimumBy (compare `on` (^. _x)) . getRow y $ m

-- Know one corner is `(0, 0)`, so just provide the other.
getDims :: Map -> V2 Int
getDims m = V2 (L.maximum xs) (L.maximum ys)
  where
    xs = map (^. _x) . M.keys $ m
    ys = map (^. _y) . M.keys $ m

getResolution :: Map -> Int
getResolution m = (+ 1) . minimum $ do
  -- # # # . .  Need to look along both axes for this treacherous shape.
  -- . . # # #  Probably would have been easier while it was `[[a]]`, but whatever.
  x <- [0 .. dims ^. _x]
  let col = getCol x m
  let height = L.maximum (map (^. _y) col) - L.minimum (map (^. _y) col)

  y <- [0 .. dims ^. _y]
  let row = getRow y m
  let width = L.maximum (map (^. _x) row) - L.minimum (map (^. _x) row)
  [height, width]
  where
    dims = getDims m

allCorners :: Map -> S.Set (V2 Int)
allCorners map_ = S.fromList $ do
  x <- takeWhile (< m) [i * res | i <- [0 ..]]
  y <- takeWhile (< n) [i * res | i <- [0 ..]]
  let v2 = V2 x y
  case M.lookup v2 map_ of
    -- TODO must be a nicer functional idiom.
    Just _ -> [V2 x y, V2 (x + res - 1) y, V2 x (y + res - 1), V2 (x + res - 1) (y + res - 1)]
    Nothing -> []
  where
    V2 m n = getDims map_
    res = getResolution map_

-- "Pivot" corner meaning 270 degree rotation.
-- These are special because we can fold around them.
isPivotCorner :: S.Set (V2 Int) -> V2 Int -> Bool
isPivotCorner corners (V2 x y) = S.size candidates == 3
  where
    -- TODO could pull in taxicab distance from that other problem
    candidates = S.filter (\(V2 x' y') -> abs (x' - x) + abs (y' - y) <= 1) corners

validSliceDirections :: S.Set (V2 Int) -> V2 Int -> [Direction]
validSliceDirections corners (V2 x y) = do
  dir <- [U, D, L, R]
  let v2 = V2 x y ==> dir
  [dir | v2 `S.member` corners]

-- TODO `Linear.V2` has a `perp` which could be interesting
rotAround :: V2 Int -> Rotation -> V2 Int -> V2 Int
rotAround pivot rot pt = pivot + rotated
  where
    pt0 = pt - pivot
    rotated = case rot of
      -- Flipped because of generally flipped orientation (up is... down).
      RotCW -> V2 (- pt0 ^. _y) (pt0 ^. _x)
      RotCCW -> V2 (pt0 ^. _y) (- pt0 ^. _x)

slice :: S.Set (V2 Int) -> V2 Int -> Direction -> (S.Set (V2 Int), S.Set (V2 Int))
slice pts pivot dir = S.partition f pts
  where
    f = case dir of
      U -> (>= pivot ^. _y) . (^. _y)
      D -> (<= pivot ^. _y) . (^. _y)
      L -> (>= pivot ^. _x) . (^. _x)
      R -> (<= pivot ^. _x) . (^. _x)

determineIncident :: S.Set (V2 Int) -> V2 Int -> V2 Int -> Maybe Direction
determineIncident corners pivot pt = inc
  where
    filt U = pt ^. _y < pivot ^. _y
    filt D = pt ^. _y > pivot ^. _y
    filt L = pt ^. _x < pivot ^. _x
    filt R = pt ^. _x > pivot ^. _x

    inc = L.find filt $ validSliceDirections corners pivot

determineRotation :: S.Set (V2 Int) -> V2 Int -> Direction -> Rotation
determineRotation corners pivot dir = rotation
  where
    dirs = validSliceDirections corners pivot
    otherDir = head . filter (/= dir) $ dirs
    -- TODO this could be a little less verbose, surely can figure it out mathematically
    rotation = case (dir, otherDir) of
      (U, R) -> RotCW
      (R, D) -> RotCW
      (D, L) -> RotCW
      (L, U) -> RotCW
      (U, L) -> RotCCW
      (L, D) -> RotCCW
      (D, R) -> RotCCW
      (R, U) -> RotCCW
      (_, _) -> error "unexpected directions at a corner"

rotateBlocks :: S.Set (V2 Int) -> State -> V2 Int -> Maybe (S.Set (V2 Int), State)
rotateBlocks corners (State pt dir) pivot = do
  incidentDir <- determineIncident corners pivot pt
  let rot = determineRotation corners pivot incidentDir

  let (static, toMove) = slice corners pivot incidentDir

  let shift = directionToV2 (opposite incidentDir)
  let unshift = directionToV2 (rotate rot incidentDir) + directionToV2 incidentDir
  let moved = S.map ((+ unshift) . rotAround pivot rot . (+ shift)) toMove
  let state' = State (rotAround pivot rot (pt + shift) + unshift) (rotate rot dir)

  return (S.union static moved, state')

isCloseEnough :: S.Set (V2 Int) -> V2 Int -> Bool
isCloseEnough corners pt =
  not
    . S.null
    . S.filter (\(V2 x y) -> x < 2 && y < 2)
    $ S.map (fmap abs . subtract pt) corners

-- Central idea - can successively rotate blocks around 270 degree corners until we match our two sides.
-- TODO Really should memoize the matched side pairs and rotations after finding once.
dfs :: Map -> S.Set (V2 Int) -> S.Set (V2 Int) -> State -> Maybe State
dfs map_ currCorners used state
  | _pos (step state) `elem` M.keys map_ = Just (step state)
  | null pivots = Nothing
  | otherwise = msum nexts
  where
    pivots =
      S.toList . S.filter (isCloseEnough (allCorners map_)) $
        S.difference (S.filter (isPivotCorner currCorners) currCorners) used
    rotations = M.mapMaybe (rotateBlocks currCorners state) pivots
    nexts = map (\(pts, state') -> dfs map_ pts (S.union used (S.fromList pivots)) state') rotations

day22b :: (Map, [Instruction]) -> Int
day22b = day22 wrap
  where
    wrap m s = M.fromJust $ dfs m (allCorners m) S.empty s
