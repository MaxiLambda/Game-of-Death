module Game.Board (
  Board (..),
  Cell (..),
  Coordinate (..),
  Color (..),
  advanceBoard,
) where

import Data.Function (on)
import Data.List (group, maximumBy, sort)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (fromList, toList)

data Color = Red | Blue | Wall deriving (Eq, Ord, Show)

newtype Coordinate = Coordinate (Int, Int) deriving (Eq, Ord, Show)

data Cell = Cell
  { color :: !Color
  , coordinate :: !Coordinate
  }
  deriving (Eq, Ord, Show)

-- newtype Board = Board [Cell]
newtype Board = Board (Map.Map Coordinate Cell) deriving (Show)

advanceBoard :: Board -> Board
advanceBoard board@(Board boardM) =
  let surivesB = surives board
      incarnatesB = incarnates board
      neighbours = toList . fromList . concatMap neighbourCoords . Map.keys $ boardM
   in Board $
        Map.union
          (Map.filter surivesB boardM)
          (Map.fromList $ mapMaybe (\c -> incarnatesB c >>= (Just . (c,))) neighbours)

-- >>> [-1..1]
-- [-1,0,1]

neighbourCoords :: Coordinate -> [Coordinate]
neighbourCoords (Coordinate (x, y)) =
  [ Coordinate (x + x', y + y')
  | x' <- [-1 .. 1]
  , y' <- [-1 .. 1]
  , not (x' == y' && x' == 0)
  ]

neighbourCells :: Board -> Coordinate -> [Cell]
neighbourCells (Board bMap) =
  filter (\c -> Wall /= color c) . mapMaybe (`Map.lookup` bMap) . neighbourCoords

-- maybe use neighbours calculated here to optimize
-- < 2 neighbours && set: dies
-- > 3 neighbours && set: dies
surives :: Board -> Cell -> Bool
surives board cell = not (starves || overpopulates)
 where
  neighbours = neighbourCells board . coordinate $ cell
  starves = length neighbours < 2
  overpopulates = length neighbours > 3

-- = 3 neighbours && empy: incarnated
incarnates :: Board -> Coordinate -> Maybe Cell
incarnates board coord =
  if length neighbours == 3 then Just $ Cell nColor coord else Nothing
 where
  neighbours = neighbourCells board coord
  nColor = head . maximumBy (compare `on` length) . group . sort $ map color neighbours
