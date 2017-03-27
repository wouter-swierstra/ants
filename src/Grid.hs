module Grid
  ( Direction (..)
  , Position  (..)
  , Sense     (..)
  , Turn      (..)
  , adjacentCell
  , sensedCell
  , toPoint
  , turn
  ) where

import Graphics.Gloss.Data.Point

type Position = (Int, Int)

-- Mapping between `symbolic` map positions and actual Gloss coordinates
toPoint :: Float -> Position -> Point
toPoint s (x, y) = (s * fromIntegral x, s * fromIntegral y)

data Direction
  = East
  | SouthEast
  | SouthWest
  | West
  | NorthWest
  | NorthEast
  deriving (Eq, Ord, Enum, Show)

adjacentCell :: Position -> Direction -> Position
adjacentCell (x, y) d =
  case d of
    East      -> (x + 1, y)
    SouthEast -> if even y then (x    , y + 1) else (x + 1, y + 1)
    SouthWest -> if even y then (x - 1, y + 1) else (x    , y + 1)
    West      -> (x - 1, y)
    NorthWest -> if even y then (x - 1, y - 1) else (x    , y - 1)
    NorthEast -> if even y then (x    , y - 1) else (x + 1, y - 1)

data Turn = Left | Right
  deriving (Eq, Show)

turn :: Turn -> Direction -> Direction
turn t d =
  case t of
    Grid.Left  -> toEnum $ (fromEnum d + 5) `mod` 6
    Grid.Right -> toEnum $ (fromEnum d + 1) `mod` 6

data Sense
  = Here
  | Ahead
  | LeftAhead
  | RightAhead
  deriving (Eq, Show)

sensedCell :: Position -> Direction -> Sense -> Position
sensedCell p d sd =
  case sd of
    Here       -> p
    Ahead      -> adjacentCell p d
    LeftAhead  -> adjacentCell p (turn Grid.Left d)
    RightAhead -> adjacentCell p (turn Grid.Right d)

