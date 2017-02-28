{-# LANGUAGE RecordWildCards #-}

module Map
  ( Cell   (..)
  , Marker (..)
  ) where

import Ant

data Cell
  = Clear         -- Clear cell, ants can move here
  | Ant AntId     -- Ant with the Int as ID is on the cell
  | Food Int      -- An amount of food is on the cell
  | Marked Marker -- The cell has been marked
  | Rocky         -- Rocky cell, ants cannot move here and nothing else can be on the
                  -- cell
  | RedAnthill
  | BlackAnthill
  deriving (Eq, Show)

data Marker
  = RedMarker   Int
  | BlackMarker Int
  deriving (Eq, Show)
