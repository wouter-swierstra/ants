{-# LANGUAGE RecordWildCards #-}

module Ant
  ( AntState (..)
  , Colour   (..)
  , AntId    (..)
  , isBlack
  , isRed
  ) where

import Grid
import Data.Char
import Graphics.Gloss.Data.Point

-- | Team colours of the ants
data Colour = Red | Black
  deriving (Eq, Show)

isBlack, isRed :: AntState -> Bool
isBlack antS= colour antS == Black
isRed = not . isBlack

type AntId = Int

data AntState = AntState
  { antId     :: AntId     -- Unique ant ID
  , colour    :: Colour    -- Team colour of the ant
  , ptr       :: Int       -- Integer that points to the current state of the ant
  , resting   :: Int       --
  , direction :: Direction -- Current direction that the ant is traveling
  , hasFood   :: Bool      -- Does the ant carry food or not?
  , position  :: Position
  } deriving Eq

instance Show AntState where
  show AntState{..} =
    "Ant # 00000\n  "    ++ show colour
    ++ "\n  Brainstate " ++ show ptr
    ++ "\n  Resting:   " ++ show resting
    ++ "\n  Heads "      ++ map toLower (show direction)
    ++ "\n  Carries "    ++ (if hasFood then "" else "no ") ++ "food"

testAnt = AntState { antId = 0 , colour = Black , ptr = 0 , resting = 10 , direction = East , hasFood  = False, position = (0, 0)}
