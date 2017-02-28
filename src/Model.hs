{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RecordWildCards          #-}

module Model
  ( World (..)
  , initial
  , isAnt
  , someAntIsAt
  , antIsAlive
  , findAnt
  , killAntAt
  ) where

import Data.Array
import Data.Maybe
import Graphics.Gloss.Data.ViewPort
import System.Random

import Ant
import Grid
import Map
import Parser.StateMachineParser
import Parser.WorldParser (WorldMap)

import qualified Data.Map as M

-- Record type of the world
data World = World
  { rndGen       :: StdGen
  , ants         :: M.Map AntId AntState
  , worldmap     :: WorldMap     -- Array Position Cell
  , redprogram   :: StateMachine -- Array Pointer ANTLang
  , blackprogram :: StateMachine
  , totalTime    :: Float
  , viewports    :: [ViewPort]
  }

type RandomGeneratorSeed = Int

-- Initial world settings, requires a seed to generate random numbers from and a
-- parsed state machine for the ants
initial ::
  RandomGeneratorSeed -> StateMachine -> StateMachine -> WorldMap -> World
initial seed redprogram blackprogram worldmap = World
  { rndGen       = mkStdGen seed
  , totalTime    = 0
  , viewports    = repeat viewPortInit
  , redprogram   = redprogram
  , blackprogram = blackprogram
  , ants         = M.empty
  , worldmap     = worldmap
  }

isAnt :: Cell -> Bool
isAnt (Ant _) = True
isAnt _       = False

someAntIsAt :: Position -> World -> Bool
someAntIsAt pos world = isAnt (worldmap world ! pos)

antIsAlive :: AntId -> World -> Bool
antIsAlive antId = (isJust . M.lookup antId . ants)

findAnt :: AntId -> World -> Maybe Position
findAnt antId world =
  do p <- M.lookup antId (ants world)
     return (position p)

killAntAt :: Position -> World -> World
killAntAt pos world =
  case worldmap world ! pos of
    Ant aid -> world { ants = M.delete aid (ants world) }
    _       -> world
