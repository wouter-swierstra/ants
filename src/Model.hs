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
import Parser.StateMachine
import Parser.World (WorldMap)

type Program  = Array Pointer ANTLang
type Colony   = Array AntId AntState

-- Record type of the world
data World = World
  { rndGen       :: StdGen
  , ants         :: Colony
  , worldmap     :: WorldMap
  , redprogram   :: Program
  , blackprogram :: Program
  , totalTime    :: Float
  , viewports    :: [ViewPort]
  }

type RandomGeneratorSeed = Int

-- Initial world settings, requires a seed to generate random numbers from and a
-- parsed state machine for the ants
initial
  :: RandomGeneratorSeed
  -> StateMachine
  -> StateMachine
  -> WorldMap
  -> World
initial seed redprogram blackprogram worldmap = World
  { rndGen       = mkStdGen seed
  , totalTime    = 0
  , viewports    = repeat viewPortInit
  , redprogram   = redprogram
  , blackprogram = blackprogram
  , ants         = listArray (0,0) []
  , worldmap     = worldmap
  }

isAnt :: Cell -> Bool
isAnt (Ant _) = True
isAnt _       = False

someAntIsAt :: Position -> World -> Bool
someAntIsAt pos world = isAnt (worldmap world ! pos)

antIsAlive :: AntId -> World -> Bool
antIsAlive antId World{..} = alive (ants ! antId)

findAnt :: AntId -> World -> Maybe Position
findAnt antId World{..} =
  let focus = ants ! antId
  in if alive focus then
    Just (position focus)
  else
    Nothing

-- Kill an ant at a position
killAntAt :: Position -> World -> World
killAntAt pos world =
  case worldmap world ! pos of
    Ant aid -> let newants = undefined
               in undefined
    _       -> world
