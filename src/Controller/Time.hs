{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}

module Controller.Time
  ( timeHandler
  ) where

import Data.Array
import System.Random

import qualified Data.Map as M

import Ant
import Model
import Parser.StateMachine
import Control.Monad.State
import Grid
import Map
import Parser.World

type Time a = a
type WorldState = State WorldMap


-- | Time handling
timeHandler :: Time Float -> World -> World
timeHandler time world = World
  { rndGen       = newgen
  , totalTime    = time
  , viewports    = viewports world
  , redprogram   = redprogram world
  , blackprogram = blackprogram world
  -- !important: The ants update the world separately, and then the worldmap is
  -- updated separately with this `intermediate' updated world to yield the next
  -- world.
  , ants         = mapM updateAnt (ants world) `evalState` worldmap world
  , worldmap     = undefined
  }
  where
    -- Split off newgen and `gen', the newgen should _not_ be used for random
    -- generation in this iteration.
    (newgen, gen)  = split (rndGen world)
    updateWorldMap = undefined

    -- Updating the ant will in some cases also update the world. The world
    -- exists independent of the ants but the ants /manipulate/ the world
    updateAnt :: AntState -> WorldState AntState
    updateAnt antstate =
      if dead antstate then return antstate else do -- Skip the ant if it is dead
      let w = world
          currentPos = position antstate
          currentDir = direction antstate
          currentCmd = program ! ptr antstate

          program =
            case colour antstate of
              Red   -> redprogram w
              Black -> blackprogram w

          -- The next pointer is sometimes dependent on the world.
          newPtr = case currentCmd of
            -- Nothing to do here
            LTurn   _    ptr1           -> ptr1
            LDrop        ptr1           -> ptr1
            LMark   _    ptr1           -> ptr1
            LUnmark _    ptr1           -> ptr1

            -- Generate a number between 0 and i-1, if the number is 0 then
            -- choose ptr1, ptr2 is chosen if the number is not null.
            LFlip   i    ptr1 ptr2      ->
              let (v, _) = randomR (0, i-1) gen
              in if v == 0 then ptr1 else ptr2

            -- Sense some condition and choose the pointer depending on the
            -- outcome of the sense. ptr1 will be chosen if the condition
            -- passes, ptr2 otherwise
            LSense sense ptr1 ptr2 cond ->
              {- conditionOfCell figures out in what state the cell is that
               - the ant will sense. It is basically a mappting between the
               - Condition type from Parser.StateMachine and Cells from Map.
               -}
              let conditionOfCell :: Condition
                  conditionOfCell =
                    -- Get the contents of the cell that the ant is going to
                    -- sense.
                    let myColour = colour antstate
                        iAmRed = myColour == Red
                    in case worldmap w ! sensedCell currentPos currentDir sense of
                      Ant aid ->
                        let focus = ants w ! aid
                        in case () of
                          _ | colour focus == myColour && hasFood focus
                                -> CFriendWithFood
                            | colour focus /= myColour && hasFood focus
                                -> CFoeWithFood
                            | colour focus == myColour
                                -> CFriend
                            | colour focus /= myColour
                                -> CFoe
                      Food _   -> CFood
                      Marked m ->
                        case m of
                          RedMarker i ->
                            if iAmRed then CMarker i else CFoeMarker
                          BlackMarker i ->
                            if iAmRed then CFoeMarker else CMarker i
                      Rocky    -> CRock
                      RedAnthill   ->
                        if iAmRed then CHome else CFoeHome
                      BlackAnthill ->
                        if iAmRed then CFoeHome else CHome
              in if cond == conditionOfCell then ptr1 else ptr2

            -- If the ant has to pick up food, then there has to be food in the
            -- cell to pick up, and the world has to be changed consequently
            -- because there will be less food on the same square.
            LPickup ptr1 ptr2 ->
              let currentCellHasFood = worldmap world ! sensedCell currentPos currentDir Here
              in case currentCellHasFood of
                Food _ -> ptr2
                _      -> ptr2

            -- Move the ant to ptr1 if the next cell is clear or to ptr2 if it
            -- is not clear.
            LMove ptr1 ptr2 ->
              let nextCell = worldmap w ! sensedCell currentPos currentDir Ahead
              in case nextCell of
                Clear -> ptr1
                _     -> ptr2

          newResting = resting antstate

          -- Direction changes if the ant is instructed to do so.
          newDirection =
            let dir = direction antstate
            in case currentCmd of
              LTurn turn _ -> Grid.turn turn dir
              _            -> dir

          -- Update the food that the ant is carrying. It will carry food if it
          -- already does or when it is instructed to pick up food and it is
          -- standing on a food square. And it will not carry food if it has
          -- food and drops it, or when it is instructed to pick up food where
          -- there is no food.
          newHasFood =
            if hasFood antstate then
              case currentCmd of
                LDrop _ -> False
                _       -> True
            else
              case currentCmd of
                LPickup _ _ ->
                  case worldmap w ! sensedCell currentPos currentDir Here of
                    Food _  -> True
                _           -> False

          newPosition = currentPos
          newAlive = alive antstate

      -- current state of the world map, update it with what the ant has done to
      -- it.
      wmap <- get

      return AntState
        { antId     = antId antstate -- never update this
        , colour    = colour antstate
        , ptr       = newPtr
        , resting   = newResting
        , direction = newDirection
        , hasFood   = newHasFood
        , position  = newPosition
        , alive     = newAlive
        }


