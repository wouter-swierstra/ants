module GamePlay where

import Simulator
import ReadWorld
import ReadInstructions
import Options
import Data.Array.IO
import Control.Monad.State
import qualified Data.Set as S

makeGameState :: Options -> IO GameState
makeGameState options =
   do theWorld   <- readWorld (getWorldFile options)
      redInstr   <- readInstructions (getRedFile options)
      blackInstr <- readInstructions (getBlackFile options)
      
      (pm, foodPos, foodParticles) <- populateWorld theWorld
      
      return $ GameState
         { world             = theWorld
         , redInstructions   = redInstr
         , blackInstructions = blackInstr
         , antPositions      = pm
         , randoms           = randomStream (getSeed options)
         , roundNumber       = 0
         , foodAdmin         = noFood { remaining = foodParticles, locations = foodPos }
         }

populateWorld :: World -> IO (AntPositions, S.Set Pos, Int)
populateWorld theWorld = 
   do list <- getAssocs theWorld
      (nrOfAnts, pm, foodSet, nrOfFood) <- foldM op (0, [], S.empty, 0) list
      arr <- newListArray (0, nrOfAnts - 1) (reverse pm)
      return (arr, foodSet, nrOfFood)
 where
   op this@(i, pm, fm, f) (pos, cell)
      | food cell > 0 = return (i, pm, S.insert pos fm, f + food cell)
      | otherwise = 
           case anthill cell of
              Nothing -> 
                 return this
              Just c ->
                 do writeArray theWorld pos (cell { antInCell = Just (makeAnt i c) })
                    return (i+1, Just pos : pm, fm, f)
         
makeAnt :: Int -> AntColor -> Ant
makeAnt i c = Ant
   { antId        = i
   , antColor     = c
   , antState     = 0
   , antResting   = 0
   , antDirection = 0
   , antHasFood   = False
   }

allRounds :: Options -> Sim ()
allRounds options = 
   do when (DumpFile `elem` options) (liftIO (putStrLn "") >> dumpRound)
      replicateM_ (getNrOfRounds options) (oneRound options)

oneRound :: Options -> Sim ()
oneRound options = 
   do rnr <- gets roundNumber
      unless (rnr >= getNrOfRounds options) $
         do modify (\game -> game {roundNumber = roundNumber game + 1})
            pm   <- gets antPositions
            list <- liftIO $ getAssocs pm
            mapM_ step list
            when (DumpFile `elem` options) dumpRound
            
dumpRound :: Sim ()
dumpRound = 
   do w     <- gets world
      list  <- liftIO $ getAssocs w
      i     <- gets roundNumber
      liftIO $ putStrLn ((unlines $ ("After round " ++ show i ++ "...") : map showCell list))