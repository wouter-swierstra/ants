{- 
   Dinner with Ambiants: ICFP Programming Contest 2004
   Programming Assignment for Advanced Functional Programming
   Universiteit Utrecht, Software Technology Master
-}

module Simulator where

import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.Array.IO
import Data.List
import Data.Int
import Data.Bits
import qualified Data.Set as S

------------------------------------------------------------------

data GameState = GameState 
   { world             :: World
   , redInstructions   :: AntInstructions 
   , blackInstructions :: AntInstructions
   , antPositions      :: AntPositions
   , randoms           :: [Int]
   , roundNumber       :: Int
   , foodAdmin         :: FoodAdmin
   }
 
type Sim = StateT GameState IO

runSimulator :: Sim () -> GameState -> IO GameState
runSimulator = execStateT

------------------------------------------------------------------
-- 2.1 Geometry

data Pos = Pos { posX :: !Int, posY :: !Int }

instance Show Pos where 
   show p = "(" ++ show (posX p) ++ ", " ++ show (posY p) ++ ")"

instance Eq Pos where
   p1 == p2 = posX p1 == posX p2 && posY p1 == posY p2

-- x and y are swapped
instance Ord Pos where
   compare p1 p2 = compare (posY p1, posX p1) (posY p2, posX p2)

-- x and y are swapped
instance Ix Pos where
   range (p1, p2) = 
      [ Pos x y | y <- [posY p1..posY p2], x <- [posX p1..posX p2] ]
      
   index (p1, p2) p = 
      (posY p - posY p1) * (1 + posX p2 - posX p1) + posX p - posX p1
      
   inRange (p1, p2) p =  
      let between :: Ord a => a -> a -> a -> Bool
          between x y z = x <= y && y <= z 
      in between (posX p1) (posX p) (posX p2) && 
         between (posY p1) (posY p) (posY p2)
         
   rangeSize (p1, p2) =
      (1 + posX p2 - posX p1) * (1 + posY p2 - posY p1)

type Dir = Int -- 0..5

adjacentCell :: Pos -> Dir -> Pos
adjacentCell p = posPlus p . f
 where
   f 0                 = Pos 1 0
   f 1 | even (posY p) = Pos 0 1 
       | otherwise     = Pos 1 1
   f 2 | even (posY p) = Pos (-1) 1
       | otherwise     = Pos 0 1
   f 3                 = Pos (-1) 0
   f 4 | even (posY p) = Pos (-1) (-1)
       | otherwise     = Pos 0 (-1)
   f 5 | even (posY p) = Pos 0 (-1) 
       | otherwise     = Pos 1 (-1)
   f d = error ("adjacentCell: " ++ show d)

   posPlus :: Pos -> Pos -> Pos
   posPlus p1 p2 = Pos { posX = posX p1 + posX p2, posY = posY p1 + posY p2 }

data LeftOrRight = IsLeft | IsRight deriving Show

turn :: LeftOrRight -> Dir -> Dir
turn IsLeft  d = (d+5) `mod` 6
turn IsRight d = (d+1) `mod` 6

data SenseDir = Here | Ahead | LeftAhead | RightAhead deriving Show

sensedCell :: Pos -> Dir -> SenseDir -> Pos
sensedCell p _ Here       = p
sensedCell p d Ahead      = adjacentCell p d
sensedCell p d LeftAhead  = adjacentCell p (turn IsLeft d)
sensedCell p d RightAhead = adjacentCell p (turn IsRight d)

------------------------------------------------------------------
-- 2.2 Biology

data AntColor = Red | Black deriving Eq

instance Show AntColor where
   show Red   = "red"
   show Black = "black"

otherColor :: AntColor -> AntColor
otherColor Red   = Black
otherColor Black = Red

data Ant = Ant 
   { antId        :: Int
   , antColor     :: AntColor
   , antState     :: Int
   , antResting   :: Int
   , antDirection :: Dir
   , antHasFood   :: Bool 
   }

instance Show Ant where
   show a = concat $ intersperse ", " 
      [ show (antColor a) ++ " ant of id " ++ show (antId a) 
      , "dir " ++ show (antDirection a)
      , "food " ++ if antHasFood a then "1" else "0"
      , "state " ++ show (antState a)
      , "resting " ++ show (antResting a)
      ]

setState :: Int  -> Ant -> Ant
setState x ant = ant {antState = x}

setResting :: Int  -> Ant -> Ant
setResting x ant = ant {antResting = x}

setDirection :: Dir  -> Ant -> Ant
setDirection x ant = ant {antDirection = x}

setHasFood :: Bool -> Ant -> Ant
setHasFood x ant = ant {antHasFood = x}

------------------------------------------------------------------
-- 2.3 Geography

rocky :: Cell -> Bool
rocky c = cellType c == Rocky 

maybeAntAt :: Pos -> Sim (Maybe Ant)
maybeAntAt p = 
   do w <- gets world
      cell <- liftIO $ readArray w p
      return (antInCell cell)
      
setAntAt :: Ant -> Pos -> Sim ()
setAntAt a p = 
   do setAntPosition p a
      changeCellAt (\cell -> cell { antInCell = Just a }) p
      
clearAntAt :: Ant -> Pos -> Sim ()
clearAntAt a p = 
   do setAntNoPosition a
      changeCellAt (\cell -> cell { antInCell = Nothing }) p

------------------------------------------------------------------
-- 2.4 Cartography
--    (see ReadWorld.hs)

type World = IOArray Pos Cell

data Cell = Cell 
   { antInCell    :: Maybe Ant
   , cellType     :: CellType
   , food         :: Int
   , anthill      :: Maybe AntColor
   , markersRed   :: Markers
   , markersBlack :: Markers
   }

data CellType = Rocky | Clear deriving (Eq, Show)

showCell :: (Pos, Cell) -> String
showCell (p, cell)
   | rocky cell = "cell " ++ show p ++ ": rock"
   | otherwise =
        let begin      = "cell " ++ show p ++ ": "
            hill       = maybe "" (\c -> show c ++ " hill; ") (anthill cell)
            g c bs     = if anyMarker bs then show c ++ " marks: " ++ showMarkers bs ++ "; " else ""
            redmarks   = g Red   (markersRed   cell)
            blackmarks = g Black (markersBlack cell)
            foodtext   = if food cell > 0 then show (food cell) ++ " food; " else ""
            anttext    = maybe "" show (antInCell cell)
        in begin ++ foodtext ++ hill ++ redmarks ++ blackmarks ++ anttext
        
------------------------------------------------------------------
-- 2.5 Chemistry

type MarkerNumber = Int -- 0..5
type Markers      = Int8

noMarkers :: Markers
noMarkers = 0

anyMarker :: Markers -> Bool
anyMarker = (/= 0)

showMarkers :: Markers -> String
showMarkers m = 
   concat [ show i | i <- [0..5], testBit m i]

setMarkerAt :: AntColor -> MarkerNumber -> Cell -> Cell
setMarkerAt c m cell =
   case c of 
      Red   -> cell {markersRed   = setBit (markersRed cell)   m}
      Black -> cell {markersBlack = setBit (markersBlack cell) m}

clearMarkerAt :: AntColor -> MarkerNumber -> Cell -> Cell      
clearMarkerAt c m cell = 
   case c of 
      Red   -> cell {markersRed   = clearBit (markersRed cell)   m}
      Black -> cell {markersBlack = clearBit (markersBlack cell) m}

------------------------------------------------------------------
-- 2.6 Phenomenology

data Condition = Friend | Foe | FriendWithFood | FoeWithFood | Food | Rock | Marker MarkerNumber | FoeMarker | Home | FoeHome 
   deriving Show

cellMatches :: Cell -> Condition -> AntColor -> Bool
cellMatches cell cond c =
   case (cond, antInCell cell) of
      (Rock          , _       ) -> rocky cell
      (Food          , _       ) -> food cell > 0
      (Home          , _       ) -> anthill cell == Just c
      (FoeHome       , _       ) -> anthill cell == Just (otherColor c)
      (Marker i      , _       ) -> testBit   (case c of Red -> markersRed cell  ; Black -> markersBlack cell) i
      (FoeMarker     , _       ) -> anyMarker (case c of Red -> markersBlack cell; Black -> markersRed cell  )
      (_             , Nothing ) -> False
      (Friend        , Just ant) -> antColor ant == c
      (Foe           , Just ant) -> antColor ant /= c
      (FriendWithFood, Just ant) -> antColor ant == c && antHasFood ant
      (FoeWithFood   , Just ant) -> antColor ant /= c && antHasFood ant

------------------------------------------------------------------
-- 2.7 Neurology

type AntState = Int

data Instruction 
   = Sense SenseDir AntState AntState Condition
   | Mark MarkerNumber AntState
   | Unmark MarkerNumber AntState
   | PickUp AntState AntState
   | Drop AntState
   | Turn LeftOrRight AntState
   | Move AntState AntState
   | Flip Int AntState AntState
 deriving Show
   
getInstruction :: AntColor -> AntState -> Sim Instruction
getInstruction c st = 
   do instrs <- case c of Red -> gets redInstructions ; Black -> gets blackInstructions
      liftIO $ readArray instrs st

------------------------------------------------------------------
-- 2.8 Neuro-Cartography
--    (see ReadInstructions.hs)

type AntInstructions = IOArray Int Instruction
 
------------------------------------------------------------------
-- 2.9 Martial Arts

adjacentAnts :: Pos -> AntColor -> Sim Bool
adjacentAnts p c =
   do free <- foldM op 0 [0..5]
      return (free <= 1)
 where
   op :: Int -> Dir -> Sim Int
   op free d 
      | free >= 2 = return free
      | otherwise = 
           do mAnt <- maybeAntAt (adjacentCell p d)
              return (free + maybe 1 (\ant -> if antColor ant == c then 0 else 1) mAnt)

checkForSurroundedAntAt :: Pos -> Sim ()
checkForSurroundedAntAt p = 
   do mAnt <- maybeAntAt p
      case mAnt of
         Nothing  -> return ()
         Just ant ->
            do kill <- adjacentAnts p (otherColor (antColor ant))
               when kill $
                  do clearAntAt ant p
                     changeCellAt (addFood $ 3 + if antHasFood ant then 1 else 0) p
                     cell <- cellAt p
                     scoreKill ant cell
                     setFoodAtPos (food cell) p

checkForSurroundedAnts :: Pos -> Sim ()
checkForSurroundedAnts p =
   do checkForSurroundedAntAt p
      mapM_ (checkForSurroundedAntAt . adjacentCell p) [0..5]

------------------------------------------------------------------
-- 2.10 Number Theory

randomStream :: Int -> [Int]
randomStream = 
   let f :: Int -> Int
       f x = x * 22695477 + 1
       g :: Int -> Int
       g x = (x `div` 65536) `mod` 16384
   in map g . drop 4 . iterate f

randomInt :: Int -> Sim Int
randomInt n = 
   do i:is <- gets randoms
      modify (\s -> s { randoms = is })
      return (i `mod` n)

------------------------------------------------------------------
-- 2.11 Kinetics

step :: (Int, Maybe Pos) -> Sim ()
step (_, Nothing)  = return ()
step (i, Just pos) = 
   do cell <- cellAt pos
      case (antInCell cell) of
         Nothing -> return ()
         Just ant
            | antResting ant > 0 || antId ant /= i -> 
                changeCellAt (changeAnt $ changeResting (\x -> x-1)) pos
            | otherwise ->
                 do instruction <- getInstruction (antColor ant) (antState ant)
                    f <- doInstruction pos cell ant instruction
                    changeCellAt f pos

doInstruction :: Pos -> Cell -> Ant -> Instruction -> Sim (Cell -> Cell)
doInstruction pos cell ant instruction =
   case instruction of 

      Sense sensedir st1 st2 cond -> 
         do let newpos = sensedCell pos (antDirection ant) sensedir
            newCell <- cellAt newpos
            let b = cellMatches newCell cond (antColor ant)
            return $ 
               changeAnt (setState (if b then st1 else st2))
                           
      Mark i st -> 
         return $ 
            setMarkerAt (antColor ant) i .
            changeAnt (setState st)

      Unmark i st -> 
         return $ 
            clearMarkerAt (antColor ant) i .
            changeAnt (setState st)
                        
      PickUp st1 st2
         | antHasFood ant || food cell == 0 -> 
              return $ 
                 changeAnt (setState st2)
         | otherwise ->
              do scorePickup ant cell
                 setFoodAtPos (food cell - 1) pos
                 return $ 
                    addFood (-1) .
                    changeAnt (setState st1 . setHasFood True)
                                    
      Drop st
         | antHasFood ant -> 
              do scoreDrop ant cell
                 setFoodAtPos (food cell) pos
                 return $
                    addFood 1 .
                    changeAnt (setState st . setHasFood False)
         | otherwise -> 
              return $
                 changeAnt (setState st)
                        
      Turn lr st -> 
         return $ 
            changeAnt (setState st . setDirection (turn lr (antDirection ant)))
                                
      Move st1 st2 ->
         do let newpos = adjacentCell pos (antDirection ant)
            newCell <- cellAt newpos
            if rocky newCell || isJust (antInCell newCell)
               then 
                  return (changeAnt (setState st2)) 
               else
                  do clearAntAt ant pos
                     setAntAt (setResting 14 . setState st1 $ ant) newpos
                     checkForSurroundedAnts newpos
                     return id
                        
      Flip n st1 st2 ->
         do i <- randomInt n
            return $ 
               changeAnt (setState $ if i == 0 then st1 else st2)

changeAnt :: (Ant -> Ant) -> Cell -> Cell
changeAnt f c = 
   c { antInCell = fmap f (antInCell c) }

addFood :: Int -> Cell -> Cell
addFood extra c = 
   c { food = extra + food c }

changeResting :: (Int -> Int) -> Ant -> Ant
changeResting f a = 
   a { antResting = f (antResting a) }
   
------------------------------------------------------------------
-- 2.12 Game Play and Scoring
--    (see GamePlay.hs)

------------------------------------------------------------------
-- Food Administration

data FoodAdmin = FoodAdmin 
   { blackScore   :: Int
   , redScore     :: Int
   , blackCarried :: Int
   , redCarried   :: Int
   , remaining    :: Int
   , locations    :: S.Set Pos
   }

changeFoodAdmin :: (FoodAdmin -> FoodAdmin) -> Sim ()
changeFoodAdmin f = 
   modify (\s -> s { foodAdmin = f (foodAdmin s) })

noFood :: FoodAdmin 
noFood = FoodAdmin 0 0 0 0 0 S.empty

changeScore :: Maybe AntColor -> (Int -> Int) -> FoodAdmin -> FoodAdmin
changeScore Nothing      f score = score { remaining  = f (remaining score)}
changeScore (Just Red)   f score = score { redScore   = f (redScore score) }
changeScore (Just Black) f score = score { blackScore = f (blackScore score) }

changeCarried :: AntColor -> (Int -> Int) -> FoodAdmin-> FoodAdmin
changeCarried Red   f score = score { redCarried   = f (redCarried score) }
changeCarried Black f score = score { blackCarried = f (blackCarried score) }

scoreDrop :: Ant -> Cell -> Sim ()
scoreDrop ant cell =
   changeFoodAdmin ( changeCarried (antColor ant) (\x -> x-1)
                   . changeScore (anthill cell) (+1)      
                   )

scorePickup :: Ant -> Cell -> Sim ()
scorePickup ant cell =
   changeFoodAdmin ( changeCarried (antColor ant) (+1)
                   . changeScore (anthill cell) (\x -> x-1)
                   )

scoreKill :: Ant -> Cell -> Sim ()
scoreKill ant cell =
   changeFoodAdmin ( changeCarried (antColor ant) (if antHasFood ant then (\x -> x-1) else id)
                   . changeScore (anthill cell) (if antHasFood ant then (+4) else (+3))
                   )

setFoodAtPos :: Int -> Pos -> Sim ()
setFoodAtPos i pos = 
   changeFoodAdmin (\s -> s { locations = if i==0 then S.delete pos (locations s)  
                                                  else S.insert pos (locations s)})
                                                  
------------------------------------------------------------------
-- Ant Positions        
                                               
type AntPositions = IOArray Int (Maybe Pos)

setAntMaybePosition :: Maybe Pos -> Ant -> Sim ()
setAntMaybePosition mpos ant = 
   do pm <- gets antPositions
      liftIO $ writeArray pm (antId ant) mpos
   
setAntPosition :: Pos -> Ant -> Sim ()
setAntPosition = setAntMaybePosition . Just

setAntNoPosition :: Ant -> Sim ()
setAntNoPosition = setAntMaybePosition Nothing

cellAt :: Pos -> Sim Cell
cellAt pos =
   do arr <- gets world
      liftIO $ readArray arr pos

changeCellAt :: (Cell -> Cell) -> Pos -> Sim ()
changeCellAt f p = 
   do arr  <- gets world
      cell <- liftIO $ readArray arr p
      liftIO (writeArray arr p (f cell))