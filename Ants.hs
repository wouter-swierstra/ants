{- 
   Dinner with Ambiants: ICFP Programming Contest 2004
   Programming Assignment for Advanced Functional Programming
   Universiteit Utrecht, Software Technology Master
-}

module Main (main) where

import Graphics.UI.WX hiding (when)
import GamePlay
import Options
import Simulator
import Control.Monad
import Data.Array.IO
import Data.IORef
import Data.List
import qualified Data.Set as S
import Data.Maybe
import System.Time
import Caching

main :: IO ()
main = 
   do myOptions <- processArguments
      if Simulate `elem` myOptions 
         then simulate myOptions
         else start (gui myOptions)

---------------------------------------------------------------------------------
-- Simulate only

simulate :: Options -> IO ()
simulate myOptions =
   do putStrLn $ "random seed: " ++ show (getSeed myOptions)
      game  <- makeGameState myOptions
      final <- runSimulator (allRounds myOptions) game
      putStrLn (showScore myOptions final)

showScore :: Options -> GameState -> String
showScore myOptions game = 
   unlines 
      [ "Score: round " ++ show (roundNumber game)  ++ ", " ++ getWorldFile myOptions
      , f (getRedTeam   myOptions ++ " (red) "  ) ++ ": " ++ show (redScore   $ foodAdmin game)
      , f (getBlackTeam myOptions ++ " (black) ") ++ ": " ++ show (blackScore $ foodAdmin game)
      ]
 where
   f = take 30 . (++repeat ' ') . ("   "++)

---------------------------------------------------------------------------------
-- Visualize game

type ControlWidgets = (Timer, Button (), Button (), Button (), Button ())
type InfoWidgets    = (StaticText (), StaticText (), StaticText (), StaticText (), StaticText (), StaticText (), StaticText ())
type ActiveWidgets  = (ScrolledWindow (), ControlWidgets, InfoWidgets)

gui :: Options -> IO ()
gui myOptions =
   do -- make gui data
      ref <- createGUIData myOptions
      
      -- create widgets    
      myFrame        <- frame [text := "AFP 2006: Ant Visualizer", fullRepaintOnResize := True]
      
      mainPanel      <- panel myFrame [size := sz 1000 700, bgcolor := white]
      rightPanel     <- panel mainPanel [size := sz 200 700, bgcolor := white]
      simulatorTimer <- timer mainPanel [enabled := False, interval := 1000 `div` getFrameRate myOptions]
      
      gameBoard      <- scrolledWindow mainPanel [scrollRate := sz 10 10, virtualSize := sz 1000 1000, size := sz 800 700, bgcolor := white]
      
      (overviewPanel, roundNumberText, timeRemaining, scalingSlider) 
         <- makeOverviewPanel myOptions rightPanel
      
      (controlPanel, stopButton, stepButton, startButton, finishButton, speedSlider) 
         <- makeControlPanel rightPanel
      
      (scorePanel, scoreByRed, scoreByBlack, carriedByRed, carriedByBlack, foodRemaining) <- 
         makeScorePanel rightPanel
      
      -- group widgets
      let controlWidgets :: ControlWidgets
          controlWidgets = (simulatorTimer, stopButton, stepButton, startButton, finishButton)
      
          infoWidgets :: InfoWidgets
          infoWidgets = (roundNumberText, scoreByRed, scoreByBlack, carriedByRed, carriedByBlack, foodRemaining, timeRemaining)
      
          activeWidgets :: ActiveWidgets
          activeWidgets = (gameBoard, controlWidgets, infoWidgets)
      
      -- set layout  
      set rightPanel 
         [layout := column 40 [empty, widget overviewPanel, widget controlPanel, widget scorePanel, empty]] 
      
      set mainPanel
         [layout := row 20 [ fill $ widget gameBoard, vfill $ widget rightPanel ]]
      
      set myFrame [layout := fill (widget mainPanel)]
            
      -- event handlers
      set gameBoard       [on paint   := \dc _ -> paintHandler dc ref]
      set stopButton      [on command := stopHandler controlWidgets ref]
      set stepButton      [on command := stepHandler activeWidgets ref]
      set startButton     [on command := startHandler controlWidgets ref]
      set finishButton    [on command := finishHandler controlWidgets ref]
      set simulatorTimer  [on command := timerHandler activeWidgets ref]
      set speedSlider     [on command := speedHandler speedSlider ref]
      set scalingSlider   [on command := scalingHandler scalingSlider gameBoard ref]
      
      -- initialization
      enableControl controlWidgets False
      updateInfo infoWidgets ref
      setVirtualSize gameBoard ref

-----------------------------------------------------------------------------------
-- Overview Panel

makeOverviewPanel myOptions w = 
   do p <- panel w [bgcolor := white]
      -- create widgets
      scalingSlider   <- hslider p False 160 400 [selection := 230]  --  5  10  50  -- pixels     
      roundNumberText <- staticText p [fontSize := 28, size := sz 40 20] 
      timeRemaining   <- staticText p [text := "unknown"]
      
      redTeamName   <- staticText p [bgcolor := red, color := white, text := getRedTeam myOptions ]
      blackTeamName <- staticText p [bgcolor := black, color := white, text := getBlackTeam myOptions ]
      timeBox  <- myBox p "Remaining time"
      rnrBox   <- myBox p "Round number"
      zoomBox  <- myBox p "Zoom in/out"
      -- set layout 
      set p 
         [layout := column 20
                       [ widget redTeamName
                       , widget blackTeamName
                       , rnrBox  .^. widget roundNumberText
                       , timeBox .^. widget timeRemaining
                       , zoomBox .^. widget scalingSlider
                       ]]
      -- return widgets
      return (p, roundNumberText, timeRemaining, scalingSlider)
                       
-----------------------------------------------------------------------------------
-- Control Panel

makeControlPanel w = 
   do p <- panel w [bgcolor := white]    
      -- create widgets
      stopButton   <- button p [text := "Stop"]
      stepButton   <- button p [text := "Step"]
      startButton  <- button p [text := "Start"]
      finishButton <- button p [text := "Finish"]
      speedSlider  <- hslider p False 160 555 [selection := 325]  --  5 25  250  -- p/s 
      speedBox     <- myBox p "Change speed"
      -- set layout 
      set p 
         [layout := column 20
                       [ grid 5 5 [[widget stopButton, widget stepButton], [widget startButton, widget finishButton]]
                       , speedBox .^. widget speedSlider 
                       ]]
      -- return widgets
      return (p, stopButton, stepButton, startButton, finishButton, speedSlider)

-----------------------------------------------------------------------------------
-- Score Panel

makeScorePanel w = 
   do p <- panel w [bgcolor := white]
      -- create widgets
      let makeInScore c = staticText p [fontSize := 28, size := sz 50 20, bgcolor := c, color := white ]
      scoreByRed     <- makeInScore red
      scoreByBlack   <- makeInScore black
      carriedByRed   <- makeInScore red
      carriedByBlack <- makeInScore black
      foodRemaining  <- staticText p [fontSize := 28, size := sz 50 40]
      scoreBox <- myBox p "Score"
      carryBox <- myBox p "Carried by ants"
      remBox   <- myBox p "Food remaining"
      -- set layout
      set p   
         [layout := column 20
                       [ scoreBox .^. row 0 [hfill $ widget scoreByRed, hfill $ widget scoreByBlack]
                       , carryBox .^. row 0 [hfill $ widget carriedByRed, hfill $ widget carriedByBlack]
                       , remBox   .^. row 0 [hfill $ widget foodRemaining] 
                       ]]
      -- return widgets                 
      return (p, scoreByRed, scoreByBlack, carriedByRed, carriedByBlack, foodRemaining) 
      
---------------------------------------------------------------------------------
-- Event Handlers

paintHandler :: DC () -> GUI ()
paintHandler dc ref = 
   do drawWorld dc ref
      drawFood  dc ref
      drawAnts  dc ref

stopHandler ::ControlWidgets -> GUI ()
stopHandler cws ref = 
   do enableControl cws False 
      disableTiming ref
     
stepHandler :: ActiveWidgets -> GUI ()
stepHandler = doSomeSteps 1

startHandler :: ControlWidgets -> GUI ()
startHandler cws ref = 
   do modifyIORef ref (\s -> s {fasten = False})
      enableControl cws True 
      resetTiming ref

finishHandler :: ControlWidgets -> GUI ()
finishHandler cws ref = 
   do modifyIORef ref (\s -> s {fasten = True})
      enableControl cws True
      resetTiming ref

timerHandler :: ActiveWidgets -> GUI ()
timerHandler aws ref =
   do fast  <- readFromIORef ref fasten
      steps <- if fast then return 1000 else numberOfSteps ref 
      doSomeSteps steps aws ref

speedHandler :: Slider () -> GUI ()
speedHandler speedSlider ref =
   do val    <- get speedSlider selection
      ticks  <- readFromIORef ref (getFrameRate . options) 
      let steps = round ((1.01 ^^ val) :: Double)
      modifyIORef ref (\s -> s {stepping = makeStepList steps ticks})
      resetTiming ref

scalingHandler :: Slider () -> ScrolledWindow () -> GUI ()
scalingHandler scalingSlider gameBoard ref = 
   do val <- get scalingSlider selection
      let scale = 1.01 ^^ val
      modifyIORef ref (\x -> x { scaling = scale })
      myGame  <- readFromIORef ref gameState
      myCache <- makeCache scale myGame
      modifyIORef ref (\x -> x { cache = myCache })
      setVirtualSize gameBoard ref
      repaint gameBoard

-----------------------------------------------------------
-- GUI data

type GUI a   = IORef GUIData -> IO a
data GUIData = GUIData
   { gameState  :: GameState
   , options    :: Options
   , stepping   :: [Int]
   , scaling    :: Float
   , timing     :: Timing
   , fasten     :: Bool
   , cache      :: Cache
   }
   
createGUIData :: Options -> IO (IORef GUIData)
createGUIData myOptions = 
   do game    <- makeGameState myOptions
      myCache <- makeCache scale game
      newIORef $ GUIData
             { gameState  = game
             , options    = myOptions 
             , stepping   = steps
             , scaling    = scale
             , timing     = Nothing
             , fasten     = False
             , cache      = myCache
             }
 where
   scale = 1.01 ^^ (230 :: Int)
   steps = makeStepList (round ((1.01 ^^ (325 :: Int)) :: Float)) (getFrameRate myOptions)

readFromIORef :: IORef a -> (a -> b) -> IO b
readFromIORef ref f =
   readIORef ref >>= (return . f)

useCache :: (Cache -> IO a) -> GUI a
useCache f ref = 
   readFromIORef ref cache >>= f 
   
-----------------------------------------------------------
-- Do some steps in the simulator

doSomeSteps :: Int -> ActiveWidgets -> GUI ()   
doSomeSteps steps (gameBoard, cws, iws) ref = 
   do replicateM_ steps (doOneStep ref)
      repaint gameBoard 
      updateInfo iws ref 
      updateTiming iws ref
      currRound <- readFromIORef ref (roundNumber . gameState)
      maxRounds <- readFromIORef ref (getNrOfRounds . options)
      when (currRound >= maxRounds) (enableControl cws False) 

doOneStep :: GUI () 
doOneStep ref = 
   do oldGame <- readFromIORef ref gameState
      optList <- readFromIORef ref options
      newGame <- runSimulator (oneRound optList) oldGame
      modifyIORef ref (\s -> s {gameState = newGame})
       
-----------------------------------------------------------
-- Drawing

drawWorld :: DC a -> GUI ()
drawWorld = useCache . drawPolygons
    
drawFood :: DC a -> GUI ()
drawFood dc ref =
   do scale <- readFromIORef ref scaling
      game  <- readFromIORef ref gameState
      let posList = S.elems (locations (foodAdmin game))
          f pos   = do middle <- useCache (cellCentre pos) ref
                       cell   <- readArray (world game) pos 
                       circle dc middle (foodRadius scale (food cell)) 
                          [color := foodGreen, brushColor := foodGreen, brushKind := BrushSolid]
      mapM_ f posList

foodRadius :: Float -> Int -> Int
foodRadius scale = 
   round . (/5) . (*scale) . sqrt . min 100 . fromIntegral
     
drawAnts :: DC a -> GUI ()
drawAnts dc ref =
   do scale <- readFromIORef ref scaling
      game  <- readFromIORef ref gameState
      list  <- getElems (antPositions game) 
      let f pos =
             do cell <- readArray (world game) pos
                maybe (return ()) (\a -> drawOneAnt dc scale pos a ref) (antInCell cell)
      mapM_ f (catMaybes list)

drawOneAnt :: DC a -> Float -> Pos -> Ant -> GUI ()
drawOneAnt dc scale pos ant ref =
   do ps <- useCache (antPolygon pos (antDirection ant)) ref
      let c = case antColor ant of Red -> red ; Black -> black 
      polygon dc ps [color := c, brushColor := c, brushKind := BrushSolid]
      when (antHasFood ant) $
         do middle <- useCache (cellCentre pos) ref
            circle dc middle (round (scale/5)) [color := foodGreen, brushColor := foodGreen, brushKind := BrushSolid]

-----------------------------------------------------------
-- Estimating Time Remaining

type Timing = Maybe (Int {-tick-}, Int {-roundnumber-}, ClockTime)

timingFrequency :: Int
timingFrequency = 5

resetTiming :: GUI ()
resetTiming ref =
   do rnr   <- readFromIORef ref (roundNumber . gameState)
      ctNow <- getClockTime
      modifyIORef ref (\s -> s {timing = Just (1, rnr, ctNow)})
 
disableTiming :: GUI ()
disableTiming ref =
   modifyIORef ref (\s -> s {timing = Nothing})

updateTiming :: InfoWidgets -> GUI ()
updateTiming (_, _, _, _, _, _, w) ref =
   do mTriple <- readFromIORef ref timing
      case mTriple of
         Nothing -> 
            return ()
         Just (tick, oldNr, ct) -> 
            do modifyIORef ref (\s -> s {timing = Just (tick+1, oldNr, ct)}) 
               when (tick `mod` timingFrequency == 0) $
                  do maxRound  <- readFromIORef ref (getNrOfRounds . options)
                     currRound <- readFromIORef ref (roundNumber . gameState)
                     ctNow     <- getClockTime
                     let micros = convertTimeDiff (diffClockTimes ctNow ct)
                         needed = (toInteger (maxRound - currRound) * micros) `div` (toInteger (currRound - oldNr) * (10^(6::Int)))
                         (mins, secs) = needed `divMod` 60
                     setEstimatedTime w mins secs
                 
setEstimatedTime :: Textual w => w -> Integer -> Integer -> IO ()
setEstimatedTime w mins secs =
   set w [ text := show mins ++ ":" ++ (if secs < 10 then "0" else "") ++ show secs ] 
             
convertTimeDiff :: TimeDiff -> Integer -- in micro seconds (10^6)
convertTimeDiff =
   let million :: Integer
       million = 10^(6::Int)
       f td = sum [ toInteger (tdMin td) * million * 60
                  , toInteger (tdSec td) * million
                  , (tdPicosec td) `div` million
                  ]
   in f . normalizeTimeDiff

numberOfSteps :: GUI Int
numberOfSteps ref =
   do (i:is) <- readFromIORef ref stepping
      modifyIORef ref (\s -> s {stepping = is})
      return i
            
-----------------------------------------------------------
-- Utility functions

enableControl :: ControlWidgets -> Bool -> IO ()
enableControl (tm, b1, b2, b3, b4) isRunning =
  do set tm [enabled := isRunning]
     set b1 [enabled := isRunning]
     set b2 [enabled := not isRunning]
     set b3 [enabled := not isRunning]
     set b4 [enabled := not isRunning]

updateInfo :: InfoWidgets -> GUI ()
updateInfo (t1, t2, t3, t4, t5, t6, _) ref =
   do game  <- readFromIORef ref gameState
      let setter w f = set w [text := show (f game) ]
      setter t1 roundNumber
      setter t2 (redScore     . foodAdmin)
      setter t3 (blackScore   . foodAdmin)
      setter t4 (redCarried   . foodAdmin) 
      setter t5 (blackCarried . foodAdmin)
      setter t6 (remaining    . foodAdmin)
   
setVirtualSize :: ScrolledWindow () -> GUI ()
setVirtualSize gameBoard ref = 
   do w         <- readFromIORef ref (world . gameState)
      (_, pos)  <- getBounds w
      scale <- readFromIORef ref scaling
      let dx = scale * (fromIntegral (posX pos) + 3)
          dy = scale * (fromIntegral (posY pos) + 3) * 0.75
      set gameBoard
         [virtualSize := sz (round dx) (round dy)]     

myBox :: Window a -> String -> IO (StaticText ())
myBox w s =
   staticText w [text := s, color := niceBlue, fontShape := ShapeItalic]
  
(.^.) :: StaticText () -> Layout -> Layout
st .^. l = column 5 [hfill $ widget st, l]

makeStepList :: Int -> Int -> [Int]
makeStepList steps ticks =
   let stp :: Double
       stp = fromIntegral steps / fromIntegral ticks
       difList (x:y:rest) = y - x : difList (y:rest)
       difList _          = []
   in cycle $ difList (0 : map (round . (stp*) . fromIntegral) [1..ticks])
   
foodGreen, niceBlue :: Color
foodGreen = rgb 72 239 54
niceBlue  = rgb 0 0 127