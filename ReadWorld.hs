module ReadWorld (readWorld) where

import Simulator
import Data.Char (isDigit)
import Data.Array.IO (newListArray)

readWorld :: String -> IO World
readWorld worldFile =
   do s <- readFile worldFile
      case map words (lines s) of
         [dimX] : [dimY] : rest | isNum dimX && isNum dimY ->
            do let cells = map stringToCell (concat rest)
                   dx    = read dimX
                   dy    = read dimY
               w <- newListArray (Pos 0 0, Pos (dx-1) (dy-1)) cells
               if length cells /= dx * dy 
                 then error ("invalid world file: " ++ show worldFile)
                 else return w
         _ -> error ("invalid world file: " ++ show worldFile)

stringToCell :: String -> Cell
stringToCell s = 
   case s of 
      ['#'] -> stdCell { cellType = Rocky }
      ['.'] -> stdCell
      ['+'] -> stdCell { anthill = Just Red }
      ['-'] -> stdCell { anthill = Just Black }
      [_] | isNum s -> stdCell { food = read s }
      _ -> error ("invalid cell in world file: " ++ show s)
        
stdCell :: Cell
stdCell = Cell { antInCell    = Nothing 
               , cellType     = Clear
               , food         = 0
               , anthill      = Nothing
               , markersRed   = noMarkers
               , markersBlack = noMarkers 
               }
               
isNum :: String -> Bool
isNum s = all isDigit s && not (null s)