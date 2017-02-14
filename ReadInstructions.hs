module ReadInstructions (readInstructions) where

import Data.Char (toUpper, isDigit)
import Data.Array.IO (newListArray)
import Simulator

readInstructions :: String -> IO AntInstructions
readInstructions antFile = 
   do input <- readFile antFile
      let list = map (fst . stringToInstruction) (lines (map toUpper input))
          len  = length list
      arr <- newListArray (0, len-1) list
      if len > 10000
        then error ("Too many states: " ++ show antFile)
        else return arr
      
stringToInstruction :: String -> (Instruction, Maybe String)
stringToInstruction string = 
   let (instr, rest) = break (==';') string
       comment = if null rest then Nothing else Just (tail rest)
       instruction =
          case words instr of
             ["SENSE", sensedir, st1, st2, cond] -> 
                Sense (readSenseDir sensedir) (readST st1) (readST st2) (readCond cond)
             ["SENSE", sensedir, st1, st2, "MARKER", i] -> 
                Sense (readSenseDir sensedir) (readST st1) (readST st2) (Marker (readI i))
             ["MARK", i, st] -> 
                Mark (readI i) (readST st)
             ["UNMARK", i, st] -> 
                Unmark (readI i) (readST st)
             ["PICKUP", st1, st2] -> 
                PickUp (readST st1) (readST st2)
             ["DROP", st] -> 
                Drop (readST st)
             ["TURN", lr, st] -> 
                Turn (readLR lr) (readST st)
             ["MOVE", st1, st2] -> 
                Move (readST st1) (readST st2)
             ["FLIP", p, st1, st2] -> 
                Flip (readP p) (readST st1) (readST st2)
             _ -> error ("Unknown instruction: " ++show instr)          
   in (instruction, comment)

  where
    readSenseDir s = 
       case s of 
          "HERE"       -> Here
          "AHEAD"      -> Ahead
          "LEFTAHEAD"  -> LeftAhead
          "RIGHTAHEAD" -> RightAhead
          _            -> error "invalid sensedir"
          
    readCond s =
       case s of
          "FRIEND"         -> Friend
          "FOE"            -> Foe
          "FRIENDWITHFOOD" -> FriendWithFood
          "FOEWITHFOOD"    -> FoeWithFood
          "FOOD"           -> Food
          "ROCK"           -> Rock
          "FOEMARKER"      -> FoeMarker
          "HOME"           -> Home
          "FOEHOME"        -> FoeHome
          _                -> error "invalid condition"

    readLR s = 
       case s of
          "LEFT"  -> IsLeft
          "RIGHT" -> IsRight
          _       -> error "invalid LeftRight value"
          
    readST  = (\i -> if i > 9999 then error ("State >9999: " ++ show i) else i) . readP
    readI   = (\i -> if i > 5 then i `mod` 6 else i) . readP
    readP s = if isNum s then read s else error ("Not a number: " ++ s) 
    
isNum :: String -> Bool
isNum s = all isDigit s && not (null s)