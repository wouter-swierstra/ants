module Options where

import Data.List (isSuffixOf)
import System.Console.GetOpt
import System.Environment
import System.Exit

processArguments :: IO Options
processArguments = 
   do args <- getArgs
      let (options, arguments, errors) = getOpt Permute optionDescription args
      case (arguments, errors) of
         ([], []) -> return options
         _        -> do putStrLn "Ant Simulator"
                        putStrLn $ usageInfo "Usage: antsim [options]" optionDescription
                        exitWith (ExitFailure 1)      

optionDescription :: [OptDescr Option]
optionDescription = 
   [ Option ""  ["dumpfile"] (NoArg DumpFile)                    "Dump trace file"
   , Option ""  ["rounds"]   (ReqArg (NrOfRounds . read) "int")  "Number of rounds (default=100000)"
   , Option ""  ["frate"]    (ReqArg (FrameRate  . read) "int")  "Frames per second (default=5)"
   , Option ""  ["seed"]     (ReqArg (Seed       . read) "int")  "Seed for random numbers (default=12345)"
   , Option "r" ["red"]      (ReqArg (RedFile          ) "file") "Red ant file (mandatory)"
   , Option "b" ["black"]    (ReqArg (BlackFile        ) "file") "Black ant file (mandatory)"
   , Option "w" ["world"]    (ReqArg (WorldFile        ) "file") "World file (mandatory)"
   , Option ""  ["simulate"] (NoArg Simulate)                    "Simulation only (no gui)"
   ]

type Options = [Option] 
data Option  = Seed Int | NrOfRounds Int | DumpFile | Simulate | FrameRate Int | RedFile String | BlackFile String | WorldFile String
   deriving (Eq, Show)

getSeed :: Options -> Int
getSeed options = 
   head $ [ i | Seed i <- options ] ++ [12345]

getNrOfRounds :: Options -> Int
getNrOfRounds options =
   head $ [ i | NrOfRounds i <- options ] ++ [100000]

getFrameRate :: Options -> Int
getFrameRate options = 
   head $ [ i | FrameRate i <- options ] ++ [5]
   
getRedFile :: Options -> String
getRedFile options = 
   head $ [ if ".ant" `isSuffixOf` s then s else s ++ ".ant" 
          | RedFile s <- options 
          ] ++ [error "Specify the red ant file"]
   
getBlackFile :: Options -> String
getBlackFile options = 
   head $ [ if ".ant" `isSuffixOf` s then s else s ++ ".ant" 
          | BlackFile s <- options
          ] ++ [error "Specify the black ant file"]

getRedTeam :: Options -> String
getRedTeam = removeDirectory . dropEnd 4 . getRedFile

getBlackTeam :: Options -> String
getBlackTeam = removeDirectory . dropEnd 4 . getBlackFile

getWorldFile :: Options -> String
getWorldFile options = 
   head $ [ if ".world" `isSuffixOf` s then s else s ++ ".world" 
          | WorldFile s <- options 
          ] ++ [error "Specify the world file"]

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

removeDirectory :: String -> String
removeDirectory file = 
   case dropWhile (/= '/') file of
      _:rest -> removeDirectory rest
      []     -> file