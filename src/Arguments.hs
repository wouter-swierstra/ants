module Arguments
  ( parseAntArguments
  ) where

import System.Console.ParseArgs

type Argument = String

parseAntArguments :: [Argument] -> Args String
parseAntArguments argxs = parseArgs ArgsComplete antArguments "." argxs

antArguments =
  [ redFileArgument
  , blackFileArgument
  , worldFileArgument
  , hresArgument
  , vresArgument
  , simulateArgument
  ]

redFileArgument = Arg
  { argIndex = "red"
  , argAbbr  = Nothing
  , argName  = Just "red"
  , argData  = argDataRequired "red ai" ArgtypeString
  , argDesc  = "Specifies the ai file to be used for the red ant colony."
  }

blackFileArgument = Arg
  { argIndex = "black"
  , argAbbr  = Nothing
  , argName  = Just "black"
  , argData  = argDataRequired "black ai" ArgtypeString
  , argDesc  = "Specifies the ai file to be used for the black ant colony."
  }

worldFileArgument = Arg
  { argIndex = "world"
  , argAbbr  = Nothing
  , argName  = Just "world"
  , argData  = argDataRequired "world" ArgtypeString
  , argDesc  = "A world file."
  }

hresArgument = Arg
  { argIndex = "hres"
  , argAbbr  = Just 'h'
  , argName  = Just "hres"
  , argData  = argDataDefaulted "x-res" ArgtypeFloat 1280.0
  , argDesc  = "Width of the viewport."
  }

vresArgument = Arg
  { argIndex = "vres"
  , argAbbr  = Just 'v'
  , argName  = Just "vres"
  , argData  = argDataDefaulted "y-res" ArgtypeFloat 720.0
  , argDesc  = "Height of the viewport"
  }

simulateArgument = Arg
  { argIndex = "simulate"
  , argAbbr  = Just 's'
  , argName  = Just "simulate"
  , argData  = argDataOptional "" ArgtypeString
  , argDesc  = "Do not run render the graphics, but instead run 100 000"
               ++ " generations and print the end results to stdout"
  }
