{-# LANGUAGE FlexibleContexts #-}

module Parser.StateMachine
  ( ANTLang      (..)
  , Pointer      (..)
  , StateMachine (..)
  , Condition    (..)
  , listToAntStateMachine
  , parseAntStateMachine
  , parseAntSource
  ) where

-- Module for parsing the generated state files

import Text.Parsec
import Text.Parsec.Token (makeTokenParser, integer)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Char
import Data.Array
import Map
import Grid
import Ant

-- Language definition
data ANTLang
  = LSense Sense Pointer Pointer Condition
  | LMark Int Pointer
  | LUnmark Int Pointer
  | LPickup Pointer Pointer
  | LDrop Pointer
  | LTurn Turn Pointer
  | LMove Pointer Pointer
  | LFlip Int Pointer Pointer
  deriving (Eq, Show)

data Condition
  = CFriend
  | CFoe
  | CFriendWithFood
  | CFoeWithFood
  | CFood
  | CRock
  | CMarker Int
  | CFoeMarker
  | CHome
  | CFoeHome
  deriving (Eq, Show)

type Pointer = Int
type StateMachine = Array Int ANTLang

-- | Convert a list to an array
listToAntStateMachine :: [ANTLang] -> StateMachine
listToAntStateMachine xs = listArray (0, length xs - 1) xs

-- | Parse an ant source file to an array (StateMahine)
parseAntStateMachine :: String -> StateMachine
parseAntStateMachine str =
  let r = parseAntSource str
  in case r of
    Prelude.Left err -> error (show err)
    Prelude.Right xs -> listToAntStateMachine xs

-- | Parse an ant source file to a list of results
parseAntSource :: String -> Either ParseError [ANTLang]
parseAntSource = parse pFile ""

pFile =
  do
    optional (many endOfLine)
    f <- many pLine
    eof
    return f

pLine =
  do
    instr <- pInstruction
    optional whitespace
    optional pComment
    many1 endOfLine
    return instr

pInstruction =
  choice
    [ pSense
    , pMark
    , pUnmark
    , pPickup
    , pDrop
    , pTurn
    , pMove
    , pFlip
    ]

pComment = do
  try $ char ';'
  many (noneOf "\r\n")
  return ()

-- parses explicitly to Int
int          = (read :: String -> Int) <$> try (many1 digit)

-- whitespace, one or more spaces
whitespace = do { try . many1 $ oneOf " \t"; return () }

-- Given a (String, a) tuple, with a constructor name for a, tries to consume
-- the string a in return for the constructor.
unary (s, c) = (try . string) s >> return c

pMark =
  do
    (try . string) "Mark"
    whitespace
    i  <- int
    whitespace
    st <- int
    return (LMark i st)

pUnmark =
  do
    (try . string) "Unmark"
    whitespace
    i  <- int
    optional whitespace
    st <- int
    return (LUnmark i st)

pPickup =
  do
    (try . string) "PickUp"
    whitespace
    st1 <- int
    whitespace
    st2 <- int
    return (LPickup st1 st2)

pDrop =
  do
    (try . string) "Drop"
    whitespace
    st <- int
    return (LDrop st)

pTurn =
  do
    (try . string) "Turn"
    whitespace
    lr <- pLeftOrRight
    whitespace
    st <- int
    return (LTurn lr st)

pFlip =
  do
    (try . string) "Flip"
    whitespace
    p   <- int
    whitespace
    st1 <- int
    whitespace
    st2 <- int
    return (LFlip p st1 st2)

pSense =
  do
    (try . string) "Sense"
    whitespace
    dir <- pSenseDirection
    whitespace
    st1 <- int
    whitespace
    st2 <- int
    whitespace
    c   <- pCondition
    return (LSense dir st1 st2 c)

pMove =
  do
    (try . string) "Move"
    whitespace
    st1 <- int
    whitespace
    st2 <- int
    return (LMove st1 st2)

pSenseDirection =
  choice $
    map unary
      [ ("Here"      , Here)
      , ("Ahead"     , Ahead)
      , ("LeftAhead" , LeftAhead)
      , ("RightAhead", RightAhead)
      ]

pCondition =
  (choice $
    map unary
     [ ("FoeHome"       , CFoeHome)
     , ("FoeMarker"     , CFoeMarker)
     , ("FoeWithFood"   , CFoeWithFood)
     , ("Foe"           , CFoe)
     , ("Food"          , CFood)
     , ("FriendWithFood", CFriendWithFood)
     , ("Friend"        , CFriend)
     , ("Home"          , CHome)
     , ("Rock"          , CRock)
     ]) <|> pMarker

pMarker =
  do
    (try . string) "Marker"
    whitespace
    i <- int
    return (CMarker i)

pLeftOrRight =
  choice $
    map unary
      [ ("Left", Grid.Left)
      , ("Right", Grid.Right)
      ]

