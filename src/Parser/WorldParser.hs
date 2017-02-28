{-# LANGUAGE FlexibleContexts #-}

module Parser.WorldParser
  ( listToWorldMap
  , parseMapToWorldMap
  , parseMap
  , WorldMap (..)
  ) where

-- Module for parsing the world files

import Data.Array
import Grid
import Map
import Control.Monad
import Text.Parsec

type WorldMap = Array Position Cell

-- | Create a worldmap from the parsed result
listToWorldMap :: Int -> Int -> [Cell] -> WorldMap
listToWorldMap w h xs =
  let from = (0,0)
      to   = (w - 1, h - 1)
  in listArray (from, to) xs

-- | Parse a map source file directly to a worldmap
-- /This is unsafe due to the right pattern match/
parseMapToWorldMap :: String -> WorldMap
parseMapToWorldMap str =
  let Prelude.Right (w, h, xs) = parseMap str
  in listToWorldMap w h xs

type Width  = Int
type Height = Int

-- | Parse a map source file to a result list
parseMap :: String -> Either ParseError (Width, Height,[Cell])
parseMap = parse pFile ""

pFile =
  do
    w <- width
    newline
    h <- height
    newline
    top    <- pRockyBorder w
    ls     <- concat <$> replicateM (h-2) (pLine w)
    bottom <- pRockyBorder w
    eof
    return (w, h, (top ++ ls ++ bottom))

width  = integer
height = integer

-- | Primitive parsers for the map type
integer = (read :: String -> Int) <$> many1 (try digit)
unary (c, con) = (try . char) c >> return con
pRock         = unary ('#', Rocky)
pClear        = unary ('.', Clear)
pRedAnthill   = unary ('+', RedAnthill)
pBlackAnthill = unary ('-', BlackAnthill)
pFood         = integer >>= return . Food
whitespace         = unary (' ', ())

pCell =
  choice
    [ pRock
    , pClear
    , pRedAnthill
    , pBlackAnthill
    , pFood
    ] -- NOTE: There can be other things on the cells, but those are only placed
      -- there _at runtime_, so they don't have to be parsed

-- The symmetry is a little disturbed because the map uses hexagonal cells and
-- not square cells. This means that odd rows have to be parsed differently
-- than even rows and that, depending on the height of the board, the first
-- and the last row need not be the same.
--
-- Strategy: Height and width is known, construct a list of parsers that will
-- fit and run those sequentially

pRockyBorder w =
  do
    optional whitespace
    cs <- replicateM w $ do { r <- pRock; whitespace; return r }
    newline
    return cs

pLine w =
  do
    optional whitespace
    cs <- replicateM w $ do { c <- pCell; whitespace; return c }
    newline
    return cs

