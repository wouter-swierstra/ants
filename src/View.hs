{-# LANGUAGE RecordWildCards #-}

module View
  ( draw
  ) where

import Graphics.Gloss

import Config
import Model
import Ant
import Grid
import Picture.Cell

import qualified Data.Map as M

type VRes a = a
type HRes a = a


draw :: VRes Float -> HRes Float -> World -> Picture
draw vr hr world =
  let -- Debugging / Testing
      s = 10
      f cs p = zipWith (\c d -> uncurry translate (toPoint s c) d) cs (cycle [p])
      redAnts   = f [ ( 0, 0), ( 3, 5), (9, 9) ] redAnt
      blackAnts = f [ (12, 3), (23, 1), (1, 1) ] blackAnt
  in pictures $ redAnts ++ blackAnts
