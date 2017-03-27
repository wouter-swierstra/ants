module Controller.Event (
    eventHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Model

-- | Event handling
eventHandler :: Event -> World -> World
eventHandler = flip const
