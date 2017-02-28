{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ViewPatterns             #-}

module Main where

import Control.Applicative

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment    (getArgs)

import Graphics.Gloss
import Config
import Model
import View
import Controller
import Parser.WorldParser
import Parser.StateMachineParser

main :: IO ()
main = do
    args     <- getArgs
    time     <- round <$> getPOSIXTime

    -- undefined in initial needs to be replaced with an ant program, a world

    let redprogram      = undefined
        blackprogram    = undefined
        worldmap        = undefined
        initial'        = initial time undefined undefined undefined
        (w, h, display) = chooseDisplay args
        background      = makeColorI 0 144 33 0
        fps             = 30
    play display background fps initial' (draw w h) eventHandler timeHandler

-- | chooseDisplay
-- Choose a display mode. Note that the resolution of a full screen mode
-- should likely match the resolution of your monitor exactly.
chooseDisplay :: [String] -> (Float, Float, Display)
chooseDisplay []
    = ( defaultHorizontalResolution, defaultVerticalResolution
      , InWindow "Ants Gloss"
                 (round defaultHorizontalResolution
                 ,round defaultVerticalResolution  )
                 (100,100)
      )

chooseDisplay [read -> horizontal, read -> vertical]
    = ( fromIntegral horizontal, fromIntegral vertical
      , FullScreen (horizontal, vertical)
      )
