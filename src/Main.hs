{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ViewPatterns             #-}

module Main where

import Control.Applicative

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment    (getArgs)
import System.Console.ParseArgs
import Data.Maybe

import Graphics.Gloss
import Model
import View
import Controller
import Parser.World
import Parser.StateMachine
import Arguments


main :: IO ()
main = do
    args     <- parseAntArguments <$> getArgs

    -- To be used as a seed for the random generator
    time <- round <$> getPOSIXTime

    -- Read the source files, the command line options are required so you can
    -- be assured that they are there and that fromJust is safe.
    redAntSource   <- readFile . fromJust $ getArgString args "red"
    blackAntSource <- readFile . fromJust $ getArgString args "black"
    worldmapSource <- readFile . fromJust $ getArgString args "world"

    let -- Parse the programs and the worldmap
        redprogram   = parseAntStateMachine redAntSource
        blackprogram = parseAntStateMachine blackAntSource
        worldmap     = parseMapToWorldMap worldmapSource

        initial'     = initial time redprogram blackprogram worldmap

        -- hres and vres are defaulted parameters so they are _always_ available
        -- in the argument vector.
        w               = fromJust $ getArgFloat args "hres"
        h               = fromJust $ getArgFloat args "vres"

        display         = InWindow "Ants Gloss" (round w, round h) (100, 100)
        background      = makeColorI 0 144 33 0
        fps             = 30
    play display background fps initial' (draw w h) eventHandler timeHandler

