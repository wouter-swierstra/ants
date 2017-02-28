{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ParallelListComp         #-}

module Controller.Time (
    timeHandler
) where

import Model

type Time a = a

-- | Time handling
timeHandler :: Time Float -> World -> World
timeHandler time world = world
