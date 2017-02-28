module Picture.Cell
  ( redAnt
  , blackAnt
  , clear
  , rock
  , food
  ) where

import Graphics.Gloss

clear :: Picture
clear = undefined

ant   :: Picture
ant = circleSolid 3

redAnt =
  let c = makeColorI 222 13 13 255
  in color c $ ant
blackAnt =
  let c = makeColorI 67 59 59 255
  in color c $ ant

rock  :: Picture
rock = undefined

food  :: Picture
food = undefined

