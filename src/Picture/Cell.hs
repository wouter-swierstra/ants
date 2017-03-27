module Picture.Cell
  ( redAnt
  , blackAnt
  , clear
  , rock
  , food
  ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

clear :: Picture
clear = undefined

ant   :: Picture
ant = circleSolid 123

antWithFood :: Color -> Picture
antWithFood c =
  pictures [ color c $ ant
           , food
           ]

redAnt =
  let c = makeColorI 222 13 13 255
  in color violet $ ant
blackAnt =
  let c = makeColorI 67 59 59 255
  in color c $ ant

rock  :: Picture
rock =
  let c = makeColorI 0 0 0 255
  in undefined

food  :: Picture
food =
  let c = makeColorI 0 0 0 2555
  in undefined

