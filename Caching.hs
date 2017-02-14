{- To speed up:
     - drawing polygons
     - computing points on screen for cells
-}

module Caching 
   ( Cache, makeCache
   , cellCentre, antPolygon, drawPolygons
   ) where

import Graphics.UI.WX
import Simulator
import Data.Array.IO
import Data.List
import Control.Monad
import qualified Data.Array as A

data Cache = C 
   { points   :: IOArray Pos CachedPoints
   , polygons :: [Polygon]
   }

------------------------------------------------------------------------
-- Constructor function

makeCache :: Float -> GameState -> IO Cache
makeCache scale game = 
   do pts   <- computePoints scale game
      polys <- computePolygons scale game
      return (C pts polys)
   
------------------------------------------------------------------------
-- Accessor functions

cellCentre :: Pos -> Cache -> IO Point
cellCentre pos cache =
   do (p, _) <- readArray (points cache) pos   
      return p
      
antPolygon :: Pos -> Int -> Cache -> IO [Point]
antPolygon pos dir cache = 
   do (_, arr) <- readArray (points cache) pos
      return (arr A.! dir)

drawPolygons :: DC a -> Cache -> IO ()
drawPolygons dc cache = 
   let f (Polygon (ps, c)) = polygon dc ps [color := c, brushColor := c, brushKind := BrushSolid]
   in mapM_ f (polygons cache)

------------------------------------------------------------------------
-- Local definitions (not exported)

type CachedPoints = (Point, A.Array Int [Point])

computePoints :: Float -> GameState -> IO (IOArray Pos CachedPoints)
computePoints scale game = 
   do (p0, p1) <- getBounds (world game)     
      newListArray (p0, p1) (map (cachedPoints scale) (range (p0, p1)))

cachedPoints :: Float -> Pos -> CachedPoints 
cachedPoints scale pos =
   let theMiddle = toFPos scale pos
       oneAnt d  = map (fposToPoint . (+theMiddle) . scaleFPos scale) (antShape d)
   in (fposToPoint theMiddle, A.listArray (0, 5) (map oneAnt [0::Int .. 5]))

antShape :: Int -> [FPos]
antShape dir =
   let make r a = FPos (r * cos a, negate (r * sin a))
       alpha    = (fromIntegral (6 - dir) * pi) / 3
   in map (uncurry make) [(1/1.5, alpha), (1/3, alpha+pi-0.8), (1/3, alpha+pi+0.8)]

newtype Polygon = Polygon ([Point], Color)

computePolygons :: Float -> GameState -> IO [Polygon]
computePolygons scale game = 
   do b <- getBounds (world game)
      let yposMax = posY (snd b)
      rows <- mapM (makeRow game) [0..yposMax]
      makePolygons scale yposMax rows
      
type Row = [(Int, Int, Color)]

makeRow :: GameState -> Int -> IO Row
makeRow game ypos = 
   do b <- getBounds (world game)
      let xposMax = posX (snd b)
      list <- sequence [ cellColor game (Pos x ypos) | x <- [0..xposMax] ]
      return [ (x, fst (last xs), pc)
             | xs@((x, Just pc):_) <- groupBy f (zip [0..] list)
             ]
 where
    f (_, pc1) (_, pc2) = pc1 == pc2
    
cellColor :: GameState -> Pos -> IO (Maybe Color)
cellColor game pos = 
   do cell <- readArray (world game) pos
      return $ 
         case (cellType cell, anthill cell) of
            (Rocky, _ )     -> Just rockBrown
            (_, Just Red)   -> Just hillRed
            (_, Just Black) -> Just hillBlack
            _               -> Nothing

type Unfinished = (Int, Int, [Point], [Point], Color)

makePolygons :: Float -> Int -> [Row] -> IO [Polygon]
makePolygons scale yposMax rows  =
   do (unfinished, polys) <- foldM make ([], []) (zip [0..] rows)
      return (map (finish yposMax) unfinished ++ polys)
 where 
    make :: ([Unfinished], [Polygon]) -> (Int, Row) -> IO ([Unfinished], [Polygon])    
    make (unfinished, polys) (ypos, cellRow) = 
       let list = makeRec ypos cellRow unfinished
       in return ([ x | Left x <- list ], [ x | Right x <- list ] ++ polys)
    
    makeRec :: Int -> [(Int, Int, Color)] -> [Unfinished] -> [Either Unfinished Polygon]
    makeRec ypos [] unfinished = map (Right . finish (ypos-1)) unfinished
    makeRec ypos new []        = map (Left . begin ypos) new
    makeRec ypos (item1@(x1, x2, pc1):rest1) (item2@(x3, x4, _, _, pc2):rest2)
       | x1' <= x4' && x2' >= x3' && pc1 == pc2 = 
            Left (merge ypos item1 item2) : makeRec ypos rest1 rest2
       | x1' <= x3' =
            Left (begin ypos item1) : makeRec ypos rest1 (item2:rest2)
       | otherwise =
            Right (finish (ypos-1) item2) : makeRec ypos (item1:rest1) rest2

     where
       x1' = pointX $ posToPoint scale $ Pos x1 ypos
       x2' = pointX $ posToPoint scale $ Pos x2 ypos
       x3' = pointX $ posToPoint scale $ Pos (x3-1) (ypos-1)
       x4' = pointX $ posToPoint scale $ Pos (x4+1) (ypos-1)
    
    merge :: Int -> (Int, Int, Color) -> Unfinished -> Unfinished
    merge ypos (x1, x2, _) (x3, x4, ps1, ps2, pc) = 
       let new1 = drawHorAboveHalfEdge scale x3 x1 ypos
           new2 = drawHorAboveHalfEdge scale (x4+1) (x2+1) ypos
       in (x1, x2, ps1 ++ new1, ps2 ++ new2, pc)
    
    begin :: Int -> (Int, Int, Color) -> Unfinished
    begin ypos (x1, x2, pc) = 
       (x1, x2, [], drawHorAboveEdge scale x1 (x2+1) ypos, pc)
    
    finish :: Int -> Unfinished -> Polygon
    finish ypos (x1, x2, ps1, ps2, pc) =
       Polygon (ps1 ++ drawHorBelowEdge scale x1 (x2+1) ypos ++ reverse ps2, pc)


drawHorAboveEdge :: Float -> Int -> Int -> Int -> [Point]
drawHorAboveEdge scale x1 x2 ypos
   | x1 > x2   = reverse (drawHorAboveEdge scale x2 x1 ypos)
   | otherwise = toPointEdge scale 2 (Pos x1 ypos) 
               : [ toPointEdge scale d (Pos x ypos) | x <- [x1..x2-1], d <- [1, 0] ]

drawHorAboveHalfEdge :: Float -> Int -> Int -> Int -> [Point]
drawHorAboveHalfEdge scale x1 x2 ypos
   | x1' <= x2' = tail $ drawHorAboveEdge scale (if even ypos then x1 else x1-1) x2 ypos -- left to right
   | otherwise  = tail $ drawHorAboveEdge scale (if even ypos then x1+1 else x1) x2 ypos -- right to left
 where
    x1' = pointX $ posToPoint scale $ Pos x1 (ypos-1)
    x2' = pointX $ posToPoint scale $ Pos x2 ypos
    
drawHorBelowEdge :: Float -> Int -> Int -> Int -> [Point]
drawHorBelowEdge scale x1 x2 ypos
   | x1 > x2   = reverse (drawHorBelowEdge scale x2 x1 ypos)
   | otherwise = toPointEdge scale 3 (Pos x1 ypos) 
               : [ toPointEdge scale d (Pos x ypos) | x <- [x1..x2-1], d <- [4, 5] ]
     
posToPoint :: Float -> Pos -> Point
posToPoint scale = 
   fposToPoint . toFPos scale

toPointEdge :: Float -> Int -> Pos -> Point
toPointEdge scale edge pos =
   let grad  = 30 + 60 * edge 
       f i   = let r = i * pi / 180
                   x = (scale * 0.5 * cos r) / cos (pi / 6)
                   y = (scale * 0.5 * sin r)
               in FPos (x, negate y)
   in fposToPoint $ toFPos scale pos + f (fromIntegral grad)
   
------------------------------------------------------------------------
-- Float positions

newtype FPos = FPos (Float, Float) deriving (Show, Eq)

instance Num FPos where
   (FPos (x1, y1)) - (FPos (x2, y2)) = FPos (x1-x2, y1-y2) 
   (FPos (x1, y1)) * (FPos (x2, y2)) = FPos (x1*x2, y1*y2) 
   (FPos (x1, y1)) + (FPos (x2, y2)) = FPos (x1+x2, y1+y2) 
   
   negate (FPos (x, y)) = FPos (negate x, negate y)
   signum (FPos (x, y)) = FPos (signum x, signum y)
   abs    (FPos (x, y)) = FPos (abs x, abs y)
   fromInteger i        = FPos (fromInteger i, fromInteger i)

scaleFPos :: Float -> FPos -> FPos
scaleFPos f (FPos (x, y)) = FPos (f*x, f*y)

fposToPoint :: FPos -> Point
fposToPoint (FPos (x, y)) = Point (round x) (round y)

toFPos :: Float -> Pos -> FPos
toFPos scale pos = 
   let extraX = if even (posY pos) then 0 else scale/2 
   in FPos (scale * (fromIntegral (posX pos)+1) + extraX, scale * (fromIntegral (posY pos)+1) * 0.75)
   
------------------------------------------------------------------------
-- Colors

brighter :: Color -> Color
brighter c = rgb (f $ colorRed c) (f $ colorGreen c) (f $ colorBlue c)
   where f i = i + (255 - i) `div` 2

rockBrown, hillRed, hillBlack :: Color
rockBrown = rgb 166 105 41 
hillRed   = brighter (brighter red)
hillBlack = brighter (brighter black) 