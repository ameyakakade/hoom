{-# LANGUAGE PatternSynonyms #-}
module Minimap (drawMap) where

import Raylib.Core (clearBackground, getMousePosition)
import Raylib.Core.Text (drawText)
import Raylib.Core.Shapes (drawRectangle, drawLine, drawLineV, drawCircle)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Util.Colors (lightGray, rayWhite, red, black, blue)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,Texture, Rectangle, pattern Rectangle)

import qualified Data.Vector.Unboxed as V

import Constants
import Raystep

drawMap :: Walls -> Vector2 -> Float -> IO ()
drawMap scene position angle = do
  clearBackground black
  drawGrid 
  drawCells scene 0
  mousePos <- getMousePosition
  let mousePosN = mousePos |/ fromIntegral cellSize
  -- let (wall, wallID) = rayStep scene position mousePosN
  -- strokeLine position wall 
  -- drawCircleOnGrid mousePosN
  drawCircleOnGrid position
  -- drawCircleOnGrid wall

  -- trying to visualise sprite rendering
  let sprite = Vector2 10.0 10.0
  drawCircleOnGrid sprite

  let left  = position |+| vector2Rotate (Vector2 collisionDistance (-planeEnds) ) angle
  let right = position |+| vector2Rotate (Vector2 collisionDistance ( planeEnds) ) angle

  strokeLine position left 
  strokeLine position right 
  strokeLine left right

  let playerDir = vector2Rotate (Vector2 1.0 0.0) angle
  let poi = sprite |-| position
  let dist = (collisionDistance /) $ (/(magnitude poi)) $ playerDir |.| poi
  let angledRay = position |+| (vectorNormalize poi) |* dist

  let t = (vectorDistance angledRay left) / (vectorDistance right left)

  strokeLine position sprite 
  drawCircleOnGrid angledRay
  drawText (show t) 30 70 40 red

  drawGrid :: IO ()

drawGrid = do
            drawRows 0
            drawCols 0

drawCells :: Walls -> Int -> IO ()
drawCells (x,y,list) no
  | list == V.empty = return ()
  | otherwise = do
      drawCellsHelper (V.take y list) no 0
      drawCells (x, y,(V.drop y list)) (no+1)

drawCellsHelper :: V.Vector Int -> Int -> Int -> IO ()
drawCellsHelper list x y
  | (list == V.empty) = return ()
  | otherwise = do
      drawCellsHelper (V.tail list) x (y+1)
      let wallID = V.head list in 
        if wallID/=0 then drawRectangle (x*cellSize) (y*cellSize) cellSize cellSize lightGray else return ()

drawRows :: Int -> IO ()
drawRows h 
    | h < sHeight = (do 
                drawLine 0 h sWidth h lightGray
                drawRows ( h + (cellSize) )
              )
    | otherwise = return ()

drawCols :: Int -> IO ()
drawCols w 
    | w < sWidth = (do 
                drawLine w 0 w sHeight lightGray
                drawCols ( w + (cellSize) )
              ) | otherwise = return ()

drawCircleOnGrid :: Vector2 -> IO ()
drawCircleOnGrid pos = drawCircle x y r red
    where x = truncate $ (vector2'x pos)*s
          y = truncate $ (vector2'y pos)*s
          r = 10
          s = fromIntegral(cellSize)

strokeLine :: Vector2 -> Vector2 -> IO ()
strokeLine posS posE = drawLineV (posS|*s) (posE|*s) red
    where s = fromIntegral(cellSize)
