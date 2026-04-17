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

drawMap :: Scene -> Vector2 -> IO ()
drawMap scene position = do
        clearBackground black
        drawGrid 
        drawCells scene 0
        mousePos <- getMousePosition
        let mousePosN = mousePos |/ fromIntegral cellSize
        let (wall, wallID) = rayStep scene position mousePosN
        strokeLine position wall 
        drawCircleOnGrid mousePosN
        drawCircleOnGrid position
        drawCircleOnGrid wall
        drawGrid :: IO ()
drawGrid = do
            drawRows 0
            drawCols 0

drawCells :: Scene -> Int -> IO ()
drawCells (0,0,_) _ = return ()
drawCells (x,y,list) no = do
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
    | h < height = (do 
                drawLine 0 h width h lightGray
                drawRows ( h + (cellSize) )
              )
    | otherwise = return ()

drawCols :: Int -> IO ()
drawCols w 
    | w < width = (do 
                drawLine w 0 w height lightGray
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
