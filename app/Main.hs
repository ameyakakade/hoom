{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose, closeWindow, getMousePosition)
import Raylib.Core.Text (drawText)
import Raylib.Core.Shapes (drawRectangle, drawLine, drawLineV, drawCircle)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance)
import Raylib.Util.Colors (lightGray, rayWhite, red, black)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)

width :: Int 
width  = 1000
height :: Int
height = 1000

cellSize :: Int
cellSize = 100

cols :: Int
cols = div width cellSize
rows :: Int
rows = div height cellSize

scene = [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
          [0, 0, 0, 1, 1, 1, 1, 1, 1, 0],
          [0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
          [0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
          [0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
          [0, 0, 1, 1, 0, 0, 1, 1, 1, 0],
          [0, 0, 1, 0, 0, 0, 1, 0, 0, 0],
          [0, 0, 1, 0, 0, 0, 1, 0, 0, 0],
          [0, 0, 1, 1, 1, 1, 1, 0, 0, 0],
          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

circle1 = (Vector2 ((*0.43) $ fromIntegral cols) ((*0.33) $ fromIntegral rows) )

startup :: IO WindowResources
startup = do
  window <- initWindow width height "hoom"
  setTargetFPS 60
  return window

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
  drawing
    ( do
        clearBackground black
        drawGrid 
        drawCells scene 0
        mousePos <- getMousePosition
        let mousePosN = mousePos |/ fromIntegral cellSize
        rayStep circle1 mousePosN
        strokeLine circle1 mousePosN 
        drawCircleOnGrid mousePosN
        drawCircleOnGrid circle1
        -- drawText "Basic raylib window" 30 40 30 lightGray
    ) >> return window

drawGrid :: IO ()
drawGrid = do
            drawRows 0
            drawCols 0

drawCells :: [[Int]] -> Int -> IO ()
drawCells [] _ = return ()
drawCells list no = do
                drawCellsHelper (head list) no 0
                drawCells (tail list) (no+1)

drawCellsHelper :: [Int] -> Int -> Int -> IO ()
drawCellsHelper [] _ _= return ()
drawCellsHelper list x y = do
                        drawCellsHelper (tail list) x (y+1)
                        let wallID = head list in 
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
              )
    | otherwise = return ()

drawCircleOnGrid :: Vector2 -> IO ()
drawCircleOnGrid pos = drawCircle x y r red
    where x = truncate $ (vector2'x pos)*s
          y = truncate $ (vector2'y pos)*s
          r = 10
          s = fromIntegral(cellSize)

strokeLine :: Vector2 -> Vector2 -> IO ()
strokeLine posS posE = drawLineV (posS|*s) (posE|*s) red
    where s = fromIntegral(cellSize)

rayStep :: Vector2 -> Vector2 -> IO Vector2
rayStep a b 
    | (vector2'x b) > fromIntegral(width)  = pure b
    | (vector2'y b) > fromIntegral(height) = pure b
    | (vector2'x b) < 0.0 = pure 0.0
    | (vector2'y b) < 0.0 = pure 0.0
    | wallID /= 0 = pure b
    | dx /= 0 = let k = dy/dx
                    c = (vector2'y a) - k*(vector2'x a)
                    x = snap (vector2'x b) dx
                    y = snap (vector2'y b) dy
                    nextRay1 = (Vector2 x (k*x + c))
                    nextRay2 = if k/=0 then (Vector2 ((y-c)/k) y) else (Vector2 0.0 0.0)
                    closest = if (vectorDistance nextRay1 b) > (vectorDistance nextRay2 b) then nextRay2 else nextRay1
                in (do 
                drawCircleOnGrid closest
                strokeLine b closest
                (rayStep b closest) >>= return
                )

    | otherwise = drawCircleOnGrid b >> return (Vector2 0.0 0.0)
    where dv = b |-| a
          dx = vector2'x dv
          dy = vector2'y dv
          wallID = hittingWall a b


snap :: (RealFrac b, Num a, Ord a) => b -> a -> b
snap a da
    | da > 0 = fromIntegral $ ceiling (a + 0.001) 
    | da < 0 = fromIntegral $ floor (a - 0.001)
    | otherwise = a

hittingWall :: Vector2 -> Vector2 -> Int
hittingWall a b = getWallID cx cy
    where dv = b |-| a
          dx = vector2'x dv
          dy = vector2'y dv
          bx = vector2'x b
          by = vector2'y b
          cx = fromIntegral $ abs $ floor $ bx + (signum dx)*0.001
          cy = fromIntegral $ abs $ floor $ by + (signum dy)*0.001

getWallID :: Int -> Int -> Int
getWallID y x 
    | y >= rows || x >= cols = 0
    | otherwise = (scene !! y) !! x 


shouldClose :: WindowResources -> IO Bool
shouldClose _ = windowShouldClose

teardown :: WindowResources -> IO ()
teardown = closeWindow . Just

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
