{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose, closeWindow, getMousePosition, isKeyDown)
import Raylib.Core.Text (drawText)
import Raylib.Core.Shapes (drawRectangle, drawLine, drawLineV, drawCircle)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vectorNormalize, vector2Rotate, vectorDistance)
import Raylib.Util.Colors (lightGray, rayWhite, red, black, blue)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, KeyboardKey(KeyM), Color (Color))

import Control.Exception (evaluate)

width :: Int 
width  = 1600
height :: Int
height = 900

cellSize :: Int
cellSize = 30

cols :: Int
cols = 15
rows :: Int
rows = 20

fov :: Float
fov = 1

scene = [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
          [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1],
          [1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1],
          [1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
          [1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1],
          [1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1],
          [1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1],
          [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]

playerPosition = (Vector2 ((*0.33) $ fromIntegral cols) ((*0.53) $ fromIntegral rows) )

startup :: IO WindowResources
startup = do
  window <- initWindow width height "hoom"
  return window

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
  drawing
    ( do
        isMDown <- isKeyDown KeyM
        --setTargetFPS 60
        if isMDown then drawMap else drawScene 
        -- drawText "Basic raylib window" 30 40 30 lightGray
    ) >> return window

drawScene :: IO ()
drawScene = do
        clearBackground blue
        mousePos <- getMousePosition
        let mousePosN = (vector2'x mousePos)/(fromIntegral width)
        let angle = 2*pi*(mousePosN)
        let playerFront = playerPosition |+| (vector2Rotate (Vector2 0.1 0.1) angle)
        drawBars playerPosition playerFront 0
        return ()

drawBars :: Vector2 -> Vector2 -> Int -> IO ()
drawBars origin ray screenX
  | screenX <= width = do
                    drawRectangle screenX (floor $ ((fromIntegral height) - (heightR))/2 ) deltaRes (floor heightR) color
                    -- strokeLine origin wall
                    drawBars origin ray (screenX + deltaRes)
  | otherwise = return ()
  where heightR = (1/distance)*(fromIntegral height)
        color = Color c c (c-30) 255
        c = (floor (100 + (150/distance) ) )
        distance = (cos angle) * (vectorDistance origin wall)
        wall = rayStep origin angledRay
        angledRay = origin |+| vector2Rotate (ray|-|origin) angle
        angle = (fov)*((fromIntegral screenX)/(fromIntegral width) - 0.5)
        deltaRes = 5
  

drawMap :: IO ()
drawMap = do
        clearBackground black
        drawGrid 
        drawCells scene 0
        mousePos <- getMousePosition
        let mousePosN = mousePos |/ fromIntegral cellSize
        let wall = rayStep playerPosition mousePosN
        strokeLine playerPosition wall 
        drawCircleOnGrid mousePosN
        drawCircleOnGrid playerPosition
        drawCircleOnGrid wall
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

rayStep :: Vector2 -> Vector2 -> Vector2
rayStep a b 
    | (vector2'x b) > fromIntegral(width)  = b
    | (vector2'y b) > fromIntegral(height) = b
    | (vector2'x b) < 0.0 = 0.0
    | (vector2'y b) < 0.0 = 0.0
    | wallID /= 0 = b
    | dx /= 0 = let k = dy/dx
                    c = (vector2'y a) - k*(vector2'x a)
                    x = snap (vector2'x b) dx
                    y = snap (vector2'y b) dy
                    nextRay1 = (Vector2 x (k*x + c))
                    nextRay2 = if k/=0 then (Vector2 ((y-c)/k) y) else (Vector2 0.0 0.0)
                    closest = if (vectorDistance nextRay1 b) > (vectorDistance nextRay2 b) then nextRay2 else nextRay1
                    in rayStep b closest
    | otherwise = (Vector2 0.0 0.0)
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
