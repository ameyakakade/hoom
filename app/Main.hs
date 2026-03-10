{-# LANGUAGE TemplateHaskell #-}
module Main where

import Raylib.Core (clearBackground, initWindow, setTargetFPS, windowShouldClose, closeWindow, getMousePosition)
import Raylib.Core.Text (drawText)
import Raylib.Core.Shapes (drawRectangle, drawLine, drawCircle)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Colors (lightGray, rayWhite, red, black)
import Raylib.Types.Core (Vector2, vector2'x, vector2'y)

width :: Int
width  = 400
height :: Int
height = 400

cellSize :: Int
cellSize = 50

cols :: Int
cols = div width cellSize
rows :: Int
rows = div height cellSize

position :: (Float, Float)
position = (20,20)

startup :: IO WindowResources
startup = do
  window <- initWindow width height "raylib [core] example - basic window"
  setTargetFPS 60
  return window

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
  drawing
    ( do
        clearBackground black
        drawGrid
        getMousePosition >>= drawCircleMouse
        drawText "Basic raylib window" 30 40 30 lightGray
    ) >> return window

drawGrid :: IO ()
drawGrid = do
            drawRows 0
            drawCols 0

drawRows :: Int -> IO ()
drawRows h 
    | h < height = (do 
                drawLine 0 h width h lightGray
                drawRows ( h + (div height rows) )
              )
    | otherwise = return ()

drawCols :: Int -> IO ()
drawCols w 
    | w < width = (do 
                drawLine w 0 w height lightGray
                drawCols ( w + (div width rows) )
              )
    | otherwise = return ()

drawCircleMouse :: Raylib.Types.Core.Vector2 -> IO ()
drawCircleMouse pos = drawCircle x y r red
    where x = truncate $ vector2'x pos
          y = truncate $ vector2'y pos
          r = 20

shouldClose :: WindowResources -> IO Bool
shouldClose _ = windowShouldClose

teardown :: WindowResources -> IO ()
teardown = closeWindow . Just

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
