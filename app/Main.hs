{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, initWindow, setTargetFPS
                   ,getFPS, windowShouldClose, closeWindow
                   ,getMouseDelta, getMousePosition, isKeyDown
                   ,disableCursor, getFrameTime)
import Raylib.Core.Text (drawText)
import Raylib.Core.Shapes (drawRectangle, drawLine, drawLineV, drawCircle)
import Raylib.Core.Textures (loadTexture, drawTexturePro)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Util.Colors (lightGray, rayWhite, red, black, blue)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,KeyboardKey(KeyM), KeyboardKey(KeyW), KeyboardKey(KeyA)
                    ,KeyboardKey(KeyS), KeyboardKey(KeyD), Color (Color)
                    ,Texture, Rectangle, pattern Rectangle)

import Constants
import Minimap
import Raystep
import Render
import Movement

scenea :: [[Int]]
scenea = [ [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,2,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,3,0,0,0,0,4,0,0,0,3,3,3,4,0,0,0,0,1],
          [1,0,0,2,2,1,0,0,0,3,0,0,3,0,0,3,0,4,0,0,0,3,0,0,0,0,0,0,0,1],
          [1,0,0,1,0,2,0,0,0,0,0,0,0,0,0,3,0,4,0,0,0,3,3,3,0,3,0,0,0,1],
          [1,0,0,0,0,3,0,0,0,0,0,0,3,3,3,3,0,4,0,0,0,0,0,3,0,3,0,0,0,1],
          [1,0,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,3,0,3,0,0,0,1],
          [1,0,0,0,2,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,3,0,3,0,4,0,1],
          [1,0,0,0,2,2,2,2,2,1,0,0,0,0,0,2,0,4,3,0,0,4,0,0,3,4,4,4,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,1],
          [1,0,0,0,0,2,0,3,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,4,0,0,0,1],
          [1,0,0,0,0,2,0,3,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,4,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,2,2,2,4,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1] ]

cols :: Int
cols = length scenea
rows :: Int
rows = length $ head scenea

scen = (cols, rows, scenea)
  
startup :: IO AppState 
startup = do 
  window <- initWindow width height "hoom"
  let texturePaths = ["dummy.png", "wall1.png", "wall2.png", "wall3.png", "wall4.png", "error.png"]
  loadedTextures <- sequence $ map loadTexture texturePaths
  disableCursor
  return ( (playerPosition, initSpeed, playerAngle, loadedTextures) , window)

acceleration = 0.2 -- in blocks per sec
deceleration = 0.02 -- in blocks per sec
maxSpeed = 5.0 -- in blocks per sec

boolToNum :: (Num a) => Bool -> a
boolToNum b = if b then 1 else 0

mainLoop :: AppState -> IO AppState
mainLoop (state, window) =
  drawing
    ( do
        let (positionOld, velocityOld, angleOld, textures) = state
        isMDown <- isKeyDown KeyM
        
        xOffset1 <- fmap boolToNum (isKeyDown KeyW)
        yOffset1 <- fmap ((*(-1)).boolToNum) (isKeyDown KeyA)
        xOffset2 <- fmap ((*(-1)).boolToNum) (isKeyDown KeyS)
        yOffset2 <- fmap boolToNum (isKeyDown KeyD)

        time <- getFrameTime
      
        mouse <- fmap vector2'x getMouseDelta 
        let angle = angleOld + mouse*0.003

        -- add acceleration and deceleration
        let velocityDir = vector2Rotate (Vector2 (xOffset1 + xOffset2) (yOffset1 + yOffset2) ) angle
        let velocityDelta = ((vectorNormalize velocityDir) |* acceleration) |-| (velocityOld |* deceleration )
        let velocityA = velocityOld + velocityDelta
        let velocityCollision = checkCollision scen velocityA positionOld
        let velocityB = velocityA |+| velocityCollision
        let mag = magnitude velocityA
        let velocity | mag > maxSpeed = ((vectorNormalize velocityB) |* maxSpeed)
                     | otherwise = velocityB

        let position = positionOld |+| (velocity |* time)
        
     -- setTargetFPS 60
        if isMDown then drawMap scen position else drawScene scen position angle textures

        fps <- getFPS
        drawText ("FPS: " ++ (show fps)) 30 40 30 red

        return ( (position, velocity, angle, textures), window)
    )


shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (_, win)= closeWindow . Just $ win

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)  
