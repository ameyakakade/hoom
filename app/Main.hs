{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (initWindow, setTargetFPS
                   ,getFPS, windowShouldClose, closeWindow
                   ,getMouseDelta, isKeyDown
                   ,disableCursor, getFrameTime)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (loadTexture)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vector2Rotate)
import Raylib.Util.Colors (red)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,KeyboardKey(KeyM), KeyboardKey(KeyW), KeyboardKey(KeyA)
                    ,KeyboardKey(KeyS), KeyboardKey(KeyD))

import System.IO
import System.Environment

import Constants
import Minimap
import Render
import Movement
import ParseLevel

startup :: IO AppState 
startup = do 
  window <- initWindow width height "hoom"
  let texturePaths = ["textures/floor_01.png", "textures/wall1.png", "textures/wall2.png", "textures/wall3.png", "textures/wall4.png", "textures/error.png", "textures/sky.png"]
  loadedTextures <- sequence $ map loadTexture texturePaths
  disableCursor

  [filename] <-getArgs
  levelHandle <- openFile filename ReadMode
  contents <- hGetContents levelHandle

  let (playerData, levelData) = parseLevel contents 

  return ( (levelData, playerData, loadedTextures) , window)

boolToNum :: (Num a) => Bool -> a
boolToNum b = if b then 1 else 0

mainLoop :: AppState -> IO AppState
mainLoop (state, window) =
  drawing
    ( do
        let (scene, (positionOld, velocityOld, angleOld), textures) = state
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
        let velocityCollision = checkCollision scene velocityA positionOld
        let velocityB = velocityA |+| velocityCollision
        let mag = magnitude velocityA
        let velocity | mag > maxSpeed = ((vectorNormalize velocityB) |* maxSpeed)
                     | otherwise = velocityB

        let position = positionOld |+| (velocity |* time)
        
     -- setTargetFPS 60
        if isMDown then drawMap scene position else drawScene scene position angle textures

        fps <- getFPS
        drawText ("FPS: " ++ (show fps)) 30 40 30 red

        return ( (scene, (position, velocity, angle), textures), window)
    )


shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (_, win)= closeWindow . Just $ win

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)  
