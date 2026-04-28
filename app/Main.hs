{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Raylib.Core (initWindow, setTargetFPS
                   ,getFPS, windowShouldClose, closeWindow
                   ,getMouseDelta, isKeyDown
                   ,disableCursor, getFrameTime)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (loadTexture, loadImage, loadRenderTexture)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vector2Rotate)
import Raylib.Util.Colors (red)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, renderTexture'texture
                    ,KeyboardKey(KeyM), KeyboardKey(KeyW), KeyboardKey(KeyA)
                    ,KeyboardKey(KeyS), KeyboardKey(KeyD), Image, image'data)

import System.IO
import System.Environment
import qualified Data.Vector.Storable as VS
import qualified Data.Word as W
import Data.Bits

import Constants
import Minimap
import Render
import Movement
import ParseLevel

startup :: IO AppState 
startup = do 
  window <- initWindow sWidth sHeight "Hoom"
  disableCursor
  state <- load "levels/level3.txt"
  return (0, state, window)

boolToNum :: (Num a) => Bool -> a
boolToNum b = if b then 1 else 0

mainLoop :: AppState -> IO AppState
mainLoop (view, state, window)
  | view == 0 = startView view state window
  | view == 1 = gameView view state window
  | otherwise = undefined

gameView :: Int -> State -> WindowResources -> IO AppState
gameView view state window = drawing
    ( do
        let (scene, (positionOld, velocityOld, angleOld), textures, canvas) = state
        let (walls, floors, sprites) = scene
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
        let velocity = updateVelocity velocityOld velocityDir positionOld walls
        let position = positionOld |+| (velocity |* time) -- setTargetFPS 60

        if (isMDown) then drawMap scene position angle else drawScene scene position angle textures canvas

        fps <- getFPS
        drawText ("FPS: " ++ (show fps)) 30 40 30 red

        return (view, (scene, (position, velocity, angle), textures, canvas), window)
    )

startView view state window = drawing
  ( do
      drawText ("This is the start screen") 30 40 30 red
      isMDown <- isKeyDown KeyM
      let newView = if isMDown then 1 else 0
      return (newView, state, window)
  )

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (_, _, win)= closeWindow . Just $ win

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
