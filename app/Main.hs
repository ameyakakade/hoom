{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Raylib.Core (initWindow, setTargetFPS ,getFPS, windowShouldClose, enableCursor
                   ,closeWindow ,getMouseDelta, isKeyDown, isKeyPressed ,disableCursor
                   ,getFrameTime, getRenderWidth)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (loadTexture, loadImage, loadRenderTexture)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vector2Rotate)
import Raylib.Util.Colors (red)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,renderTexture'texture ,KeyboardKey(KeyM), Rectangle(..)
                    ,KeyboardKey(KeyW), KeyboardKey(KeyA) ,KeyboardKey(KeyS)
                    ,KeyboardKey(KeyD), KeyboardKey(KeyP), Image, image'data)
import Raylib.Util.GUI (guiButton)

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
import LevelEditor

startup :: IO AppState 
startup = do 
  window <- initWindow sWidth sHeight "Hoom"
  (state, uiState) <- load "levels/level3.txt"
  return (0, state, uiState, window)

boolToNum :: (Num a) => Bool -> a
boolToNum b = if b then 1 else 0

mainLoop :: AppState -> IO AppState
mainLoop (view, state, uiState, window)
  | view == 0 = startView view state uiState window
  | view == 1 = gameView view state uiState window
  | view == 2 = gameView view state uiState window
  | view == 3 = editorView view state uiState window
  | view == 4 = changeLevel view state uiState window
  | otherwise = undefined

gameView :: Int -> State -> UIState -> WindowResources -> IO AppState
gameView view state uiState window = drawing
    ( do
        let (scene, (positionOld, velocityOld, angleOld), textures, canvas, nextLevel) = state
        let (walls, floors, sprites) = scene
        isMDown <- isKeyDown KeyM
        isPDown <- isKeyPressed KeyP
        
        xOffset1 <- fmap boolToNum (isKeyDown KeyW)
        yOffset1 <- fmap ((*(-1)).boolToNum) (isKeyDown KeyA)
        xOffset2 <- fmap ((*(-1)).boolToNum) (isKeyDown KeyS)
        yOffset2 <- fmap boolToNum (isKeyDown KeyD)

        time <- getFrameTime
      
        mouse <- fmap vector2'x getMouseDelta 
        let angle = angleOld + mouse*0.003

        -- add acceleration and deceleration
        let velocityDir = vector2Rotate (Vector2 (xOffset1 + xOffset2) (yOffset1 + yOffset2) ) angle
        let velocity    = updateVelocity velocityOld velocityDir positionOld walls
        let position    = positionOld |+| (velocity |* time) -- setTargetFPS 60

        if isMDown then drawMap scene position angle else drawScene scene position angle textures canvas

        fps <- getFPS
        drawText ("FPS: " ++ show fps) 30 40 30 red

        if isPDown then enableCursor else return ()
        let newView = (checkNextLevel position walls nextLevel) $ if isPDown && view == 2 then 3 else view

        return (newView, (scene, (position, velocity, angle), textures, canvas, nextLevel), uiState, window)
    )

startView view state uiState window = drawing
  ( do
      width <- fromIntegral <$> getRenderWidth
      let buttonWidth = 100
      startGame <- guiButton (Rectangle ((width/2) - (buttonWidth/2)) 100 buttonWidth 30) $ Just "Start Game" 
      editor <- guiButton (Rectangle ((width/2) - (buttonWidth/2)) 140 buttonWidth 30) $ Just "Open Editor" 
      let newView = if editor then 3 else if startGame then 1 else 0
      if startGame then disableCursor else return ()

      return (newView, state, uiState, window)
  )

changeLevel :: Int -> State -> UIState -> WindowResources -> IO AppState
changeLevel view state uiState window = do
  let (_, _, _, _, (_, filePath)) = state
  (newState, newUiState) <- load filePath
  disableCursor
  return (1, newState, newUiState, window)

checkNextLevel :: Vector2 -> Walls -> NextLevel -> Int -> Int
checkNextLevel position (_, cols, _) (index, _) currView = if (currView == 1 && (index == ((floor $ vector2'y position)*cols + (floor $ vector2'x position)) ) ) then 4 else currView

shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (_, _, _, win)= closeWindow . Just $ win

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
