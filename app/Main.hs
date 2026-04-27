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
  let texturePaths = ["textures/wall1.png", "textures/wall2.png", "textures/wall3.png", "textures/wall4.png", "textures/error.png", "textures/sky.png"]
  floorCanvas <- loadRenderTexture width (div height 2)
  loadedTexturesA <- sequence $ map loadTexture texturePaths
  let loadedTextures = (renderTexture'texture floorCanvas):loadedTexturesA
  
  disableCursor

  floorImg <- loadImage "textures/Ground.png"
  let floorLis = image'data floorImg
  -- converting a image which is [Word8] into vector of word32 in abgr format
  let (floorTex) = VS.fromList $ map (foldl (\acc x -> x .|. (shiftL acc 8)) 0) $ ( foldl' (\acc x -> if ( (length $ head acc)>3) then [x]:acc else ( x:(head acc)):(tail acc)) [[]] $ map fromIntegral floorLis)

  [filename] <- getArgs
  levelHandle <- openFile filename ReadMode
  contents <- hGetContents levelHandle

  let (playerData, levelData) = parseLevel contents 

  canvas <- loadRenderTexture width height

  return ( (levelData, playerData, loadedTextures, floorTex, canvas) , window)

boolToNum :: (Num a) => Bool -> a
boolToNum b = if b then 1 else 0

mainLoop :: AppState -> IO AppState
mainLoop (state, window) =
  drawing
    ( do
        let (scene, (positionOld, velocityOld, angleOld), textures, floorTex, canvas) = state
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
        let velocity = updateVelocity velocityOld velocityDir positionOld scene
        let position = positionOld |+| (velocity |* time) -- setTargetFPS 60
        if (isMDown) then drawMap scene position angle else drawScene scene position angle textures floorTex canvas

        fps <- getFPS
        drawText ("FPS: " ++ (show fps)) 30 40 30 red

        return ( (scene, (position, velocity, angle), textures, floorTex, canvas), window)
    )


shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (_, win)= closeWindow . Just $ win

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
