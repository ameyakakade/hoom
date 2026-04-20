{-# LANGUAGE PatternSynonyms #-}
module Render (drawScene) where

import Raylib.Core (clearBackground)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Textures (loadTexture, drawTexturePro, updateTexture)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,Color (Color) ,Texture, Rectangle, pattern Rectangle)

import qualified Data.Vector.Storable as V
import qualified Data.Word as W
import qualified Foreign.ForeignPtr as P
import qualified Foreign.Ptr as PP

import Constants
import Raystep

drawScene :: Scene -> Vector2 -> Float -> Textures -> IO ()
drawScene scene position angle textures = do
  clearBackground (Color 70 100 150 255)
  drawSkybox angle textures
  drawFloor position angle textures
  drawBars scene position angle 0 textures
  return ()

drawBars :: Scene -> Vector2 -> Float -> Int -> Textures -> IO ()
drawBars scene origin angle screenX textures
  | screenX <= width = do
                 -- strokeLine origin wall
      drawTexturePro (textures !! (wallID)) textureRect drawingRect (Vector2 0.0 0.0) 0.0 color
      drawBars scene origin angle (screenX + (floor deltaRes)) textures 
  | otherwise = return ()
  where heightR = (1/distance)*(fromIntegral height)*heightFactor
        color 
          | wallID == 5 = Color 255 0 0 255
          | otherwise = Color 255 255 255 c
        c = floor $ min ( ((20/distance)^2)*255 ) 255 
        drawingRect = Rectangle (fromIntegral screenX) ( ((fromIntegral height) - (heightR))/2 ) deltaRes heightR
        textureRect = Rectangle (interp*textureSize) 0 deltaRes textureSize
        distance = (wall |-| origin) |.| vector2Rotate (Vector2 1.0 0.0) angle
        interp = max x y
        x = snd . properFraction $ vector2'x wall
        y = snd . properFraction $ vector2'y wall
        (wall, wallID) = rayStep scene origin angledRay
        angledRay = origin |+| vector2Rotate (Vector2 collisionDistance (xinterp*planeEnds)) angle
        xinterp = (fromIntegral screenX)/(fromIntegral width) - 0.5
        deltaRes = 1.0

drawFloor :: Vector2 -> Float -> Textures -> IO ()
drawFloor position angle textures = do 
  let floorTex = textures !! 0
  let a = V.generate (512*512*3) (fn angle)
  let (p, offset, len) = V.unsafeToForeignPtr a
  let vp = P.castForeignPtr p :: P.ForeignPtr ()
  let h2 = (fromIntegral height)/2
  let drawingRect = Rectangle 0 (h2) (fromIntegral width) (h2)
  let textureRect = Rectangle 0 0 512 512

  P.withForeignPtr vp (updateTexture floorTex)
  drawTexturePro floorTex textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)

drawFloorHelper :: Vector2 -> Float -> Int -> Textures -> IO ()
drawFloorHelper position angle screenY textures
  | screenY > (div height 2) = do

      drawFloorHelper position angle (screenY-(floor deltaRes)) textures
  | otherwise = return ()
  where color = Color c c c cd
        c = floor $ interp*100
        cd = floor $ min ( ((40/distance)^2)*255 ) 255 
        interp = (fromIntegral screenY)/(fromIntegral height)
        distance = ((fromIntegral height)*heightFactor*) $ (1/) $ (fromIntegral screenY) - (fromIntegral $ div height 2)
        deltaRes = 1.0
        x = vector2'x position
        y = vector2'y position

drawSkybox :: Float -> Textures -> IO ()
drawSkybox angle textures = do
  let sky = textures !! 6
  let drawingRect = Rectangle 0 0 (fromIntegral width) ((fromIntegral height))
  let w = 1024/2
  let textureRect = Rectangle (angle*w/(2*pi)) 0 w 512

  drawTexturePro sky textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
  return ()

fn :: Float -> Int -> W.Word8
fn a i
  | a > 2 = 255
  | otherwise = if ((mod i 3)==0) then 255 else 0
