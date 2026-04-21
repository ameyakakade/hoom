{-# LANGUAGE PatternSynonyms #-}
module Render (drawScene) where

import Raylib.Core (clearBackground)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Textures (loadTexture, drawTexturePro, updateTexture)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,Color (Color) ,Texture, Rectangle, pattern Rectangle)

import qualified Data.Vector.Storable as VS
import qualified Data.Word as W
import qualified Foreign.ForeignPtr as P
import qualified Foreign.Ptr as PP

import Constants
import Raystep

texW = 1600
texH = 500

drawScene :: Scene -> Vector2 -> Float -> Textures -> FloorTex -> IO ()
drawScene scene position angle textures floorTex = do
  clearBackground (Color 70 100 150 255)
  drawSkybox angle textures
  drawFloor position angle textures floorTex
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

drawFloor :: Vector2 -> Float -> Textures -> FloorTex -> IO ()
drawFloor position angle textures floorTex = do 
  let fun = fn floorTex position angle
  let a = VS.generate (texW*texH) fun
  let floorCanvas = textures !! 0
  let (p, offset, len) = VS.unsafeToForeignPtr a
  let vp = P.castForeignPtr p :: P.ForeignPtr ()
  let h2 = (fromIntegral height)/2
  let drawingRect = Rectangle 0 (h2) (fromIntegral width) (h2)
  let textureRect = Rectangle 0 0 (fromIntegral texW) (fromIntegral texH)

  P.withForeignPtr vp (updateTexture floorCanvas)
  drawTexturePro floorCanvas textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)

fn :: FloorTex -> Vector2 -> Float -> Int -> W.Word32
fn floorTex position angle i 
  | 0 == 0 = (VS.!) floorTex (tx + ty*512)
  | otherwise = 4278295360
  where x = mod i texW
        y = div i texW
        tx = mod x 512
        ty = mod y 512

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

  -- a b g r
