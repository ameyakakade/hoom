{-# LANGUAGE PatternSynonyms #-}
module Render (drawScene) where

import Raylib.Core (clearBackground)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Textures (loadTexture, drawTexturePro, updateTexture)
import Raylib.Util(textureMode)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate, vectorLerp)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, renderTexture'texture
                    ,Color (Color) ,Texture, Rectangle, pattern Rectangle)

import qualified Data.Vector.Storable as VS
import qualified Data.Word as W
import qualified Foreign.ForeignPtr as P
import qualified Foreign.Ptr as PP

import Constants
import Raystep

texW = width
texH = div height 2

type FloorPos = VS.Vector Vector2

drawScene :: Scene -> Vector2 -> Float -> Textures -> FloorTex -> Canvas -> IO ()
drawScene scene position angle textures floorTex canvas = do
  textureMode canvas (do
    clearBackground (Color 70 100 150 255)
    drawSkybox angle textures
    drawFloor position angle textures floorTex
    let left  = position |+| vector2Rotate (Vector2 collisionDistance (-planeEnds) ) angle
    let right = position |+| vector2Rotate (Vector2 collisionDistance ( planeEnds) ) angle
    drawBars scene position left right angle 0 textures
                     )
  drawTexturePro (renderTexture'texture canvas) (Rectangle 0 0 (fromIntegral width) (-(fromIntegral height)) ) (Rectangle 0 0 (fromIntegral sWidth) (fromIntegral sHeight)) (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)


drawBars :: Scene -> Vector2 -> Vector2 -> Vector2 -> Float -> Int -> Textures -> IO ()
drawBars scene origin left right angle screenX textures
  | screenX <= width = do
                 -- strokeLine origin wall
      drawTexturePro (textures !! (wallID)) textureRect drawingRect (Vector2 0.0 0.0) 0.0 color
      drawBars scene origin left right angle (screenX + (floor deltaRes)) textures 
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
        angledRay = vectorLerp left right xinterp
        xinterp = (fromIntegral screenX)/(fromIntegral width)
        deltaRes = 1.0

drawFloor :: Vector2 -> Float -> Textures -> FloorTex -> IO ()
drawFloor position angle textures floorTex = do 

  let (leftPos, rightPos) = createPosArrays position angle
  
  let fun = fn floorTex leftPos rightPos
  let a = VS.generate (texW*texH) fun
  let floorCanvas = textures !! 0
  let (p, offset, len) = VS.unsafeToForeignPtr a
  let vp = P.castForeignPtr p :: P.ForeignPtr ()
  let h2 = (fromIntegral height)/2
  let drawingRect = Rectangle 0 (h2) (fromIntegral width) (h2)
  let textureRect = Rectangle 0 0 (fromIntegral texW) (fromIntegral texH)

  P.withForeignPtr vp (updateTexture floorCanvas)
  drawTexturePro floorCanvas textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)

-- give this a array of start and end positions as vectors which it will read
-- this function is like a shader

fn :: FloorTex -> FloorPos -> FloorPos -> Int -> W.Word32
fn floorTex leftPos rightPos i 
  | 0 == 0 = (VS.!) floorTex (tx + ty*512)
  | otherwise = 4278295360
  where x = mod i texW
        y = div i texW
        tx = mod wx 512
        ty = mod wy 512
        wx = floor $ (vector2'x pos)*256
        wy = floor $ (vector2'y pos)*256
        leftV = leftPos VS.! y
        rightV = rightPos VS.! y
        pos = (vectorLerp leftV rightV ( (fromIntegral x)/(fromIntegral texW) ))

fovScaling = 1/(cos (fov/2))

createPosArrays :: Vector2 -> Float -> (FloorPos, FloorPos)
createPosArrays position angle = tLtV $ distToEnds $ map (*fovScaling) $ map yToDist [0..texH]
  where leftV  = (vector2Rotate (Vector2 1.0 0.0) ((-fov/2)+angle))
        rightV = (vector2Rotate (Vector2 1.0 0.0) (( fov/2)+angle))
        distToEndsL = \dist -> position + leftV|*dist
        distToEndsR = \dist -> position + rightV|*dist
        distToEnds = \distL -> (map distToEndsL distL, map distToEndsR distL)
        tLtV = \(l, r) -> (VS.fromList l, VS.fromList r)

yToDist :: Int -> Float
yToDist y = ( ((fromIntegral texH)*heightFactor*) $ (1/) $ yy ) :: Float
  where yy = fromIntegral y

-- drawFloorHelper :: Vector2 -> Float -> Int -> Textures -> IO ()
-- drawFloorHelper position angle screenY textures
--   | screenY > (div height 2) = do

--       drawFloorHelper position angle (screenY-(floor deltaRes)) textures
--   | otherwise = return ()
--   where color = Color c c c cd
--         c = floor $ interp*100
--         cd = floor $ min ( ((40/distance)^2)*255 ) 255 
--         interp = (fromIntegral screenY)/(fromIntegral height)
--         deltaRes = 1.0
--         x = vector2'x position
--         y = vector2'y position

drawSkybox :: Float -> Textures -> IO ()
drawSkybox angle textures = do
  let sky = textures !! 6
  let drawingRect = Rectangle 0 0 (fromIntegral width) ((fromIntegral height))
  let w = 1024/2
  let textureRect = Rectangle (angle*w/(2*pi)) 0 w 512

  drawTexturePro sky textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
  return ()

  -- a b g r
