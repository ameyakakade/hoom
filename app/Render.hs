{-# LANGUAGE PatternSynonyms #-}
module Render (drawScene) where

import Raylib.Core (clearBackground)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (loadTexture, drawTexturePro, updateTexture)
import Raylib.Util(textureMode)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate, vectorLerp, magnitude)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, renderTexture'texture
                    ,Color (Color) ,Texture, Rectangle, pattern Rectangle)
import Raylib.Util.Colors (red)

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
    zBuf <- drawBars scene position left right angle 0 textures

    drawSprites position left right angle textures

    return ()
                     )
  drawTexturePro (renderTexture'texture canvas) (Rectangle 0 0 (fromIntegral width) (-(fromIntegral height)) ) (Rectangle 0 0 (fromIntegral sWidth) (fromIntegral sHeight)) (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)


drawBars :: Scene -> Vector2 -> Vector2 -> Vector2 -> Float -> Int -> Textures -> IO [Float]
drawBars scene origin left right angle screenX textures
  | screenX <= width = do
                 -- strokeLine origin wall
      drawTexturePro (textures !! (wallID)) textureRect drawingRect (Vector2 0.0 0.0) 0.0 color
      fmap (distance:) $ drawBars scene origin left right angle (screenX + (floor deltaRes)) textures
  | otherwise = return [0.0]
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
        tx = mod wx 256
        ty = mod wy 256
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

drawSkybox :: Float -> Textures -> IO ()
drawSkybox angle textures = do
  let sky = textures !! 6
  let drawingRect = Rectangle 0 0 (fromIntegral width) ((fromIntegral height))
  let w = 1024/2
  let textureRect = Rectangle (angle*w/(2*pi)) 0 w 512

  drawTexturePro sky textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
  return ()

drawSprites :: Vector2 -> Vector2 -> Vector2 -> Float -> Textures -> IO ()
drawSprites position leftV rightV angle textures = do
  let sprite = textures !! 2
  let playerDir = vector2Rotate (Vector2 1.0 0.0) angle
  let spritePos = Vector2 10.0 10.0
  let poi = spritePos |-| position
  let distance = playerDir |.| poi
  let dist = (collisionDistance /) $ (/(magnitude poi)) $ distance
  let angledRay = position |+| ((vectorNormalize poi) |* dist)

  let ar1 = vector2'x angledRay
  let ar2 = vector2'y angledRay
  let l1  = vector2'x leftV
  let l2  = vector2'y leftV
  let r1  = vector2'x rightV
  let r2  = vector2'y rightV

  let t1 = (ar1-l1)/(r1-l1)
  let t2 = (ar2-l2)/(r2-l2)
  
  let heightR = (1/distance)*(fromIntegral height)*heightFactor
  let x = t1*(fromIntegral width)

  let drawingRect = Rectangle (x-(heightR/2)) (((fromIntegral height)/2) - (heightR/2)) heightR heightR 
  let textureRect = Rectangle 0 0 256 256

  if ((t1<1.0) && (t1>0.0)) then drawTexturePro sprite textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255) else return ()
  
  drawText (show t1) 30 80 40 red
  drawText (show t2) 30 120 40 red
