{-# LANGUAGE PatternSynonyms #-}
module Render (drawScene) where

import Raylib.Core (clearBackground)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (loadTexture, drawTexturePro, updateTexture)
import Raylib.Util(textureMode)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate, vectorLerp, magnitude, lerp, clamp)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, renderTexture'texture
                    ,Color (Color) ,Texture, Rectangle, pattern Rectangle)
import Raylib.Util.Colors (red)

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V
import qualified Data.Word as W
import qualified Foreign.ForeignPtr as P
import qualified Foreign.Ptr as PP
import Data.List

import Constants
import Raystep

texW = width
texH = div height 2

drawScene :: Scene -> Vector2 -> Float -> Textures -> Canvas -> IO ()
drawScene (scene, floors, sprites) position angle (wallTex, floorTex, floorCanvas, spriteTex) canvas = do
  textureMode canvas (do
    clearBackground (Color 70 100 150 255)

    drawSkybox angle wallTex
    drawFloor position angle floors floorTex floorCanvas

    let left  = position |+| vector2Rotate (Vector2 collisionDistance (-planeEnds) ) angle
    let right = position |+| vector2Rotate (Vector2 collisionDistance   planeEnds  ) angle
    zBuf <- drawBars scene position left right angle 0 wallTex

    drawSprites sprites zBuf position angle spriteTex
    return ()
                     )
  drawTexturePro (renderTexture'texture canvas) (Rectangle 0 0 (fromIntegral width) (-(fromIntegral height)) ) (Rectangle 0 0 (fromIntegral sWidth) (fromIntegral sHeight)) (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)


drawBars :: Walls -> Vector2 -> Vector2 -> Vector2 -> Float -> Int -> WallTextures -> IO [Float]
drawBars scene origin left right angle screenX textures
  | screenX <= width = do
                 -- strokeLine origin wall
      drawTexturePro (textures !! wallID) textureRect drawingRect (Vector2 0.0 0.0) 0.0 color
      (distance:) <$> drawBars scene origin left right angle (screenX + floor deltaRes) textures
  | otherwise = return [0.0]
  where heightR = (1/distance)* fromIntegral height *heightFactor
        color 
          | wallID == 5 = Color 255 0 0 255
          | otherwise = Color 255 255 255 c
        c              = floor $ min ( ((20/distance)^2)*255 ) 255 
        drawingRect    = Rectangle (fromIntegral screenX) ( (fromIntegral height - heightR)/2 ) deltaRes heightR
        textureRect    = Rectangle (interp*textureSize) 0 deltaRes textureSize
        distance       = (wall |-| origin) |.| vector2Rotate (Vector2 1.0 0.0) angle
        interp         = max x y
        x              = snd . properFraction $ vector2'x wall
        y              = snd . properFraction $ vector2'y wall
        (wall, wallID) = rayStep scene origin angledRay
        angledRay      = vectorLerp left right xinterp
        xinterp        = fromIntegral screenX/fromIntegral width
        deltaRes       = 1.0

drawFloor :: Vector2 -> Float -> Floors -> FloorTextures -> FloorCanvas -> IO ()
drawFloor position angle floors floorTex floorCanvas = do 

  let (leftPos, rightPos) = createPosArrays position angle
  
  let fun              = fn floors floorTex leftPos rightPos
  let a                = VS.generate (texW*texH) fun
  let (p, offset, len) = VS.unsafeToForeignPtr a
  let vp               = P.castForeignPtr p :: P.ForeignPtr ()
  let h2               = fromIntegral height/2
  let drawingRect      = Rectangle 0 h2 (fromIntegral width) h2
  let textureRect      = Rectangle 0 0 (fromIntegral texW) (fromIntegral texH)

  P.withForeignPtr vp (updateTexture floorCanvas)
  drawTexturePro floorCanvas textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)

-- give this a array of start and end positions as vectors which it will read
-- this function is like a shader

type FloorPos = VS.Vector Vector2

fn :: Floors -> FloorTextures -> FloorPos -> FloorPos -> Int -> W.Word32
fn floors floorTextures leftPos rightPos i 
  | 0 == 0 = (VS.!) floorTex (tx + ty*512)
  | otherwise = 4278295360
  where x        = mod i texW
        y        = div i texW
        tx       = mod wx 256
        ty       = mod wy 256
        wx       = floor $ vector2'x pos*256
        wy       = floor $ vector2'y pos*256
        leftV    = leftPos VS.! y
        rightV   = rightPos VS.! y
        pos      = vectorLerp leftV rightV ( fromIntegral x/fromIntegral texW )
        floorTex = floorTextures !! id'
        id'      = getFloorID floors (floor $ vector2'y pos) (floor $ vector2'x pos)

getFloorID :: Floors -> Int -> Int -> Int
getFloorID (cols, rows, floors) y x 
  | y < 0 || x < 0 = 0
  | y >= cols || x >= rows = 0
  | otherwise = floors V.! (rows*y + x) 

createPosArrays :: Vector2 -> Float -> (FloorPos, FloorPos)
createPosArrays position angle = tLtV $ distToEnds $ map ((*fovScaling) . yToDist) [0..texH]
  where leftV            = vector2Rotate (Vector2 1.0 0.0) (-(fov/2)+angle)
        rightV           = vector2Rotate (Vector2 1.0 0.0) ( (fov/2)+angle)
        distToEndsL dist = position + leftV|*dist
        distToEndsR dist = position + rightV|*dist
        distToEnds distL = (map distToEndsL distL, map distToEndsR distL)
        tLtV (l, r)      = (VS.fromList l, VS.fromList r)

fovScaling = 1/ cos (fov/2)

yToDist :: Int -> Float
yToDist y = ( (fromIntegral texH*heightFactor*) $ 1/yy ) :: Float
  where yy = fromIntegral y

drawSkybox :: Float -> WallTextures -> IO ()
drawSkybox angle textures = do
  let sky         = head textures
  let drawingRect = Rectangle 0 0 (fromIntegral width) (fromIntegral height)
  let w           = 1024/2
  let textureRect = Rectangle (angle*w/(2*pi)) 0 w 512

  drawTexturePro sky textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
  return ()


drawSprites :: StaticSprites -> [Float] -> Vector2 -> Float -> SpriteTextures -> IO ()
drawSprites sprites zBuf position angle textures = do
  let sprite    = textures !! 0

  let spriteList = sortBy (\(d1, _, _, _) (d2, _, _, _) -> compare d2 d1 ) $ map (getXandDist position angle) sprites
  let fn (distance, xL, xR, id) = drawSpriteHelper (textures !! id) zBuf distance xL xR

  mapM_ fn spriteList
  return ()

drawSpriteHelper sprite zBuf distance xL xR = if distance>0 && (xL>=0 || xR>=0)
                   then drawBarSprites distance xL xL xR zBuf sprite
                   else return ()
  


drawBarSprites :: Float -> Float -> Float -> Float -> [Float] -> Texture -> IO ()
drawBarSprites distance x xL xR zBuf texture
  | x <= xR = do
      if distance < zBuf !! floor (clamp 0.0 (fromIntegral width) x)
        then
        drawTexturePro texture textureRect drawingRect (Vector2 0.0 0.0) 0.0 color
        else return ()
      drawBarSprites distance (x + deltaRes) xL xR zBuf texture
  | otherwise = return ()
  where heightR = (1/distance)*fromIntegral height*heightFactor
        color = Color 255 255 255 c
        c = floor $ min ( ((20/distance)^2)*255 ) 255 
        drawingRect = Rectangle x ((fromIntegral height/2) - (heightR/2)) deltaRes heightR 
        textureRect = Rectangle (fromIntegral $ floor $ xinterp*textureSize) 0 deltaRes textureSize
        xinterp = (xR - x)/(xR-xL)
        deltaRes = 1

getXandDist position angle (id, spritePos) = (distance, xL, xR, id)
 where poi = spritePos - position
       distance = playerDir |.| poi
       leftV  = position |+| vector2Rotate (Vector2 collisionDistance (-planeEnds) ) angle
       rightV = position |+| vector2Rotate (Vector2 collisionDistance   planeEnds  ) angle
       playerDir = vector2Rotate (Vector2 1.0 0.0) angle
       dist = (collisionDistance /) $ (/magnitude poi) distance
       angledRay = position |+| (vectorNormalize poi |* dist)

       ar1 = vector2'x angledRay
       l1  = vector2'x leftV
       r1  = vector2'x rightV

       t1 = (ar1-l1)/(r1-l1)
       
       heightR = (1/distance)*fromIntegral height*heightFactor
       x = t1*fromIntegral width
       xL1 = x - (heightR/2)
       xR1 = x + (heightR/2)

       (xL, xR) = if (xL1<0 && xR1<0) || (xL1>fromIntegral width && xR1>fromIntegral width)
         then (-1,-1)
         else (xL1, xR1)
