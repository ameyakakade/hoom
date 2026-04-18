{-# LANGUAGE PatternSynonyms #-}
module Render (drawScene) where

import Raylib.Core (clearBackground)
import Raylib.Core.Textures (loadTexture, drawTexturePro)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,Color (Color) ,Texture, Rectangle, pattern Rectangle)

import Constants
import Raystep

drawScene :: Scene -> Vector2 -> Float -> Textures -> IO ()
drawScene scene position angle textures = do
        clearBackground (Color 255 255 255 255)
        drawBars scene position angle 0 textures
        return ()

drawBars :: Scene -> Vector2 -> Float -> Int -> Textures -> IO ()
drawBars scene origin angle screenX textures
  | screenX <= width = do
                 -- drawRectangle screenX (floor $ ((fromIntegral height) - (heightR))/2 ) deltaRes (floor heightR) color
                 -- strokeLine origin wall
                    drawTexturePro (textures !! (wallID)) (Rectangle (interp*textureSize) 0 1 textureSize) rect (Vector2 0.0 0.0) 0.0 color
                    drawBars scene origin angle (screenX + deltaRes) textures 
  | otherwise = return ()
  where heightR = (1/distance)*(fromIntegral height)*heightFactor
        color 
          | wallID == 5 = Color 255 0 0 255
          | otherwise = Color 255 255 255 (c)
        c = floor $ min ( ((10/distance)^2)*255 ) 255 
        rect = Rectangle (fromIntegral screenX) ( ((fromIntegral height) - (heightR))/2 ) (fromIntegral deltaRes) heightR
        distance = (wall |-| origin) |.| vector2Rotate (Vector2 1.0 0.0) angle
        interp = max x y
        x = snd . properFraction $ vector2'x wall
        y = snd . properFraction $ vector2'y wall
        (wall, wallID) = rayStep scene origin angledRay
        angledRay = origin |+| vector2Rotate (Vector2 collisionDistance (xinterp*planeEnds)) angle
        xinterp = (fromIntegral screenX)/(fromIntegral width) - 0.5
        deltaRes = 1
  
