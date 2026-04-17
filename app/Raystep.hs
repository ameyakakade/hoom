{-# LANGUAGE PatternSynonyms #-}
module Raystep (rayStep, getWallID, getWallIDVec2) where

import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)

import Constants 

rayStep = rayStepA 0

rayStepA :: Int -> Scene -> Vector2 -> Vector2 -> (Vector2, Int)
rayStepA count scene a b 
    | count > 50 = (b, 5)
    | (vector2'x b) > fromIntegral(width)  = (b, 0)
    | (vector2'y b) > fromIntegral(height) = (b, 0)
    | (vector2'x b) < 0.0 = (0.0, 0)
    | (vector2'y b) < 0.0 = (0.0, 0)
    | wallID /= 0 = (b, wallID)
    | dx /= 0 = let k = dy/dx
                    c = (vector2'y a) - k*(vector2'x a)
                    x = snap (vector2'x b) dx
                    y = snap (vector2'y b) dy
                    nextRay1 = (Vector2 x (k*x + c))
                    nextRay2 = if k/=0 then (Vector2 ((y-c)/k) y) else (Vector2 0.0 0.0)
                    closest = if (vectorDistance nextRay1 b) > (vectorDistance nextRay2 b) then nextRay2 else nextRay1
                    in rayStepA (count+1) scene b closest
    | otherwise = ((Vector2 0.0 0.0), 0)
    where dv = b |-| a
          dx = vector2'x dv
          dy = vector2'y dv
          wallID = hittingWall scene a b


snap :: (RealFrac b, Num a, Ord a) => b -> a -> b
snap a da
    | da > 0 = fromIntegral $ ceiling (a + 0.001) 
    | da < 0 = fromIntegral $ floor (a - 0.001)
    | otherwise = a

hittingWall :: Scene -> Vector2 -> Vector2 -> Int
hittingWall scene a b = getWallID scene cx cy
    where dv = b |-| a
          dx = vector2'x dv
          dy = vector2'y dv
          bx = vector2'x b
          by = vector2'y b
          cx = fromIntegral $ abs $ floor $ bx + (signum dx)*0.001
          cy = fromIntegral $ abs $ floor $ by + (signum dy)*0.001

getWallID :: Scene -> Int -> Int -> Int
getWallID (cols, rows, scene) x y 
  | x < 0 || y < 0 = 0
  | x >= cols || y >= rows = 0
  | otherwise = (scene !! x) !! y 

getWallIDVec2 :: Scene -> Vector2 -> Int
getWallIDVec2 scene position = getWallID scene x y
  where y = floor $ vector2'y position
        x = floor $ vector2'x position
