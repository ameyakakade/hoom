{-# LANGUAGE PatternSynonyms #-}
module Movement (checkCollision) where 

import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)

import Constants
import Raystep(getWallIDVec2)

checkCollision :: Scene -> Vector2 -> Vector2 -> Vector2
checkCollision scene velocity position = 
  Vector2 cvx cvy
  where cvx
          | right /= 0 = if velx>0 then (-velx) else 0
          | left  /= 0 = if velx<0 then (-velx) else 0
          | otherwise = 0
        cvy 
          | front /= 0 = if vely>0 then (-vely) else 0 
          | back  /= 0 = if vely<0 then (-vely) else 0
          | otherwise = 0
        front = getWallIDVec2 scene $ (position |+| (Vector2 0   collisionDistance))
        back  = getWallIDVec2 scene $ (position |+| (Vector2 0 (-collisionDistance)))
        right = getWallIDVec2 scene $ (position |+| (Vector2   collisionDistance 0))
        left  = getWallIDVec2 scene $ (position |+| (Vector2 (-collisionDistance) 0))
        velx = vector2'x velocity
        vely = vector2'y velocity
