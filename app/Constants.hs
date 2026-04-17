{-# LANGUAGE PatternSynonyms #-}
module Constants (width, height, cellSize, planeEnds, textureSize, playerPosition
                 ,initSpeed, playerAngle, collisionDistance, Textures, State
                 ,AppState, Scene) where

import Raylib.Util (WindowResources)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, Texture)
width :: Int 
width  = 1200
height :: Int
height = 900

cellSize :: Int
cellSize = 20

fov :: Float
fov = pi/2
planeEnds = collisionDistance * (tan (fov/2))

textureSize :: Float
textureSize = 256

playerPosition = (Vector2 2 2)
initSpeed = (Vector2 0.0 0.0)
playerAngle :: Float
playerAngle = 0.0

collisionDistance :: Float
collisionDistance = 0.2

type Textures = [Texture]
type State = (Vector2, Vector2, Float, Textures)
type AppState = (State, WindowResources)
type Scene = (Int, Int, [[Int]])
