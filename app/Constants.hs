{-# LANGUAGE PatternSynonyms #-}
module Constants (width, height, cellSize, planeEnds, textureSize
                 ,collisionDistance, acceleration, heightFactor
                 ,deceleration, maxSpeed, Textures, State
                 ,AppState, Scene, Player) where

import Raylib.Util (WindowResources)
import Raylib.Types (Vector2, Texture)
import qualified Data.Vector.Unboxed as V

width :: Int 
width  = 1600
height :: Int
height = 1000

cellSize :: Int
cellSize = 20

fov :: Float
fov = pi/1.5
planeEnds = collisionDistance * (tan (fov/2))
heightFactor :: Float
heightFactor = (1/(tan (fov/2)))*(fromIntegral width)/(fromIntegral height)

textureSize :: Float
textureSize = 256

collisionDistance :: Float
collisionDistance = 0.2

acceleration :: Float
acceleration = 0.2 -- in blocks per sec

deceleration :: Float
deceleration = 0.02 -- in blocks per sec

maxSpeed :: Float
maxSpeed = 5.0 -- in blocks per sec

type Textures = [Texture]
  --          position speed angle
type Player = (Vector2, Vector2, Float)
           -- level  player  textures
type State = (Scene, Player, Textures)
type AppState = (State, WindowResources)
type Scene = (Int, Int, V.Vector Int)
