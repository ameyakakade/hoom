{-# LANGUAGE PatternSynonyms #-}
module Constants (width, height, cellSize, planeEnds, textureSize, sWidth, sHeight
                 ,collisionDistance, acceleration, heightFactor, fi
                 ,deceleration, maxSpeed, fov, Textures, State, Canvas
                 ,AppState, Scene, Player, FloorTex) where

import Raylib.Util (WindowResources)
import Raylib.Types (Vector2, Texture, RenderTexture2D)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS
import qualified Data.Word as W

width :: Int 
width  = div sWidth 1
height :: Int
height = div sHeight 1

sWidth :: Int
sWidth = 1600
sHeight :: Int
sHeight = 900

cellSize :: Int
cellSize = 20

fov :: Float
fov = pi/3
planeEnds = collisionDistance * (tan (fov/2))
heightFactor :: Float
heightFactor = (fromIntegral width)/(fromIntegral height)

textureSize :: Float
textureSize = 256

collisionDistance :: Float
collisionDistance = 0.1

acceleration :: Float
acceleration = 0.2 -- in blocks per sec

deceleration :: Float
deceleration = 0.02 -- in blocks per sec

maxSpeed :: Float
maxSpeed = 5.0 -- in blocks per sec

type Textures = [Texture]
type Canvas = RenderTexture2D
type FloorTex = VS.Vector W.Word32
  --          position speed angle
type Player = (Vector2, Vector2, Float)
           -- level  player  textures
type State = (Scene, Player, Textures, FloorTex, Canvas)
type Scene = (Int, Int, V.Vector Int)
type AppState = (State, WindowResources)

fi = fromIntegral
