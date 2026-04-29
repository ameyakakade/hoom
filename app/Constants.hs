module Constants (width, height, cellSize, planeEnds, textureSize, sWidth, sHeight
                 ,collisionDistance, acceleration, heightFactor
                 ,deceleration, maxSpeed, fov, Textures, State, Canvas, FloorCanvas
                 ,Walls, Floors, StaticSprites, WallTextures, FloorTextures, SpriteTextures
                 ,AppState, Scene, Player, FloorTex) where

import Raylib.Util (WindowResources)
import Raylib.Types (Vector2, Texture, RenderTexture2D)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS
import qualified Data.Word as W

width :: Int 
width  = div sWidth 2
height :: Int
height = div sHeight 2

sWidth :: Int
sWidth = 1600
sHeight :: Int
sHeight = 900

cellSize :: Int
cellSize = 25

fov :: Float
fov = pi/2.0

planeEnds :: Float
planeEnds = collisionDistance * tan (fov/2)
heightFactor :: Float
heightFactor = 1

textureSize :: Float
textureSize = 64

collisionDistance :: Float
collisionDistance = 0.05

acceleration :: Float
acceleration = 0.2 -- in blocks per sec

deceleration :: Float
deceleration = 0.02 -- in blocks per sec

maxSpeed :: Float
maxSpeed = 5.0 -- in blocks per sec

type FloorTex       = VS.Vector W.Word32
type WallTextures   = [Texture]
type SpriteTextures = [Texture]
type FloorTextures  = [FloorTex]
type FloorCanvas    = Texture
type Textures       = (WallTextures, FloorTextures, FloorCanvas, SpriteTextures)
  
type Canvas = RenderTexture2D

           -- position speed angle
type Player = (Vector2, Vector2, Float)

           -- level  player  textures
type Walls         = (Int, Int, V.Vector Int)
type Floors        = (Int, Int, V.Vector Int)
type StaticSprites = [Vector2]
type Scene         = (Walls, Floors, StaticSprites)

type State    = (Scene, Player, Textures, Canvas)
type AppState = (Int, State, WindowResources)
