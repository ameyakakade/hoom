{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, initWindow, setTargetFPS
                   ,getFPS, windowShouldClose, closeWindow
                   ,getMouseDelta, getMousePosition, isKeyDown
                   ,disableCursor, getFrameTime)
import Raylib.Core.Text (drawText)
import Raylib.Core.Shapes (drawRectangle, drawLine, drawLineV, drawCircle)
import Raylib.Core.Textures (loadTexture, drawTexturePro)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Util.Math(Vector(..), vectorNormalize, vectorDistance, vector2Rotate)
import Raylib.Util.Colors (lightGray, rayWhite, red, black, blue)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y
                    ,KeyboardKey(KeyM), KeyboardKey(KeyW), KeyboardKey(KeyA)
                    ,KeyboardKey(KeyS), KeyboardKey(KeyD), Color (Color)
                    ,Texture, Rectangle, pattern Rectangle)

type Textures = [Texture]
type State = (Vector2, Vector2, Float, Textures)
type AppState = (State, WindowResources)

width :: Int 
width  = 1440
height :: Int
height = 1080

cellSize :: Int
cellSize = 30


fov :: Float
fov = 0.8

textureSize :: Float
textureSize = 256

scene :: [[Int]]
scene = [ [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,2,0,0,3,0,0,4,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,2,0,3,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,2,0,3,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
          [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] ]

cols :: Int
cols = length scene
rows :: Int
rows = length $ head scene

playerPosition = (Vector2 2.0 2.0)
playerAngle = 0

startup :: IO AppState 
startup = do 
  window <- initWindow width height "hoom"
  let texturePaths = ["dummy.png", "wall1.png", "wall2.png", "wall3.png", "wall4.png", "error.png"]
  loadedTextures <- sequence $ map loadTexture texturePaths
  disableCursor
  return ( (playerPosition, Vector2 1 1, playerAngle, loadedTextures) , window)

acceleration = 0.04 -- in blocks per sec
deceleration = 0.006 -- in blocks per sec
maxSpeed = 4.0 -- in blocks per sec

boolToNum :: (Num a) => Bool -> a
boolToNum b = if b then 1 else 0

mainLoop :: AppState -> IO AppState
mainLoop (state, window) =
  drawing
    ( do
        let (positionOld, velocityOld, angleOld, textures) = state
        isMDown <- isKeyDown KeyM

        xOffset1 <- fmap boolToNum (isKeyDown KeyW)
        yOffset1 <- fmap ((*(-1)).boolToNum) (isKeyDown KeyA)
        xOffset2 <- fmap ((*(-1)).boolToNum) (isKeyDown KeyS)
        yOffset2 <- fmap boolToNum (isKeyDown KeyD)

        time <- getFrameTime
      
        mouse <- fmap vector2'x getMouseDelta 
        let angle = angleOld + mouse*0.003

        let velocityCollision = getVelocityCollision positionOld

        -- add acceleration and deceleration
        let velocityDir = vector2Rotate (Vector2 (xOffset1 + xOffset2) (yOffset1 + yOffset2) ) (angle+pi/4)
        let velocityDelta = ((vectorNormalize velocityDir) |* acceleration) |-| (velocityOld |* deceleration )
        let velocityA = velocityOld + velocityDelta + velocityCollision
        let mag = magnitude velocityA
        let velocity | mag > maxSpeed = ((vectorNormalize velocityA) |* maxSpeed)
                     | otherwise = velocityA

        let position = positionOld |+| (velocity |* time)

     -- setTargetFPS 60
        if isMDown then drawMap position else drawScene position angle textures

        fps <- getFPS
        drawText ("FPS: " ++ (show fps)) 30 40 30 red

        return ( (position, velocity, angle, textures), window)
    )

drawScene :: Vector2 -> Float -> Textures -> IO ()
drawScene position angle textures = do
        clearBackground (Color 40 37 30 255)
        mousePos <- getMousePosition
        let mousePosN = (vector2'x mousePos)/(fromIntegral width)
        let playerFront = position |+| (vector2Rotate (Vector2 0.001 0.001) angle)
        drawBars position playerFront 0 textures
        return ()

drawBars :: Vector2 -> Vector2 -> Int -> Textures -> IO ()
drawBars origin ray screenX textures
  | screenX <= width = do
                 -- drawRectangle screenX (floor $ ((fromIntegral height) - (heightR))/2 ) deltaRes (floor heightR) color
                 -- strokeLine origin wall
                    drawTexturePro (textures !! (wallID)) (Rectangle (interp*textureSize) 0 1 textureSize) rect (Vector2 0.0 0.0) 0.0 color
                    drawBars origin ray (screenX + deltaRes) textures 
  | otherwise = return ()
  where heightR = 1.5*(1/distance)*(fromIntegral height)
        color 
          | wallID == 5 = Color 255 0 0 255
          | otherwise = Color 255 255 255 (c)
        c = floor $ min ( ((10/distance)^2)*255 ) 255 
        rect = Rectangle (fromIntegral screenX) ( ((fromIntegral height) - (heightR))/2 ) (fromIntegral deltaRes) heightR
        distance = (cos angle) * (vectorDistance origin wall)
        interp = max x y
        x = snd . properFraction $ vector2'x wall
        y = snd . properFraction $ vector2'y wall
        (wall, wallID) = rayStep origin angledRay
        angledRay = origin |+| vector2Rotate (ray|-|origin) angle
        angle = (fov)*((fromIntegral screenX)/(fromIntegral width) - 0.5)
        deltaRes = 1
  

drawMap :: Vector2 -> IO ()
drawMap position = do
        clearBackground black
        drawGrid 
        drawCells scene 0
        mousePos <- getMousePosition
        let mousePosN = mousePos |/ fromIntegral cellSize
        let (wall, wallID) = rayStep position mousePosN
        strokeLine position wall 
        drawCircleOnGrid mousePosN
        drawCircleOnGrid position
        drawCircleOnGrid wall
        drawGrid :: IO ()
drawGrid = do
            drawRows 0
            drawCols 0

drawCells :: [[Int]] -> Int -> IO ()
drawCells [] _ = return ()
drawCells list no = do
                drawCellsHelper (head list) no 0
                drawCells (tail list) (no+1)

drawCellsHelper :: [Int] -> Int -> Int -> IO ()
drawCellsHelper [] _ _= return ()
drawCellsHelper list x y = do
                        drawCellsHelper (tail list) x (y+1)
                        let wallID = head list in 
                            if wallID/=0 then drawRectangle (x*cellSize) (y*cellSize) cellSize cellSize lightGray else return ()

drawRows :: Int -> IO ()
drawRows h 
    | h < height = (do 
                drawLine 0 h width h lightGray
                drawRows ( h + (cellSize) )
              )
    | otherwise = return ()

drawCols :: Int -> IO ()
drawCols w 
    | w < width = (do 
                drawLine w 0 w height lightGray
                drawCols ( w + (cellSize) )
              ) | otherwise = return ()

drawCircleOnGrid :: Vector2 -> IO ()
drawCircleOnGrid pos = drawCircle x y r red
    where x = truncate $ (vector2'x pos)*s
          y = truncate $ (vector2'y pos)*s
          r = 10
          s = fromIntegral(cellSize)

strokeLine :: Vector2 -> Vector2 -> IO ()
strokeLine posS posE = drawLineV (posS|*s) (posE|*s) red
    where s = fromIntegral(cellSize)

rayStep = rayStepA 0

rayStepA :: Int -> Vector2 -> Vector2 -> (Vector2, Int)
rayStepA count a b 
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
                    in rayStepA (count+1) b closest
    | otherwise = ((Vector2 0.0 0.0), 0)
    where dv = b |-| a
          dx = vector2'x dv
          dy = vector2'y dv
          wallID = hittingWall a b


snap :: (RealFrac b, Num a, Ord a) => b -> a -> b
snap a da
    | da > 0 = fromIntegral $ ceiling (a + 0.001) 
    | da < 0 = fromIntegral $ floor (a - 0.001)
    | otherwise = a

hittingWall :: Vector2 -> Vector2 -> Int
hittingWall a b = getWallID cx cy
    where dv = b |-| a
          dx = vector2'x dv
          dy = vector2'y dv
          bx = vector2'x b
          by = vector2'y b
          cx = fromIntegral $ abs $ floor $ bx + (signum dx)*0.001
          cy = fromIntegral $ abs $ floor $ by + (signum dy)*0.001

getWallID :: Int -> Int -> Int
getWallID y x 
    | y >= cols || x >= rows = 0
    | otherwise = (scene !! y) !! x 


shouldClose :: AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: AppState -> IO ()
teardown (_, win)= closeWindow . Just $ win

getVelocityCollision :: Vector2 -> Vector2
getVelocityCollision position = Vector2 0.00 0.00

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)  
