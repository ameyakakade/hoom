{-# LANGUAGE PatternSynonyms #-}

module LevelEditor (editorView) where

import Raylib.Core (isKeyPressed, clearBackground, getMouseDelta, getMouseWheelMove, isKeyDown, isMouseButtonDown, disableCursor, isKeyReleased, isMouseButtonReleased, getMousePosition, isMouseButtonPressed)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (drawTexturePro)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, KeyboardKey(..), Rectangle, pattern Rectangle, MouseButton(..), Color(..))
import Raylib.Util.Colors (black, red, white, lightGray)
import Raylib.Core.Shapes (drawRectangleRec, drawLine, drawLineV, drawCircle, drawRectangleV)
import Raylib.Util.Math(Vector(..), vectorNormalize, vector2Rotate, clamp)

import Data.List
import qualified Data.Vector.Unboxed as V

import Constants 

editorView :: Int -> State -> UIState -> WindowResources -> IO AppState
editorView view state uiState window = drawing
  ( do
      let (scene, player, textures, canvas) = state
      let (walls, floors, sprites) = scene

      let (offset, scale, selection) = uiState
      let (wallRows, wallCols, wall) = walls
      let (wallTex, floorTex, floorCanvas, spriteTex) = textures
        
      clearBackground black

      drawWalls offset scale (fromIntegral wallRows) (fromIntegral wallCols) wall wallTex 0
      drawGrid offset scale (fromIntegral wallRows) (fromIntegral wallCols)
      drawSelection selection scale offset (fromIntegral wallRows) (fromIntegral wallCols)

      isRDown <- isMouseButtonDown MouseButtonRight
      newOffset <- if isRDown then (offset |+|) <$> getMouseDelta else return offset

      newScale <- (\x -> (x*0.03) + scale) <$> getMouseWheelMove

      isCDown <- isKeyPressed KeyC
      isMPressed <- isMouseButtonPressed MouseButtonLeft
      isMDown <- isMouseButtonDown MouseButtonLeft
      isMReleased <- isMouseButtonReleased MouseButtonLeft
      mPos <- getMousePosition

      let oldPos = start selection
      if isMDown then drawRectangleV oldPos (mPos - oldPos) (Color 100 100 100 200) else return ()
      let newSelection = updateSelection scale selection isCDown isMPressed isMReleased mPos newOffset

      isPDown <- isKeyPressed KeyP
      if isPDown then disableCursor else return ()
      let newView = if isPDown then 2 else view

      isADown <- isKeyPressed KeyA
      let newWalls = if isADown then replaceCells walls 3 selection else walls

      let newScene = (newWalls, floors, sprites)
      let newState = (newScene, player, textures, canvas)
      let newUiState = (newOffset, newScale, newSelection)

      return (newView, newState, newUiState, window)
  )

blockSize = 20
padding = 2

drawGrid :: Vector2 -> Float -> Float -> Float -> IO ()
drawGrid offset scale rows cols = do
  let rowWidth = scale*rows*blockSize + rows*padding + padding
  let colWidth = scale*cols*blockSize + cols*padding + padding
  let xOff = vector2'x offset
  let yOff = vector2'y offset
  drawCols scale xOff yOff rowWidth cols
  drawRows scale xOff yOff rows colWidth

drawCols :: Float -> Float -> Float -> Float -> Float -> IO ()
drawCols scale xOff yOff rowWidth cols
  | cols >= 0 = do
      drawRectangleRec drawingRect white
      drawCols scale xOff yOff rowWidth (cols - 1)
  | otherwise = return ()
  where drawingRect = Rectangle (xOff + (blockSize*cols*scale + padding*cols)) yOff padding rowWidth

drawRows :: Float -> Float -> Float -> Float -> Float -> IO ()
drawRows scale xOff yOff rows colWidth
  | rows >= 0 = do
      drawRectangleRec drawingRect white
      drawRows scale xOff yOff (rows-1) colWidth
  | otherwise = return ()
  where drawingRect = Rectangle xOff (yOff + (blockSize*rows*scale + padding*rows)) colWidth padding

updateSelection :: Float -> Selection -> Bool -> Bool -> Bool -> Vector2 -> Vector2 -> Selection
updateSelection scale oldSel clearSel pressed released mousePos offset
  | clearSel  = None
  | pressed   = Selection {start = mousePos, cells = cells oldSel}
  | released  = Selection {start = Vector2 0.0 0.0 , cells = updateCells scale ((start oldSel) - offset) (mousePos - offset) (cells oldSel) }
  | otherwise = oldSel

updateCells :: Float -> Vector2 -> Vector2 -> [(Int, Int)] -> [(Int, Int)]
updateCells scale start end oldCells = nub $ newCells ++ oldCells
  where
    newCells = [(x,y) | x <- [sx..ex], y <- [sy..ey] ]
    sx = floor $ vector2'x start / (blockSize*scale + padding)
    sy = floor $ vector2'y start / (blockSize*scale + padding)
    ex = floor $ vector2'x end / (blockSize*scale + padding)
    ey = floor $ vector2'y end / (blockSize*scale + padding)

drawSelection :: Selection -> Float -> Vector2 -> Float -> Float -> IO ()
drawSelection selection scale offset rows cols
  | selection == None = return ()
  | otherwise = do
  let cell = cells selection
  let xOff = vector2'x offset
  let yOff = vector2'y offset
  let fn (x, y) = drawRectangleRec (Rectangle (xOff + fromIntegral x*blockSize*scale + fromIntegral x*padding) (yOff + fromIntegral y*blockSize*scale + fromIntegral y*padding) (blockSize*scale + padding*2) (blockSize*scale + padding*2) ) (Color 200 200 200 100)
  mapM_ fn cell
  | otherwise = return ()

drawWalls :: Vector2 -> Float -> Float -> Float -> V.Vector Int -> WallTextures -> Int -> IO ()
drawWalls offset scale rows cols scene textures index
  | scene == V.empty = return ()
  | otherwise = do
      let id = V.head scene
      let textureRect = Rectangle 0 0 textureSize textureSize
      let x = fromIntegral $ mod index (floor cols)
      let y = fromIntegral $ floor $ (fromIntegral index / cols)
      let offX = vector2'x offset
      let offY = vector2'y offset
      let drawingRect = Rectangle (offX + blockSize*x*scale + padding*x + padding) (offY + blockSize*y*scale + padding*y + padding) (blockSize*scale) (blockSize*scale)
      if id /= 0 then
        drawTexturePro (textures!!id) textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
        else return ()
      drawWalls offset scale rows cols (V.tail scene) textures (index+1)

replaceCells :: (Int, Int, V.Vector Int) -> Int -> Selection -> (Int, Int, V.Vector Int)
replaceCells (rows, cols, scene) id selection = (rows, cols, replaceByIndex scene 0 newCells id)
  where newCells = sort $ map (\(x, y) -> x + cols*y) $ cells selection

replaceByIndex :: V.Vector Int -> Int -> [Int] -> Int -> V.Vector Int
replaceByIndex list index indexList value
  | null indexList = list
  | list == V.empty = V.empty
  | otherwise = if index == head indexList
                then value `V.cons` replaceByIndex (V.tail list) (index+1) (tail indexList) value
                else (V.head list) `V.cons` replaceByIndex (V.tail list) (index+1) (indexList) value


