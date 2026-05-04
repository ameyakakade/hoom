{-# LANGUAGE PatternSynonyms #-}

module LevelEditor (editorView) where

import Raylib.Core (isKeyPressed, clearBackground, getMouseDelta, getMouseWheelMove, isKeyDown, isMouseButtonDown, disableCursor, isKeyReleased, isMouseButtonReleased, getMousePosition, isMouseButtonPressed)
import Raylib.Util (drawing, raylibApplication, WindowResources)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (drawTexturePro)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, KeyboardKey(..), Rectangle, pattern Rectangle, MouseButton(..), Color(..), Texture)
import Raylib.Util.Colors (black, red, white, lightGray)
import Raylib.Core.Shapes (drawRectangleRec, drawLine, drawLineV, drawCircle, drawRectangleV)
import Raylib.Util.Math(Vector(..), vectorNormalize, vector2Rotate, clamp)

import System.IO
import System.Environment
import Data.List
import qualified Data.Vector.Unboxed as V

import Constants 
import ParseLevel

editorView :: Int -> State -> UIState -> WindowResources -> IO AppState
editorView view state uiState window = drawing
  ( do
      let (scene, player, textures, canvas, nextLevel, keys) = state
      let (walls, floors, sprites) = scene

      let (offset, scale, selection, floorTex) = uiState
      let (wallRows, wallCols, wall) = walls
      let (floorRows, floorCols, floor') = floors
      let (wallTex, _, floorCanvas, spriteTex, _) = textures
        
      clearBackground black

      drawCells True offset scale (fromIntegral floorRows) (fromIntegral floorCols) floor' floorTex 0
      drawCells False offset scale (fromIntegral wallRows) (fromIntegral wallCols) wall wallTex 0
      drawGrid offset scale (fromIntegral wallRows) (fromIntegral wallCols)
      drawSprites offset scale sprites spriteTex
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

      is1 <- isKeyDown KeyOne
      is2 <- isKeyDown KeyTwo
      is3 <- isKeyDown KeyThree
      is4 <- isKeyDown KeyFour
      is5 <- isKeyDown KeyFive
      is6 <- isKeyDown KeySix
      is7 <- isKeyDown KeySeven
      is8 <- isKeyDown KeyEight
      is9 <- isKeyDown KeyNine
      is0 <- isKeyDown KeyZero

      let selectedId = if is1 then 1
                       else if is2 then 2
                            else if is3 then 3
                                 else if is4 then 4
                                      else if is5 then 5
                                           else if is6 then 6
                                                else if is7 then 7
                                                     else if is8 then 8
                                                          else if is9 then 9 else 0

      isADown <- isKeyPressed KeyA
      let newWalls = if isADown then replaceCells walls selectedId selection else walls

      isZDown <- isKeyPressed KeyZ
      let newFloors = if isZDown then replaceCells floors selectedId selection else floors

      isSDown <- isKeyPressed KeyS

      args <- getArgs
      if isSDown then saveLevel state (head args) else return ()

      let newScene = (newWalls, newFloors, sprites)
      let newState = (newScene, player, textures, canvas, nextLevel, keys)
      let newUiState = (newOffset, newScale, newSelection, floorTex)

      return (newView, newState, newUiState, window)
  )

blockSize = 40
padding = 1

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

drawCells :: Bool -> Vector2 -> Float -> Float -> Float -> V.Vector Int -> [Texture] -> Int -> IO ()
drawCells floorFlag offset scale rows cols scene textures index
  | scene == V.empty = return ()
  | otherwise = do
      let id = V.head scene
      let textureRect = Rectangle 0 0 textureSize textureSize
      let x = fromIntegral $ mod index (floor cols)
      let y = fromIntegral $ floor $ (fromIntegral index / cols)
      let offX = vector2'x offset
      let offY = vector2'y offset
      let drawingRect = Rectangle (offX + blockSize*x*scale + padding*x + padding) (offY + blockSize*y*scale + padding*y + padding) (blockSize*scale) (blockSize*scale)
      if (floorFlag || id /= 0) then
        drawTexturePro (textures!!id) textureRect drawingRect (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
        else return ()
      drawCells floorFlag offset scale rows cols (V.tail scene) textures (index+1)

drawSprites :: Vector2 -> Float -> StaticSprites -> SpriteTextures -> IO ()
drawSprites offset scale sprites textures = mapM_ fn sprites
  where fn (id, v2) = drawTexturePro
                      (textures!!id)
                      (Rectangle 0 0 textureSize textureSize)
                      (Rectangle (offX + (vector2'x v2)*(blockSize*scale + padding))
                       (offY + (vector2'y v2)*(blockSize*scale + padding))
                        (blockSize*scale) (blockSize*scale))
                      (Vector2 0.0 0.0) 0.0 (Color 255 255 255 255)
        offX = vector2'x offset
        offY = vector2'y offset

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


