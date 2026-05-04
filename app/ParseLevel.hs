{-# LANGUAGE PatternSynonyms #-}
module ParseLevel (load, saveLevel) where

import Raylib.Core.Audio (loadSound)
import Raylib.Core.Textures (loadTexture, loadImage, loadRenderTexture)
import Raylib.Types (Vector2, pattern Vector2, renderTexture'texture, image'data, vector2'x, vector2'y)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS
import Data.Bits
import System.IO

import Constants

load :: String -> IO (State, UIState)
load filename = do
  levelHandle <- openFile filename ReadMode
  contents    <- hGetContents levelHandle

  let (playerData, levelData, wallPaths, floorPaths, spritePaths, audioPaths, nextLevel, maxKeys) = parseLevel contents 

  wallTextures   <- mapM loadTexture wallPaths
  floorTextures  <- mapM loadRawImage floorPaths
  floorCanvas    <- renderTexture'texture <$> loadRenderTexture width (div height 2)
  spriteTextures <- mapM loadTexture spritePaths

  audio <- mapM loadSound audioPaths

  let loadedTextures = (wallTextures, floorTextures, floorCanvas, spriteTextures, (wallPaths, floorPaths, spritePaths, audioPaths))

  canvas <- loadRenderTexture width height

  floorTexturesUnRaw <- mapM loadTexture floorPaths
  let uiState = (Vector2 0.0 0.0, 1.0, Selection {start = Vector2 0.0 0.0, cells=[(0,1), (2,1)]}, floorTexturesUnRaw)

  return ((levelData, playerData, loadedTextures, canvas, nextLevel, maxKeys, audio), uiState)

loadRawImage :: String -> IO FloorTex
loadRawImage path = do
  floorImg <- loadImage path
  let floorLis = image'data floorImg
  -- converting a image which is [Word8] into vector of word32 in abgr format
  let floorTex = VS.fromList
        $ map (foldl (\acc x -> x .|. shiftL acc 8) 0)
            $  foldl'
        (\acc x -> if length (head acc)>3 then
                     [x]:acc
                   else
                     (x:head acc):tail acc )
        [[]] $ map fromIntegral floorLis

  return floorTex

parseLevel :: String -> (Player, Scene, [String], [String], [String], [String], NextLevel, Keys)
parseLevel input = ((Vector2 ppx ppy, Vector2 pvx pvy, angle, 0, False), (walls, floors, stsp), wallPaths, floorPaths, spritePaths, audioPaths, nextLevel, maxKeys)
  where larr = lines input
        [playerData, paths, nextLevelData, wallData, floorData, spriteData] = splitOn "$" larr

        nextLevel = ((read $ head nextLevelData), (nextLevelData !! 1) )

        wall     = V.fromList wallsTemp
        wallsTemp :: [Int]
        wallsTemp   = map read $ concatMap words wallData
        wallsLevelA = map words wallData
        wallRows    = length wallsLevelA
        wallCols    = length $ head wallsLevelA

        floor'     = V.fromList floorsTemp
        floorsTemp :: [Int]
        floorsTemp   = map read $ concatMap words floorData
        floorsLevelA = map words floorData
        floorRows    = length floorsLevelA
        floorCols    = length $ head floorsLevelA

        [ppx,ppy,pvx,pvy,angle] = map read $ words $ unlines playerData

        walls  = (wallRows, wallCols, wall)
        floors = (floorRows, floorCols, floor')
        stsp   = map sphf spriteData

        maxKeys = length $ filter (\(x, _) -> x == 0) stsp

        [wallPaths, floorPaths, spritePaths, audioPaths] = splitOn "+" paths

sphf l = (floor id, Vector2 x y)
  where [id, x, y] = map read $ words l

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim = foldr (splitOnHelper delim) [[]]

splitOnHelper :: (Eq a) => a -> (a -> [[a]] -> [[a]])
splitOnHelper delim x acc = if x==delim then
                              []:acc
                            else
                              (x : head acc):tail acc 
saveLevel :: State -> FilePath -> IO ()
saveLevel state filePath = do
  let (scene, playerData, textures, _, nextLevel, _, _) = state
  let (walls, floors, staticSprites) = scene
  let (position, velocity, angle, _, _) = playerData
  let (_, _, _, _, (wallPaths, floorPaths, spritePaths, audioPaths)) = textures

  let wallText = getTextBlock walls
  let floorText = getTextBlock floors

  let level = unlines $
       [ (vec2ToText position),
         (vec2ToText velocity),
         show angle
       ] ++ ["$"] ++
       wallPaths ++ ["+"] ++
       floorPaths ++ ["+"] ++
       spritePaths ++ ["+"] ++
       audioPaths ++ ["$"] ++
       (getTextBlock walls) ++ ["$"] ++
       (getTextBlock floors) ++ ["$"] ++
       (map (\(x, v2)-> (show x ++ " " ++ (vec2ToText v2))) staticSprites)

  writeFile filePath level
       
  return ()
  

getTextBlock :: (Int, Int, V.Vector Int) -> [String]
getTextBlock (rows, cols, array) = map unwords $ chunks cols $ map show $ V.toList array

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

vec2ToText :: Vector2 -> String
vec2ToText vec = (show $ vector2'x vec) ++ " " ++ (show $ vector2'y vec)
