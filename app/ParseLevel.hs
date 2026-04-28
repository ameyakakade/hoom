{-# LANGUAGE PatternSynonyms #-}
module ParseLevel (load) where

import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)
import Raylib.Core.Textures (loadTexture, loadImage, loadRenderTexture)
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y, renderTexture'texture, image'data)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS
import Data.Bits
import System.IO

import Constants

load filename = do
  levelHandle <- openFile filename ReadMode
  contents <- hGetContents levelHandle

  let (playerData, levelData, wallPaths, floorPaths, spritePaths) = parseLevel contents 

  floorCanvas <- loadRenderTexture width (div height 2)
  loadedTexturesA <- sequence $ map loadTexture wallPaths
  let loadedTextures = (renderTexture'texture floorCanvas):loadedTexturesA
  

  floorImg <- loadImage "textures/Ground.png"
  let floorLis = image'data floorImg
  -- converting a image which is [Word8] into vector of word32 in abgr format
  let (floorTex) = VS.fromList $ map (foldl (\acc x -> x .|. (shiftL acc 8)) 0) $ ( foldl' (\acc x -> if ( (length $ head acc)>3) then [x]:acc else ( x:(head acc)):(tail acc)) [[]] $ map fromIntegral floorLis)


  canvas <- loadRenderTexture width height

  return (levelData, playerData, loadedTextures, floorTex, canvas)


parseLevel :: String -> (Player, Scene, [String], [String], [String])
parseLevel input = ((Vector2 ppx ppy, Vector2 pvx pvy, angle), (walls, floors, stsp), wallPaths, floorPaths, spritePaths)
  where larr = lines input
        [playerData, paths, wallData, floorData, spriteData] = splitOn "$" larr

        wall     = V.fromList wallsTemp
        wallsTemp :: [Int]
        wallsTemp = map read $ concat $ map words wallData
        wallsLevelA    = map words wallData
        wallCols      = length wallsLevelA
        wallRows      = length $ head wallsLevelA

        floor     = V.fromList floorsTemp
        floorsTemp :: [Int]
        floorsTemp = map read $ concat $ map words floorData
        floorsLevelA    = map words floorData
        floorCols      = length floorsLevelA
        floorRows      = length $ head floorsLevelA

        [ppx,ppy,pvx,pvy,angle] = map read $ words $ unlines playerData

        walls  = (wallCols, wallRows, wall)
        floors = (floorCols, floorRows, wall)
        stsp   = map sphf spriteData

        [wallPaths, floorPaths, spritePaths] = splitOn "+" paths

sphf l = Vector2 x y
  where [id, x, y] = map read $ words l

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim list = foldr (splitOnHelper delim) [[]] list

splitOnHelper delim = \x acc -> if (x==delim) then []:acc else (x : head acc):(tail acc) 
