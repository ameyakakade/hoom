{-# LANGUAGE PatternSynonyms #-}
module ParseLevel (load) where

import Raylib.Core.Textures (loadTexture, loadImage, loadRenderTexture)
import Raylib.Types (pattern Vector2, renderTexture'texture, image'data)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as VS
import Data.Bits
import System.IO

import Constants

load :: String -> IO State
load filename = do
  levelHandle <- openFile filename ReadMode
  contents    <- hGetContents levelHandle

  let (playerData, levelData, wallPaths, floorPaths, spritePaths) = parseLevel contents 

  wallTextures   <- mapM loadTexture wallPaths
  floorTextures  <- mapM loadRawImage floorPaths
  floorCanvas    <- renderTexture'texture <$> loadRenderTexture width (div height 2)
  spriteTextures <- mapM loadTexture spritePaths

  let loadedTextures = (wallTextures, floorTextures, floorCanvas, spriteTextures)


  canvas <- loadRenderTexture width height

  return (levelData, playerData, loadedTextures, canvas)

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


parseLevel :: String -> (Player, Scene, [String], [String], [String])
parseLevel input = ((Vector2 ppx ppy, Vector2 pvx pvy, angle), (walls, floors, stsp), wallPaths, floorPaths, spritePaths)
  where larr = lines input
        [playerData, paths, wallData, floorData, spriteData] = splitOn "$" larr

        wall     = V.fromList wallsTemp
        wallsTemp :: [Int]
        wallsTemp   = map read $ concatMap words wallData
        wallsLevelA = map words wallData
        wallCols    = length wallsLevelA
        wallRows    = length $ head wallsLevelA

        floor'     = V.fromList floorsTemp
        floorsTemp :: [Int]
        floorsTemp   = map read $ concatMap words floorData
        floorsLevelA = map words floorData
        floorCols    = length floorsLevelA
        floorRows    = length $ head floorsLevelA

        [ppx,ppy,pvx,pvy,angle] = map read $ words $ unlines playerData

        walls  = (wallCols, wallRows, wall)
        floors = (floorCols, floorRows, floor')
        stsp   = map sphf spriteData

        [wallPaths, floorPaths, spritePaths] = splitOn "+" paths

sphf l = (floor id, Vector2 x y)
  where [id, x, y] = map read $ words l

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim = foldr (splitOnHelper delim) [[]]

splitOnHelper :: (Eq a) => a -> (a -> [[a]] -> [[a]])
splitOnHelper delim x acc = if x==delim then
                              []:acc
                            else
                              (x : head acc):tail acc 
