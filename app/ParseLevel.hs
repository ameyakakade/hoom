{-# LANGUAGE PatternSynonyms #-}
module ParseLevel (parseLevel) where

import Constants
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)
import qualified Data.Vector.Unboxed as V

parseLevel :: String -> (Player, Scene)
parseLevel input = ((Vector2 ppx ppy, Vector2 pvx pvy, angle), (walls, floors, stsp))
  where larr = lines input
        (playerData, levelData) = splitAt 3 larr

        level     = V.fromList leveltemp
        leveltemp :: [Int]
        leveltemp = map read $ concat $ map words levelData
        levelA    = map words levelData
        cols      = length levelA
        rows      = length $ head levelA

        [ppx,ppy,pvx,pvy,angle] = map read $ words $ unlines playerData

        walls  = (cols, rows, level)
        floors = (cols, rows, level)
        stsp   = [ (Vector2 2.0 3.0),
                   (Vector2 10.0 30.0),
                   (Vector2 8.3 15.0),
                   (Vector2 12.3 30.0),
                   (Vector2 13.3 3.0),
                   (Vector2 20.3 31.0),
                   (Vector2 15.0 2.0),
                   (Vector2 5.0 30.0),
                   (Vector2 20.0 20.0),
                   (Vector2 9.3 9.0)]
