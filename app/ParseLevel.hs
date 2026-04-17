{-# LANGUAGE PatternSynonyms #-}
module ParseLevel (parseLevel) where

import Constants
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)
import qualified Data.Vector.Unboxed as V

parseLevel :: String -> (Player, Scene)
parseLevel input = ((Vector2 ppx ppy, Vector2 pvx pvy, angle), (cols, rows, level))
  where larr = lines input
        (playerData, levelData) = splitAt 3 larr
        level = V.fromList leveltemp
        leveltemp :: [Int]
        leveltemp = map read $ concat $ map words levelData
        levelA = map words levelData
        cols = length levelA
        rows = length $ head levelA
        [ppx,ppy,pvx,pvy,angle] = map read $ words $ unlines playerData
