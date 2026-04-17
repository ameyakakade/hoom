{-# LANGUAGE PatternSynonyms #-}
module ParseLevel (parseLevel) where

import Constants
import Raylib.Types (Vector2, pattern Vector2, vector2'x, vector2'y)

parseLevel :: String -> (Player, Scene)
parseLevel input = ((Vector2 ppx ppy, Vector2 pvx pvy, angle), (cols, rows, level))
  where larr = lines input
        (playerData, levelData) = splitAt 3 larr
        level = map (map read) $ map words levelData
        cols = length level
        rows = length $ head level
        [ppx,ppy,pvx,pvy,angle] = map read $ words $ unlines playerData
