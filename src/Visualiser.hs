module Visualiser
where

import Data.Array
import Map


-- >>> consoleRender (emptyMap 8 12)

consoleRender :: DungeonMap -> IO()
consoleRender map =
    consoleRender0 map (getWidth map, getHeight map) (0,0)

consoleRender0 :: DungeonMap -> (Int, Int) -> (Int, Int) -> IO()
consoleRender0 map dimensions@(width, height) (x, y)
    | (x == width - 1 && y == height - 1) = do
        putStrLn [(getPixel map x y), (getPixel map x y)]
    | x == width - 1 = do
        putStrLn [(getPixel map x y), (getPixel map x y)]
        consoleRender0 map dimensions (0, y + 1)
    | otherwise = do
        putStr [(getPixel map x y), (getPixel map x y)]
        consoleRender0 map dimensions (x + 1, y)


