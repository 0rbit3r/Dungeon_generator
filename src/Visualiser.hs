module Visualiser
where

import Data.Array
import Map

-- | Renders a dungeon in console by calling 'consoleRender'
consoleRender :: DungeonMap -- ^Dungeon to render
                -> IO ()      -- ^IO
consoleRender map =
    consoleRender0 map (getWidth map, getHeight map) (0,0)

-- |Renders the dungeon by scanning the Dungeon array and printing every symbol (twice for visual ratio closer to 1:1)
consoleRender0 :: DungeonMap    -- ^Dungeon to render
                -> (Int, Int)   -- ^Dimensions of the Dungeon
                -> (Int, Int)   -- ^Current pixel to be printed
                -> IO()         -- ^IO
consoleRender0 map dimensions@(width, height) (x, y)
    | (x == width - 1 && y == height - 1) = do
        putStrLn [(getPixel map x y), (getPixel map x y)]
    | x == width - 1 = do
        putStrLn [(getPixel map x y), (getPixel map x y)]
        consoleRender0 map dimensions (0, y + 1)
    | otherwise = do
        putStr [(getPixel map x y), (getPixel map x y)]
        consoleRender0 map dimensions (x + 1, y)


