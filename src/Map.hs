module Map
where
    
import Data.Array

-- | Used as a console rendering character.
cell_EMPTY :: Char
cell_EMPTY = ' '
-- | Used as a console rendering character.
cell_ROOM :: Char
cell_ROOM = '#'
-- | Used as a console rendering character.
cell_HALLWAY :: Char
cell_HALLWAY = '#'

-- |Used as a holder for the 2D array that represents the Dungeon and its dimensions.
data DungeonMap = Invalid | DungeonMap (Array (Int, Int) Char) (Int, Int)

-- |Creates new empt dungeon with given dimensions.
emptyMap :: Int             -- ^width
            -> Int          -- ^height
            -> DungeonMap   -- ^Resulting dungeon
emptyMap n m = DungeonMap (array ((0,0),(n - 1, m - 1)) [((a,b),cell_EMPTY) | a <- [0..(n-1)], b <- [0..(m-1)]]) (n,m)

-- | Returns a pixel of the map at given coordinates.
getPixel :: DungeonMap  -- ^The Dungeon
            -> Int      -- ^The x coordinate
            -> Int      -- ^The y one.
            -> Char     -- ^The result
getPixel dungeon@(DungeonMap arr _) x y
    | checkBounds dungeon x y =  arr ! (x,y)
    | otherwise = cell_EMPTY

-- |Sets a pixel in the dungeon to the given one. Only way to directly modify a dungeon. 
setPixel :: DungeonMap      -- ^Initial dungeon
            -> Int          -- ^The x coordinate
            -> Int          -- ^The y coordinate
            -> Char         -- ^The char to which to set the pixel
            -> DungeonMap   -- ^The resulting dungeon
setPixel dungeon@(DungeonMap arr dims) x y char
    | checkBounds dungeon x y =  DungeonMap (arr // [((x,y), char)]) dims
    | otherwise = dungeon

-- |Returns width of the dungeon
getWidth :: DungeonMap -> Int
getWidth (DungeonMap _ (width, _)) = width

-- |Returns height of the dungeon
getHeight :: DungeonMap -> Int
getHeight (DungeonMap _ (_, height)) = height

-- |Returns True if the pixel lies in workable area of the dungeon (There is a margin of length 1 pixel at every side)
checkBounds :: DungeonMap -> Int -> Int -> Bool
checkBounds dungeon x y =
    x > 1 && x < width - 2 && y > 1 && y < height - 2
    where
        width = getWidth dungeon
        height = getHeight dungeon