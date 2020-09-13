module Map
where
    
import Data.Array


cell_EMPTY = ' '
cell_ROOM = 'O'
cell_HALLWAY = '#'

data DungeonMap = Invalid | DungeonMap (Array (Int, Int) Char) (Int, Int)

emptyMap :: Int -> Int -> DungeonMap
emptyMap n m = DungeonMap (array ((0,0),(n - 1, m - 1)) [((a,b),cell_EMPTY) | a <- [0..(n-1)], b <- [0..(m-1)]]) (n,m)

getPixel :: DungeonMap -> Int -> Int -> Char
getPixel dungeon@(DungeonMap arr _) x y
    | checkBounds dungeon x y =  arr ! (x,y)
    | otherwise = cell_EMPTY

setPixel :: DungeonMap -> Int -> Int -> Char -> DungeonMap
setPixel dungeon@(DungeonMap arr dims) x y char
    | checkBounds dungeon x y =  DungeonMap (arr // [((x,y), char)]) dims
    | otherwise = dungeon

getWidth :: DungeonMap -> Int
getWidth (DungeonMap _ (width, _)) = width

getHeight :: DungeonMap -> Int
getHeight (DungeonMap _ (_, height)) = height

checkBounds :: DungeonMap -> Int -> Int -> Bool
checkBounds dungeon x y =
    x > 0 && x < width - 1 && y > 0 && y < height - 1
    where
        width = getWidth dungeon
        height = getHeight dungeon


-- >>> emptyMap 5 5 ! (3,2)
-- '0'
--

-- >>> (array (1,10) [(a, 'h') | a <- [1..10]]) ! 5
-- 'h'
--

-- >>> getPixel (emptyMap 5 5) 3 2
-- '0'
--
-- >>> setPixel (emptyMap 3 3) 1 2 'h'
-- array ((0,0),(2,2)) [((0,0),'0'),((0,1),'0'),((0,2),'0'),((1,0),'0'),((1,1),'0'),((1,2),'h'),((2,0),'0'),((2,1),'0'),((2,2),'0')]
--
-- >>> length (emptyMap 4 5)
-- 20
--