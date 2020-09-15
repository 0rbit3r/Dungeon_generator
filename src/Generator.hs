module Generator
where

import Data.Array
import System.Random
import Visualiser
import Map
import Direction

-- | This function creates necessary parameters and calls 'createRoom0'
-- Parts of the room can be out of bounds without error
createRoom :: DungeonMap    -- ^Original dungeon to add the room to
              -> (Int, Int) -- ^coordinates of rooms center
              -> (Int, Int) -- ^Dimensions
              -> DungeonMap -- ^Resulting dungeon with the newly created room
createRoom dungeon (centerX,centerY) (width, height) =  
    createRoom0 dungeon xStart yStart (xStart, xEnd) yEnd
    where
        xStart = max 1 $ centerX - width `div` 2
        yStart = max 1 $ centerY - height `div` 2
        xEnd = min ((getWidth dungeon) - 2) $ (centerX + width `div` 2)
        yEnd = min ((getHeight dungeon) - 2) $ (centerY + height `div` 2)

-- | Creates room from its center position and dimensions (The resulting room will always have odd dimensions due to difficulties of handling center of even sized rooms)
-- Works by scanning from top left to botoom right and changing every pixel to R. 
createRoom0 :: DungeonMap       -- ^Original dungeon to add the current pixel to
               -> Int           -- ^Current x coordinate
               -> Int           -- ^Current x coordinate
               -> (Int, Int)    -- ^X Starting and ending point of the room (Don't change during recursion)
               -> Int           -- ^Y ending point of the room (bottom most row) (Don't change during recursion)
               -> DungeonMap    -- ^Resulting DunegonMap
createRoom0 dungeon x y xData@(xStart, xEnd) yEnd
    | (x == xEnd && y==yEnd)    = setPixel dungeon x y cell_ROOM
    | x == xEnd                 = createRoom0 (setPixel dungeon x y cell_ROOM) xStart (y + 1) xData yEnd
    | otherwise                 = createRoom0 (setPixel dungeon x y cell_ROOM) (x + 1) y xData yEnd



-- | Creates new dungeon based on size and seed
generateMap :: (Int, Int)       -- ^Dimensions of the dungeon
               -> Int           -- ^Seed for the dungeon
               -> DungeonMap    -- ^Resulting Dungeon
generateMap (width, height) = undefined

-- | Creates hallway that slithers around until it is long enough or reaches room/hallway.
-- If the limit is reached a new room is created.
makeSnake :: DungeonMap         -- ^Original dungeon to add the hallway to
             -> (Int, Int, Int) -- ^Current x and y position and current length
             -> Int             -- ^MaxLength
             -> Direction       -- ^Current Direction
             -> [Int]           -- ^Infinite list of pseudorandom numbers
             -> DungeonMap      -- ^Resulting dungeon with the hallway added
makeSnake dungeon (x,y,length) maxLen dir randoms
    | length >= maxLen = endWithRoom dungeon (x,y) randoms
    | (x0 == 1) || x0 == ((getWidth dungeon) - 2) || (y0==1) || (y0==((getHeight dungeon) - 2)) = makeSnake dungeon (x, y, length + 1) maxLen (rotR dir) (drop 2 randoms) --endWithRoom dungeon (x,y) randoms
    | currPix /= cell_EMPTY = dungeon
    | otherwise = makeSnake newDungeon (x0,y0, length + 1) maxLen dir0 (drop 2 randoms)
    where
        newDungeon = setPixel dungeon x y cell_HALLWAY
        currPix = getPixel dungeon x y
        dir0
            | (head randoms) > 950 = rotL dir
            | (randoms !! 1) < 50 = rotR dir
            | otherwise           = dir 
        x0 = x + fst (gtVect dir0)
        y0 = y + snd (gtVect dir0)


-- |Creates a structure with at least one Room and a hallway exiting from it.
-- First the function randomly selects a spot for the first room. If the spot is not suitable the function will recursively try again (up to "trys" times)
-- Then it randomly selects direction and location of the hallway using 'getEnterance'
-- The hallway is build using 'makeSnake'. Therefore the hallway can end either with a room or by attaching to another structure.
makeBegleri :: DungeonMap   	-- ^Initial Dungeon
                 -> [Int]       -- ^Infinite list of random numbers from 0 to 1000
                 -> Int         -- ^Number of trys the function should put into finding suitable spot
                 -> DungeonMap  -- ^Resulting dungeon
makeBegleri dungeon randoms trys
    | trys <= 0             = dungeon
    | availableSpace < 3    = makeBegleri dungeon (drop 2 randoms) (trys - 1)
    | otherwise             
            = makeSnake roomedMap (doorX, doorY, 0) 30 dir (drop 7 randoms) 
    where
        x               = randomNum (randoms !! 0) (3, getWidth dungeon - 4)
        y               = randomNum (randoms !! 1) (3, getHeight dungeon - 4)
        availableSpace  = (fst $ getFreeSpace dungeon (x,y)) - 2
        roomWidth       = max 3 $ availableSpace - (randomNum (randoms !! 2) (0,4))
        roomHeight      = max 3 $ availableSpace - (randomNum (randoms !! 3) (0,4))
        roomedMap       = createRoom dungeon (x,y) (roomWidth, roomHeight)
        (dir, (doorX, doorY))       = getEnterance dungeon (x,y) (roomWidth, roomHeight) (drop 4 randoms)
        
--
-- |Generates random coordinates of an enterance to a given room and the natural direction from it.
getEnterance :: DungeonMap                  -- ^The dungeon to modify
                -> (Int, Int)               -- ^Center X and Y coordinates of the room
                -> (Int, Int)               -- ^Dimensions of the room
                -> [Int]                    -- ^Infinite list of random numbers from 0 to 1000
                -> (Direction, (Int, Int))  -- ^The direction and coordinates of the start of a hallway
getEnterance dungeon (x, y) (width, height) randoms 
    | dir == Dup    && checkBounds dungeon (x + randX) (y - halfY - 1) = (Dup, (x + randX, y - halfY - 1))
    | dir == Ddown  && checkBounds dungeon (x + randX) (y + halfY + 1) = (Ddown, (x + randX, y + halfY + 1))
    | dir == Dleft  && checkBounds dungeon (x - halfX - 1) (y + randY) = (Dleft, (x - halfX - 1, y + randY))
    | dir == Dright && checkBounds dungeon (x + halfX + 1) (y + randY) = (Dright, (x + halfX + 1, y + randY))
    | otherwise = getEnterance dungeon (x, y) (width, height) (drop 1 randoms)
    where
        randX = randomNum (randoms !! 1) (-1 * halfX, halfX)
        halfX =  div width 2
        randY = randomNum (randoms !! 2) (-1 * halfY, halfY)
        halfY = div height 2
        rand = (randoms !! 0)
        dir
            | (rand < 250) = Dup
            | (rand < 500) = Ddown
            | (rand < 750) = Dleft
            | otherwise = Dright

-- | Used to make a Room at the end of a hallway. This function finds how big a room can be and then it randomises the shape a little
endWithRoom ::  DungeonMap      -- ^The Dungeon
                -> (Int, Int)   -- ^Coordinates
                -> [Int]        -- ^Random numbers
                -> DungeonMap   -- ^Resulting Dungeon
endWithRoom dungeon loc@(x,y) randoms =
    createRoom  dungeon loc (width, height)
    where
        maxSize = (fst (getFreeSpace dungeon loc)) - 2
        width =  max 3 $ maxSize - (randomNum (head randoms) (0,2))
        height = max 3 $ maxSize - (randomNum (head randoms) (0,2))

-- | Creates infinite list of random numbers that fall between given bounds
randomNums :: RandomGen a =>    a -- ^Random generator
                    -> (Int, Int) -- ^Bounds
                    -> [Int]      -- ^Resulting infinite list
randomNums gen0 bounds@(low, high) =
    (num:randomNums gen1 bounds)
    where
        (num, gen1) = randomR (low, high) gen0

-- | Gets number in given bounds from an infinite list of random numbers between 0 - 1000
randomNum :: Int            -- ^Infinite list of random numbers in range [0,100]
            -> (Int, Int)   -- ^Bounds
            -> Int          -- ^Resulting random Int
randomNum rand (low, high) = ((rand * (high - low)) `div` 1000) + low


-- | Returns a tuple of size three containing:
-- maximum size of square room you can create at a given point and nearest
-- X and y coordinates of the closest non Empty cell
-- Creates parameteres for and calls 'getFreeSpace0'
getFreeSpace :: DungeonMap          -- ^Dungeon in question
                -> (Int, Int)       -- ^Pixel to check coordinates.
                -> (Int, (Int, Int))  -- ^Result - max size of creatable room and coordinates of the closest Room pixel)
getFreeSpace dungeon (x,y)
    | checkBounds dungeon x y && (getPixel dungeon x y == cell_EMPTY) = getFreeSpace0 dungeon (max 1 $ x - 1, max 1 $ y - 1) (x,y) 3 9
    | otherwise = (0,(-1,-1))

-- | Is called by 'getFreeSpace'. Recursively tries bigger and bigger squares, scanning each one pixel by pixel unitl it reaches a Room pixel.
getFreeSpace0 :: DungeonMap         -- ^The dungeon in question
                -> (Int, Int)       -- ^Current position (x,y)
                -> (Int, Int)       -- ^X and Y coordinates pixel in the center of the scanned space
                -> Int              -- ^Current free space size
                -> Int              -- ^Limit
                -> (Int, (Int, Int))  -- ^Result - Size of the biggest non intersecting square + coordinates to the nearest non epnty cell
getFreeSpace0 dungeon (x,y) (centerX, centerY) size limit
    | size > limit                      = (size - 2,(-1,-1))
    | getPixel dungeon x y == cell_ROOM = (size -2, (x, y)) 
    | (x == xEnd && y==yEnd)            = getFreeSpace0 dungeon (max 1 $ xStart - 1, max 1 $ yStart - 1) (centerX, centerY) (size + 2) limit
    | x == xEnd                         = getFreeSpace0 dungeon (xStart, y + 1) (centerX, centerY) size limit
    | otherwise                         = getFreeSpace0 dungeon (x + 1, y) (centerX, centerY) size limit
    where
        xStart = max 1 $ centerX - size `div` 2
        yStart = max 1 $ centerY - size `div` 2
        xEnd = min ((getWidth dungeon) - 2) $ (centerX + size `div` 2)
        yEnd = min ((getHeight dungeon) - 2) $ (centerY + size `div` 2)

-- |Creates the dungeon by repeatedly calling 'makeBegleri'
createDungeon :: DungeonMap     -- ^Initial dungeon (usually 'emptyMap')
                -> [Int]        -- ^infinite list of random numbers from 0 to 1000
                -> Int          -- ^Number of begleries to try to fit into the map.
                -> DungeonMap   -- ^Resulting dungeon
createDungeon dungeon randoms begleriNum
    | begleriNum <= 0   = dungeon
    | otherwise         = createDungeon (makeBegleri dungeon randoms 5) (drop 1000 randoms) (begleriNum - 1)






-- >>>consoleRender $ createDungeon (emptyMap 40 40) (randomNums (mkStdGen 29) (0,1000)) 30
--                                                                                 
--     ##############          ######                            ##########        
--     ##          ##          ##  ##  ######            ######  ##############    
--     ##          ##          ##  ##  ######            ##################  ##    
--     ##          ##          ##  ##  ######            ######  ##########  ##    
--     ##          ##          ##  ##  ##                        ##########  ##    
--     ##          ##        ########  ##                                    ##    
--     ##  ##############    ########  ##                                    ##    
--     ##  ##############    ########  ##                                    ##    
--     ##  ##############          ##  ##                                    ##    
--     ##  ##############          ##  ##############                        ##    
--     ##################          ##    ##        ##          ######        ##    
--     ##  ##############          ##    ##        ##          ################    
--     ##  ##############          ##    ##        ##          ######        ##    
--     ####    ##                  ##    ##        ##                        ##    
--       ##  ######                ##    ##        ##############            ##    
--       ##  ######                ##    ##        ##############            ##    
--       ##  ######                ##    ##        ##############            ##    
--       ##                        ##    ##        ##############            ##    
--       ##    ############################        ##############            ##    
--       ##    ##############            ##                                  ##    
--       ##    ##############            ##                                  ##    
--       ##    ##############            ##                                  ##    
--       ##    ##############            ##        ####################      ##    
--       ##    ##############            ##        ##                ##      ##    
--       ##    ##############    ######  ##        ##                ##      ##    
--       ##                    ########  ##        ##                ##      ##    
--       ##                    ########  ##        ##          ##########    ##    
--     ########                ##        ##        ##  ######  ##########    ##    
--     ##########################        ##        ##  ######  ##########    ##    
--     ########              ####      ######      ##  ######                ##    
--     ########              ####      ######      ##  ##                    ##    
--     ########              ####      ######      ##  ##  ######            ##    
--     ########  ######      ####                  ######  ######    ######  ##    
--     ########  ######      ####                  ##      ####################    
--               ######      ####        ######    ##      ######    ######        
--                 ##        ####        ############      ######                  
--                                                                                 
--                                                                                 
--


-- >>>take 20 (randomNums (mkStdGen 4) (0,1000))
-- [29,526,311,93,723,139,329,263,436,824,756,418,561,375,511,227,891,750,851,560]
--

-- >>> getFreeSpace (makeSnake (createRoom (emptyMap 20 20) (5, 10) (4, 4)) (13,13,0) 20 Dleft (randomNums  (mkStdGen 1) (0 ,1000))) (2,10)
-- (1,3,9)
--

-- >>> getFreeSpace (createRoom (emptyMap 5 5) (2, 2) (1, 1)) (0,2)
-- (3,2,2)
--

-- >>> getPixel (makeSnake (createRoom (emptyMap 20 20) (5, 10) (4, 4)) (13,13,0) 20 Dleft (randomNums  (mkStdGen 1) (0 ,1000))) 3 10
-- 'R'
--
