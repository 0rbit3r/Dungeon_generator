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
    | (x == 1) || x == ((getWidth dungeon) - 2) || (y==1) || (y==((getHeight dungeon) - 2)) = endWithRoom dungeon (x,y) randoms
    | length == maxLen = endWithRoom dungeon (x,y) randoms
    | currPix /= cell_EMPTY = dungeon --Add door?
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



makeBegleri :: DungeonMap
                 -> [Int]
                 -> Int 
                 -> DungeonMap
makeBegleri dungeon randoms trys
    | trys <= 0             = dungeon
    | availableSpace < 3    = makeBegleri dungeon (drop 2 randoms) (trys - 1)
    | otherwise             
            = makeSnake roomedMap (doorX, doorY, 0) 30 dir (drop 7 randoms) 
    where
        x               = randomNum (randoms !! 0) (1, getWidth dungeon - 2)
        y               = randomNum (randoms !! 1) (1, getHeight dungeon - 2)
        availableSpace  = (fst $ getFreeSpace dungeon (x,y)) - 2
        roomWidth       = max 3 $ availableSpace - (randomNum (randoms !! 2) (0,4))
        roomHeight      = max 3 $ availableSpace - (randomNum (randoms !! 3) (0,4))
        roomedMap       = createRoom dungeon (x,y) (roomWidth, roomHeight)
        (dir, (doorX, doorY))       = getEnterance dungeon (x,y) (roomWidth, roomHeight) (drop 4 randoms)

-- | TODO: check for bounds errors
-- Generates random coordinates of an enterance to a given room

getEnterance :: DungeonMap
                -> (Int, Int)
                -> (Int, Int)
                -> [Int]
                -> (Direction, (Int, Int))
getEnterance dungeon (x, y) (width, height) randoms 
    | dir == Dup    = (Dup, (x + randX, y - halfY - 1))
    | dir == Ddown  = (Ddown, (x + randX, y + halfY + 1))
    | dir == Dleft  = (Dleft, (x - halfX - 1, y + randY))
    | dir == Dright = (Dright, (x + halfX + 1, y + randY))
    where
        rand = (randoms !! 0)
        randX = randomNum (randoms !! 1) (-1 * halfX, halfX)
        halfX =  div width 2
        randY = randomNum (randoms !! 2) (-1 * halfY, halfY)
        halfY = div height 2
        dir
            | (rand < 250) && (y - halfY > 3) = Dup
            | (rand < 500) && (y + halfY < getHeight dungeon - 4) = Ddown
            | (rand < 750) && (x - halfX > 3) = Dleft
            | (x + halfX < getWidth dungeon - 4)  = Dright
            | (y - halfY > 3) = Dup
            | (y + halfY < getHeight dungeon - 4) = Ddown
            | otherwise = Dleft



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


createDungeon :: DungeonMap -> [Int] -> Int -> DungeonMap
createDungeon dungeon randoms begleriNum
    | begleriNum <= 0   = dungeon
    | otherwise         = createDungeon (makeBegleri dungeon randoms 5) (drop 100 randoms) (begleriNum - 1)

-- >>>consoleRender $ createDungeon (emptyMap 40 40) (randomNums (mkStdGen 5) (0,1000)) 20
--                                                                                 
--                       OOOOOOOOOOOOOO                                            
--                       OOOOOOOOOOOOOO                                            
--                       OOOOOOOOOO                                        OOOOOO  
--                       OOOOOOOOOO                                        OOOOOO  
--                                     OOOOOO                          ####OOOOOO  
--     OOOOOOOOOOOOOO                  OOOOOO                          ##  OOOOOO  
--     OOOOOOOOOOOOOO                  OOOOOO                          ##  OOOOOO  
--     OOOOOOOOOOOOOO                  OOOOOO##########                ##          
--     OOOOOOOOOOOOOO############      OOOOOO        ##          OOOOOOOOOO        
--     OOOOOOOOOOOOOO      ##  ##                    ##          OOOOOOOOOO        
--     OOOOOOOOOOOOOO      OOOOOOOOOOOOOO            ##          OOOOOOOOOO        
--     OOOOOOOOOOOOOO      OOOOOOOOOOOOOO            ############OOOOOOOOOO        &
--   OOOO                  OOOOOOOOOOOOOO          ######        OOOOOOOOOO        
--   OOOO                  OOOOOOOOOOOOOO          ##  ##                          
--   OOOOOO                OOOOOOOOOOOOOO    OOOOOO##  OOOOOO                      
--   OOOOOO                OOOOOOOOOOOOOO    OOOOOO##  OOOOOO                      
--   OOOOOO                OOOOOOOOOOOOOO    OOOOOO##  OOOOOO        OOOOOOOOOO    
--   OOOOOO                    ##            ####  ##                OOOOOOOOOO    
--   OOOOOO                    ##################  ####              OOOOOOOOOO    
--   OOOOOO                                ##        ##              OOOOOOOOOO    
--   OOOOOO                                ##        ##              OOOOOOOOOO    
--                                         ##        ##              ##            
--   OOOO            OOOOOOOOOO            ##        OOOOOOOOOO      ##            
--   OOOO############OOOOOOOOOO            ##        OOOOOOOOOO########            
--   OOOO            OOOOOOOOOO      OOOOOOOOOOOOOO  OOOOOOOOOO      ##            
--   OOOO            ##              OOOOOOOOOOOOOO  OOOOOOOOOO########            
--   OOOO            ##              OOOOOOOOOOOOOO  OOOOOOOOOO            OOOOOO  
--   OOOOOOOOOOOOOO  ##              OOOOOOOOOOOOOO                        OOOOOO  
--   OOOOOOOOOOOOOO  ####OOOOOO      OOOOOOOOOOOOOO                OOOOOO##OOOOOO  
--   OOOOOOOOOOOOOO      OOOOOO      OOOOOOOOOOOOOO                OOOOOO  OOOOOO  
--   OOOOOOOOOOOOOO      OOOOOO      OOOOOOOOOOOOOO                OOOOOO  OOOOOO  
--   OOOOOOOOOOOOOO                        ############################            
--                       OOOOOOOOOO        ####                      ##            
--   OOOOOO              OOOOOOOOOO####OOOOOOOOOOOOOO####################          
--   OOOOOO              OOOOOOOOOO    OOOOOOOOOOOOOO    OOOOOOOOOOOOOO##          
--   OOOOOO############################OOOOOOOOOOOOOO####OOOOOOOOOOOOOO##          
--   OOOOOO                            OOOOOOOOOOOOOO    OOOOOOOOOOOOOOOOOO        
--   OOOOOO                            OOOOOOOOOOOOOO    OOOOOOOOOOOOOOOOOO        
--                                                                                 
--


-- >>> randomNum 420 (100,150)
-- 121
--

-- >>>take 20 (randomNums (mkStdGen 10) (0,1000))
-- [942,533,979,879,140,143,940,149,6,249,978,495,398,350,833,904,301,68,111,307]
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

-- >>> consoleRender (makeSnake (emptyMap 20 20) (13,13,0) 15 Dleft (randomNums  (mkStdGen 5678674568) (0 ,1000)))
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________________________
-- ________________________HHHH____________
-- ________________________HH______________
-- ________________________HH______________
-- __________________RRRRRRRRRRRRRR________
-- __________________RRRRRRRRRRRRRR________
-- __________________RRRRRRRRRRRRRR________
-- ________________________________________
--


-- 0 1 2 3 4 5 6 7 8 9 10111213141516171819
-- ________________________________________ 0
-- ________________________________________ 1
-- ________________________________________ 2
-- ________________________________________ 3
-- ________________________________________ 4
-- __________XX____________________________ 5
-- ________________________________________ 6
-- ________________________________________ 7
-- ______RRRRRRRRRR________________________ 8
-- ______RRRRRRRRRR________________________ 9
-- ____XXRRRR  RRRR______XX________________ 10
-- ______RRRRRRRRRR________________________ 11
-- ______RRRRRRRRRR________________________ 12
-- ______HH________________HHHH____________ 13
-- ______HHHHHHHHHHHHHHHHHHHH______________ 14
-- ________________________________________ 15
-- ________________________________________ 16
-- ________________________________________ 17
-- ________________________________________ 18
-- ________________________________________ 19