module Direction
where

-- | Direction is used for 'makeSnake'. It stores direction (Duh)
data Direction = Dup | Ddown | Dleft | Dright
    deriving (Eq, Show)

-- |Rotates the direction by 90 degrees to the Left
rotL :: Direction       -- ^Direction to rotate
        -> Direction    -- ^Resulting direction
rotL dir
    | dir == Dup = Dleft
    | dir == Dleft = Ddown
    | dir == Ddown = Dright
    | otherwise = Dup

-- |Rotates the direction by 90 degrees to the Right
rotR :: Direction       -- ^Direction to rotate
        -> Direction    -- ^Resulting direction
rotR dir
    | dir == Dup = Dright
    | dir == Dleft = Dup
    | dir == Ddown = Dleft
    | otherwise = Ddown

-- |Returns the x and y delta values for a direction
gtVect :: Direction     -- ^Direction
        -> (Int, Int)   -- ^Resulting pair of deltas
gtVect dir
    | dir == Dup = (0,-1)
    | dir == Dleft = (-1,0)
    | dir == Ddown = (0,1)
    | otherwise = (1,0)