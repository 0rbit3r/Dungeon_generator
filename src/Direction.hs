module Direction
where

data Direction = Dup | Ddown | Dleft | Dright
    deriving (Eq, Show)

rotL :: Direction -> Direction
rotL dir
    | dir == Dup = Dleft
    | dir == Dleft = Ddown
    | dir == Ddown = Dright
    | otherwise = Dup

rotR :: Direction -> Direction
rotR dir
    | dir == Dup = Dright
    | dir == Dleft = Dup
    | dir == Ddown = Dleft
    | otherwise = Ddown

gtVect :: Direction -> (Int, Int)
gtVect dir
    | dir == Dup = (0,-1)
    | dir == Dleft = (-1,0)
    | dir == Ddown = (0,1)
    | otherwise = (1,0)