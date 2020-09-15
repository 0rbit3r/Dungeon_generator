module Main where
import Generator
import Map
import Visualiser
import System.Random

prompt x = do
    putStrLn x
    number <- getLine
    return number

main :: IO ()
main = do
    number <- prompt " Please input a seed: "
    consoleRender $ createDungeon (emptyMap 40 40) (randomNums (mkStdGen (read number :: Int)) (0,1000)) 40


-- >>>consoleRender $ createDungeon (emptyMap 40 40) (randomNums (mkStdGen (read number :: Int)) (0,1000)) 40