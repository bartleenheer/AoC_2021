module Day1 where

import System.IO

toInt :: [String] -> [Integer]
toInt = map (\ x -> read x :: Integer)

countIncreased :: [Integer] -> [Integer]
countIncreased (x:y:xs)
    | y > x = 1 : countIncreased (y:xs)
    | otherwise = countIncreased (y:xs)
countIncreased [x] = []

makeWindows :: [Integer] -> [Integer]
makeWindows (x:y:z:xs) = sum [x,y,z] : makeWindows (y:z:xs)
makeWindows [x,y] = []

main :: IO ()
main = do
    withFile "1" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr "Puzzle1: "
        print (sum $ countIncreased $ toInt $ lines contents)
        putStr "Puzze2: "
        print (sum $ countIncreased $ makeWindows $ toInt $ lines contents)
        )