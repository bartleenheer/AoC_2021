module Day2 where

import System.IO

changePos :: Integer -> Integer -> [[String]] -> Integer
changePos x y ([t,a]:tas)
    | t == "forward" = changePos (x+i) y tas
    | t == "down" = changePos x (y+i) tas
    | t == "up" = changePos x (y-i) tas
    where i = read a :: Integer
changePos x y [] = x * y


changeAim :: Integer -> Integer -> Integer -> [[String]] -> Integer
changeAim x y z ([t,a]:tas)
    | t == "forward" = changeAim (x+i) y (z + (y * i)) tas
    | t == "down" = changeAim x (y+i) z tas
    | t == "up" = changeAim x (y-i) z tas
    where i = read a :: Integer
changeAim x _ z [] = x * z

main :: IO ()
main = do
    withFile "2" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr "Puzzle1: "
        print (changePos 0 0 $ map words $ lines contents)
        putStr "Puzze2: "
        print (changeAim 0 0 0 $ map words $ lines contents)
        )