module Day1 where

import GetInput (getFile)

day1 :: IO ()
day1 = do
    input <- getFile "day1.txt"
    let s = map read (words input) :: [Integer]
    print $ length $ filter (>0) (zipWith (\x y -> y - x) s (drop 1 s))
    print $ length $ filter (>0) (zipWith (\x y -> y - x) s (drop 3 s))
