module Day1 where

import GetInput (getFile)

day1 :: IO ()
day1 = do
    input <- getFile "day1.txt"
    let s = map read (words input) :: [Integer]
    putStrLn $ show $ length $ filter (>0) (zipWith (\x y -> y - x) s (drop 1 s))
    putStrLn $ show $ length $ filter (>0) (zipWith (\x y -> y - x) s (drop 3 s))
