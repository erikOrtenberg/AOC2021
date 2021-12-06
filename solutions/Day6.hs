module Day6 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)

day6 :: IO()
day6 = do
    ss <- getFile "day6.txt"
    let fish = (iterate updateFishies (convertToAmount $ map read $ splitOn "," ss)) ::[[Int]]

    print $ sum $ fish!!80
    print $ sum $ fish!!256
    
convertToAmount :: [Int] -> [Int] 
convertToAmount list = map (\x -> length $ filter (==x) list) [0..8]  

updateFishies :: [Int] -> [Int]
updateFishies [] = []
updateFishies (x:xs) 
    | x > 0 = zipWith (+) (xs ++ (repeat 0)) ((take 6 (repeat 0)) ++ [x,0,x]) 
    | otherwise = (xs ++ (repeat 0))  
