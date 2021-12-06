module Day6 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)

data Week = Week Int Int Int Int Int Int Int Int Int

day6 :: IO()
day6 = do
    ss <- getFile "day6.txt"
    let fish = convertToAmount . map read . splitOn "," $ ss 
    print $ sumWeek $ foldr (\x y -> updateFishies y) fish [1..80] 
    print $ sumWeek $ foldr (\x y -> updateFishies y) fish [1..256] 
    
sumWeek :: Week -> Int
sumWeek (Week a b c d e f g h i) = a + b + c + d + e + f + g + h + i

convertToAmount :: [Int] -> Week
convertToAmount list = intToWeek $ map (\x -> length $ filter (==x) list) [0..8]  
    where
        intToWeek :: [Int] -> Week
        intToWeek (a:b:c:d:e:f:g:h:i:[]) = Week a b c d e f g h i

updateFishies :: Week -> Week 
updateFishies (Week a b c d e f g h i) = Week b c d e f g (h+a) i a
