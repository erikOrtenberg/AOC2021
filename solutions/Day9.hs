module Day9 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe

day9 :: IO()
day9 = do
    ss <- getFile "day9.txt"
    let depthMap = map (map read . tail . splitOn "") (lines ss) :: [[Int]]
    let nothing = replicate (length (head depthMap)) 10
    --print depthMap
    print $ sum $ zipWith3 getDangerLevel (nothing : depthMap) (map convertLine depthMap) (tail depthMap ++ [nothing])

n = replicate (length "7678934989765431234965432345679567899987999876543210198965019878921987654343498765678932445678998678") 0 ::[Int]
mm = convertLine $ map read $ tail $ splitOn "" "7678934989765431234965432345679567899987999876543210198965019878921987654343498765678932445678998678"
nn = map read $ tail $ splitOn "" "4567929878965420129874301286789456789976898989654321987894329767990986543232349874589321234567987567" ::[Int]
z = [1,2,3,4,5,6,6,2] :: [Int]
t = convertLine [5,1,2,3,5,1,2,0] 
lol= [9,3,5,6,2,4,5,2] :: [Int]

t0= replicate 3 Nothing :: [Maybe Int]
t1= [Just 1, Nothing, Nothing] :: [Maybe Int]


nope = replicate (length (head testDay9)) 0 ::[Int]
testDay9 =  map (map read . tail . splitOn "") (lines "2199943210\n3987894921\n9856789892\n8767896789\n9899965678") :: [[Int]]

getDangerLevel :: [Int] -> [Maybe Int] -> [Int] -> Int
getDangerLevel _ [] _ = 0
getDangerLevel (top:xs) (y:ys) (bottom:zs)
    | mid < top && mid < bottom = 1 + mid + getDangerLevel xs ys zs
    | otherwise = getDangerLevel xs ys zs
    where
        mid = fromMaybe 10 y

convertLine :: [Int] -> [Maybe Int]
convertLine xs = zipWith3 convertElem (10:xs) xs (tail xs ++ [10])

convertElem :: Int -> Int -> Int -> Maybe Int
convertElem left mid right
    | mid < left && mid < right = Just mid
    | otherwise = Nothing