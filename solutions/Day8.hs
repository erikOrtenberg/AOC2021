module Day8 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as M

day8 :: IO()
day8 = do
    ss <- getFile "day8.txt"
    let measurment = map ((\(x:y:_) ->  [words x, words y] ) . splitOn "|") (lines ss)
    --print $ concatMap (!!1) measurment
    print $ sum $ map (getAllOfSameLength (concatMap (!!1) measurment)) ([2,3,4,7] :: [Int])
    print $ sum $ zipWith findCode (map (!!0) measurment) (map (!!1) measurment)

findCode :: [String] -> [String] -> Int
findCode cypher output = convertOutput (orderCypher (map sort cypher)) (map sort output)

orderCypher :: [String] -> [String]
orderCypher unsortedCypher = [zero,one,two,three,four,five,six,seven,eight,nine]
    where
        (one:_) = filter (\x -> 2 == length x) unsortedCypher
        (four:_) = filter (\x -> 4 == length x) unsortedCypher
        (seven:_) = filter (\x -> 3 == length x) unsortedCypher
        (eight:_) = filter (\x -> 7 == length x) unsortedCypher
        (three:_) = filter (\x -> length x == 5 && length (intersect one x) == 2) unsortedCypher
        (nine:_) = filter (\x -> length (intersect three x) == 5) unsortedCypher \\ [eight, three]
        zeroSix = filter (\x -> length (eight \\ x) == 1) unsortedCypher \\ [nine]
        (zero:_) = filter (\x -> length (intersect one x) == 2) zeroSix
        (six:_) = zeroSix \\ [zero]
        twoFive = unsortedCypher \\ [zero,one,three,four,six,seven,eight,nine]
        (two:_) = filter (\x -> length (intersect nine x) == 4) twoFive
        (five:_) = twoFive \\ [two]

convertOutput :: [String] -> [String] -> Int
convertOutput _ [] = 0
convertOutput lookup (x:output)
    | index < 0 = 0
    | otherwise = 10^ length output * index + convertOutput lookup output
    where
        index = extractMaybe $ elemIndex x lookup

extractMaybe :: Maybe Int -> Int
extractMaybe (Just x) = x
extractMaybe Nothing = -1

getAllOfSameLength :: [String] -> Int -> Int
getAllOfSameLength list len  = length $ filter (\x -> length x == len) list