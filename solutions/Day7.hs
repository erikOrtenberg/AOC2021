module Day7 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as M

day7 :: IO()
day7 = do
    ss <- getFile "day7.txt"
    let crabs = foldr (\x -> M.insertWith (+) x 1) M.empty $ map read $ splitOn "," ss :: Map Int Int
    let ((minCrab, a), (maxCrab, b)) = (M.findMin crabs, M.findMax crabs)
    print $ minimum $ map (\x -> getFuelCost x crabs) [minCrab .. maxCrab] 
    print $ minimum $ map (\x -> getFuelCost' x crabs) [minCrab .. maxCrab] 
    putStrLn ""

tempMap = M.insert 5 1 $ M.insert 10 2 M.empty :: Map Int Int
tempMap2 = M.insert 10 2 M.empty :: Map Int Int

getFuelCost :: Int -> Map Int Int -> Int
getFuelCost day m = sum . M.elems $ M.mapWithKey (\k v -> (abs (day - k)) * v) m

getFuelCost' :: Int -> Map Int Int -> Int 
getFuelCost' day m = sum . M.elems $ M.mapWithKey (\k v -> (abs (day - k)) * (1 + abs (day - k)) * v  `div` 2) m