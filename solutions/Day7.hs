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
    print $ minimum $ map (\x -> getFuelCost (\k v -> (abs (x - k)) * v) crabs) [minCrab .. maxCrab] 
    print $ minimum $ map (\x -> getFuelCost (\k v -> (abs (x - k)) * (1 + abs (x - k)) * v  `div` 2) crabs) [minCrab .. maxCrab] 

getFuelCost ::  (Int -> Int -> Int) -> Map Int Int -> Int
getFuelCost f m = sum . M.elems $ M.mapWithKey f m