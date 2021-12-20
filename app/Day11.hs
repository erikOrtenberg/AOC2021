module Day11 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

day11 :: IO()
day11 = do
    ss <- getFile "day11.txt"
    let octos = convertToMap $ map (map read . tail . splitOn "") (lines ss) :: Map (Int, Int) Int
    --part 1
    print $ snd $ stepXDays (octos, 0) 100
    --part 2
    print $ (findFirstSimFlash octos 0) + 1

findFirstSimFlash :: Map (Int, Int) Int -> Int -> Int
findFirstSimFlash m days 
    | M.size m == n = days
    | otherwise = findFirstSimFlash nextM (days + 1)
    where 
        (nextM, n) = stepOneDay m

stepXDays :: (Map (Int, Int) Int, Int) -> Int -> (Map (Int, Int) Int, Int)
stepXDays m 0 = m
stepXDays m n = stepXDays (newM, newN + (snd m)) (n-1)
    where
        (newM, newN) = stepOneDay (fst m)

stepOneDay :: Map (Int, Int) Int -> (Map (Int, Int) Int, Int)
stepOneDay m 
    | length triggeredOctos > 0 = triggerOctos increasedOctos 0 triggeredOctos
    | otherwise = (increasedOctos, 0)
    where 
        increasedOctos = increaseEnergyLevel m
        triggeredOctos = getTriggerCoords increasedOctos

resetTriggeredOctos :: Map (Int, Int) Int -> Map (Int, Int) Int
resetTriggeredOctos m = M.map f m
    where
        f x 
            | x >= 10 = 0
            | otherwise = x

triggerOctos :: Map (Int, Int) Int -> Int -> [(Int, Int)] -> (Map (Int, Int) Int, Int) 
triggerOctos octos n [] = (resetTriggeredOctos octos, n)
triggerOctos octos n (cur:triggered) = triggerOctos newOctos (n+1) (nub (triggered ++ newTriggers)) 
    where
        newOctos = triggerOcto octos cur 
        newTriggers = map snd $ filter (\x -> 10 == fst x) $ zip (map (\x -> fromMaybe 0 $ M.lookup x newOctos) [(fst cur + x, snd cur + y) | x <- [-1,0,1], y <- [-1,0,1]]) [(fst cur + x, snd cur + y) | x <- [-1,0,1], y <- [-1,0,1]]

triggerOcto :: Map (Int, Int) Int -> (Int, Int) -> Map (Int, Int) Int 
triggerOcto m p = foldr (\(x,y) m -> M.adjust (+1) (x,y) m) m [(fst p + x, snd p + y) | x <- [-1,0,1], y <- [-1,0,1]]

convertToMap :: [[Int]] -> Map (Int, Int) Int
convertToMap xs = foldl (\m (x,y) -> M.insert (x,y) (xs!!y!!x) m ) M.empty [(x,y) | x <- [0 .. length (head xs)-1], y <- [0.. length xs-1]]

increaseEnergyLevel :: Map (Int, Int) Int -> Map (Int, Int) Int
increaseEnergyLevel m = M.map (+1) m

getTriggerCoords :: Map (Int, Int) Int -> [(Int, Int)]
getTriggerCoords m = M.keys $ M.filter (>= 10) m