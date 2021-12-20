module Day9 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M


day9 :: IO()
day9 = do
    ss <- getFile "day9.txt"
    let depthMap = convertDepthMapToMap $ map (map read . tail . splitOn "") (lines ss) :: Map (Int, Int) Int
    print $ M.foldl (\x y -> x + y + 1) 0 $ getAllLowPoints depthMap
    print $ product $ take 3 $ sortBy (flip compare) $ M.elems $ M.mapWithKey (\k _ -> findBasinSize depthMap (99,99) k) (getAllLowPoints depthMap)



convertDepthMapToMap :: [[Int]] -> Map (Int, Int) Int
convertDepthMapToMap xs = foldl (\m (x,y) -> M.insert (x,y) (xs!!y!!x) m ) M.empty [(x,y) | x <- [0 .. length (head xs)-1], y <- [0.. length xs-1]]

getAllLowPoints :: Map (Int,Int) Int -> Map (Int,Int) Int
getAllLowPoints m = M.filterWithKey (\x _ -> isLowPoint m x) m

isLowPoint :: Map (Int, Int) Int -> (Int, Int) -> Bool
isLowPoint m (x,y) = lUp (x+1,y) > val && lUp (x-1,y) > val && lUp (x,y+1) > val && lUp (x,y-1) > val
    where
        val = fromMaybe 0 $ M.lookup (x,y) m
        lUp p = fromMaybe 10 $ M.lookup p m

findBasinSize :: Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
findBasinSize m size p = length $ nub $ findBasinCoords m size p

findBasinCoords :: Map (Int, Int) Int -> (Int,Int) ->(Int, Int) -> [(Int, Int)]
findBasinCoords m size (x,y)
    | x < 0 || y < 0 || x > fst size || y > snd size = []
    | otherwise = (x,y) : concatMap (findBasinCoords m size . fst) (filter tempF coords)
    where
        coords = [((x,y+1),top > mid && top /= 9),((x,y-1),bottom > mid && bottom /= 9),((x-1,y), left > mid && left /= 9),((x+1,y),right > mid && right /= 9)]
        mid = fromMaybe 0 $ M.lookup (x,y) m
        top = fromMaybe 10 $ M.lookup (x,y+1) m
        bottom = fromMaybe 10 $ M.lookup (x,y-1) m
        left = fromMaybe 10 $ M.lookup (x-1,y) m
        right = fromMaybe 10 $ M.lookup (x+1,y) m
        tempF :: ((Int, Int), Bool) -> Bool
        tempF val = snd val