module Day5 where

import GetInput (getFile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

data Line = Line (Int, Int) (Int, Int) 
    deriving Show



day5 :: IO ()
day5 = do
    ss <- getFile "day5.txt"
    let first = filter (\x -> isOrthogonal x) $ map createLine $ map (splitOn " -> ") $ lines ss
    let second = filter (\x -> isOrthogonal x || isDiagonal x) $ map createLine $ map (splitOn " -> ") $ lines ss
    print $ M.size $ M.filter (>1) $ getNumberOfOverlaps first
    print $ M.size $ M.filter (>1) $ getNumberOfOverlaps second

l1 = Line (2,3) (5,3)
l2 = Line (5,2) (5,5)
l3 = Line (1,1) (3,3)
l4 = Line (9,7) (7,9)

getNumberOfOverlaps :: [Line] -> Map (Int, Int) Int
getNumberOfOverlaps ls = foldr (\x -> M.insertWith (+) x 1) M.empty $ concatMap lineToList ls

getOverlap :: Line -> Line -> [(Int, Int)]
getOverlap l1 l2 = intersect (lineToList l1) (lineToList l2)

lineToList :: Line -> [(Int, Int)]
lineToList line 
    | isOrthogonal line = orthogonalLineToList line
    | otherwise = diagonalLineToList line

orthogonalLineToList :: Line -> [(Int, Int)]
orthogonalLineToList (Line (x1,y1) (x2,y2))
    | x > y = zip (take x (iterate (+1) x1)) (take x (repeat y1))
    | otherwise = zip (take y (repeat x1)) (take y (iterate (+1) y1))
    where
        x = abs (x2 - x1) + 1
        y = abs (y2 - y1) + 1

diagonalLineToList :: Line -> [(Int, Int)]
diagonalLineToList (Line (x1,y1) (x2,y2)) 
    | x1 < x2 && y1 < y2 = zip xs ys
    | x1 < x2 = zip xs (reverse ys)
    | y1 < y2 = zip (reverse xs) ys
    | otherwise = zip (reverse xs) (reverse ys)
    where
        xs = [min x1 x2 .. max x1 x2]
        ys = [min y1 y2 .. max y1 y2]


isDiagonal :: Line -> Bool
isDiagonal (Line (x1,y1) (x2,y2)) = x `div` y == 1 
    where
        x = abs (x2 - x1) 
        y = abs (y2 - y1) 

isOrthogonal :: Line -> Bool
isOrthogonal (Line (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2 

createLine :: [String] -> Line
createLine (s1:s2:[]) 
    | x1 < x2 || y1 < y2 = Line ( x1,y1) (x2,y2)
    | otherwise = Line (x2,y2) (x1,y1)
    where
        xy1 = (map read $ splitOn "," s1) 
        xy2 = (map read $ splitOn "," s2)
        (x1,y1) = (xy1!!0, xy1!!1)
        (x2,y2) = (xy2!!0, xy2!!1)
createLine _ = Line (0,0) (0,0)