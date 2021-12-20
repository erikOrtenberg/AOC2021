module Day13 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S 
import Data.Char

data Instruction = I Direction Int
    deriving (Eq, Show)

data Direction = U | L
    deriving (Eq, Show)

day13 :: IO()
day13 = do
    ss <- getFile "day13.txt"
    let (s1, s2) = toZip $ map lines $ splitOn "\n\n" ss
    let coords = foldr (\x -> S.insert (x!!0, x!!1)) S.empty $ (map (\x -> map read x) $ map (splitOn ",") s1 :: [[Int]]) :: Set (Int, Int)
    let instructions = map (toInstruciton . (splitOn "=") . drop 11) s2
    --part 1
    print $ S.size $ foldSheet coords (head instructions)
    --part 2
    let endState = foldl (\x y -> foldSheet x y) coords instructions
    let (maxX, maxY) = (S.foldr (\x y -> max (fst x) y) 0 endState, S.foldr (\x y -> max (snd x) y) 0 endState)
    putStrLn $ ' ':(unwords $ map (\y -> (map (\x -> convertFromSet endState (x,y)) [0..maxX]) ++ "\n\r") [0 .. maxY])

convertFromSet :: Set (Int, Int) -> (Int, Int) -> Char
convertFromSet s p
    | S.member p s = '#'
    | otherwise = ' '

foldSheet :: Set (Int, Int) -> Instruction -> Set (Int, Int)
foldSheet m (I dir pos) 
    | dir == L = S.map fx m
    | otherwise = S.map fy m
    where
        fx (x,y) 
            | x > pos = (2 * pos - x, y)
            | otherwise = (x,y) 
        fy (x,y) 
            | y > pos = (x, 2 * pos - y)
            | otherwise = (x,y) 


toInstruciton :: [String] -> Instruction
toInstruciton (dir:pos:[])
    | dir == "x" = (I L (read pos))
    | otherwise = (I U (read pos))

toZip :: [[a]] -> ([a],[a])
toZip ls = (ls!!0, ls!!1)