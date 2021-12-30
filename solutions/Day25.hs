module Day25 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn, wordsBy)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char
import Text.Read (readMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Direction = East | South | Empty
    deriving (Show, Eq)

type Herd = Map (Int,Int) Direction

printHerd :: (Int,Int) -> Herd -> IO ()
printHerd (maxX,maxY) herd = putStrLn $ concatMap (\y -> [f herd (x, y) | x <- [0 .. maxX]] ++ "\n" ) [y | y <- [0 .. maxY]]
    where
        f h p = case M.findWithDefault Empty p herd of
          East -> '>' 
          South -> 'v'
          Empty -> '.'


getDirection :: Char -> Direction
getDirection c = case c of
    '>' -> East
    'v' -> South
    '.' -> Empty
    _ -> error "not a supported direction"

pInput :: String -> (Herd,Int,Int)
pInput s = (foldr (\(c,d) -> M.insert c (getDirection d)) M.empty onlyDirectionInput, length (head input) - 1,length input - 1)
    where
        onlyDirectionInput = inputWithCoords
        inputWithCoords = [((x, y), input !! y !! x)|
           x <- [0 .. length (head input) - 1], y <- [0 .. length input - 1]]
        input = lines s

findTurnsTillStop :: (Int,Int) -> Herd -> Int
findTurnsTillStop p herd = findTurnsTillStop' p herd 1


findTurnsTillStop' :: (Int,Int) -> Herd -> Int -> Int
findTurnsTillStop' (x,y) herd turns
    | steps == 0 = turns
    | otherwise = findTurnsTillStop' (x,y) movedHerd (turns + 1)
    where
        (movedHerd,steps) = moveAllSeaCucumbers (x,y) herd

moveAllSeaCucumbers :: (Int,Int) -> Herd -> (Herd,Int)
moveAllSeaCucumbers (maxX,maxY) herd = foldl (\x y -> moveSeaCucumber (maxX,maxY) South (fst movedEast) x y ) movedEast allEmptySlotsNew
    where
        movedEast = foldl (\x y -> moveSeaCucumber (maxX,maxY) East herd x y ) (herd,0) allEmptySlots
        allEmptySlots = M.keys $ M.filter (==Empty) herd
        allEmptySlotsNew = M.keys $ M.filter (==Empty) (fst movedEast)

moveSeaCucumber :: (Int,Int) -> Direction -> Herd -> (Herd, Int) -> (Int,Int) -> (Herd,Int)
moveSeaCucumber (maxX,maxY) dir ref (herd,steps) (x,y) 
    | current /= Empty = (herd, steps)
    | left == East && dir == East = (M.insert ((x+maxX) `mod` (maxX+1),y) Empty (M.insert (x,y) East  herd), steps + 1) 
    | up == South && dir == South = (M.insert (x,(y+maxY) `mod` (maxY+1)) Empty (M.insert (x,y) South herd), steps + 1 )
    | otherwise = (herd, steps)
    where
        current = M.findWithDefault Empty (x,y) herd
        left = M.findWithDefault Empty ((x+maxX) `mod` (maxX+1),y) ref
        up = M.findWithDefault Empty (x,(y+maxY) `mod` (maxY+1)) ref

day25 :: IO()
day25 = do
    ss <- getFile "day25.txt"
    let (input,x,y) = pInput ss
    --printHerd input
    print $ findTurnsTillStop (x,y) input
    --print $ findTurnsTillStop input