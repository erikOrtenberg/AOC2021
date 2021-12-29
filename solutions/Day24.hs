module Day24 where

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

getOneMonadNr :: (Int, Int, Bool) -> (Int, Bool)
getOneMonadNr (addToX, addToW, isDiv1)
    | isDiv1 = (addToW, True)
    | otherwise = (-addToX, False)

getMonadNr :: [(Int,Int,Bool)] -> Int -> [(Int,Int,Bool)]
getMonadNr [] _ = []
getMonadNr ((addToX, addToW, isDiv1):rest) entryNr = (a,entryNr,b) : getMonadNr rest (entryNr + 1)
    where
        (a,b) = getOneMonadNr (addToX, addToW, isDiv1)

extractStackPairs :: [(Int, Int, Bool)] -> [((Int,Int),(Int,Int))]
extractStackPairs stack  
    | length newStack < 2 = [newPair]
    | otherwise = newPair : extractStackPairs newStack
    where
        (newPair, newStack) = extractStackPair stack

extractStackPair :: [(Int, Int, Bool)] -> (((Int,Int),(Int,Int)), [(Int,Int,Bool)])
extractStackPair stack = (newPair, newStack)
    where
        newPair = findStackPair stack
        newStack = removeStackPair stack

findStackPair :: [(Int, Int, Bool)] -> ((Int,Int),(Int,Int))
findStackPair (a@(val1,nr1,b1):b@(val2,nr2,b2):rest) 
    | b1 && not b2 = ((val1,nr1),(val2,nr2))
    | otherwise = findStackPair (b:rest)
findStackPair _ = ((-1,-1),(-1,-1))

removeStackPair :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
removeStackPair (a@(val1,nr1,b1):b@(val2,nr2,b2):rest) 
    | b1 && not b2 = rest
    | otherwise = a : removeStackPair (b:rest) 
removeStackPair _ = []

normalizePairs :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
normalizePairs ((v1,i1),(v2,i2)) = ((v1 - min v1 v2,  i1), (v2 - min v1 v2,  i2))

day24 :: IO()
day24 = do
    ss <- getFile "day24.txt"
    let input = map (\xs -> (\ys -> (ys!!1, ys!!2, (ys!!0) == 1)) (map (read . drop 6) [xs!!3, xs!!4, xs!!14])) $ tail $ map lines $ splitOn "inp w\n" ss :: [(Int,Int,Bool)]

    --code to help find the pairs and make it easy to calculate the max/min of digits for every pair, please forgive me haskell overlords
    --part 1
    print $ map normalizePairs $ extractStackPairs $ getMonadNr input 1
    putStrLn "n1 = 8  <->  n14 = 9\nn2 = 9  <->  n13 = 8\nn3 = 9  <->  n4 = 1\nn5 = 3  <->  n12 = 9\nn6 = 9  <->  n9 = 2\nn8 = 9  <->  n7 = 4\nn10 = 9 <->  n11 = 3"
    putStrLn "89913949293989"
    --part 2
    putStrLn "n1 = 1  <->  n14 = 2\nn2 = 2  <->  n13 = 1\nn3 = 9  <->  n4 = 1\nn5 = 1  <->  n12 = 7\nn6 = 8  <->  n9 = 1\nn8 = 6  <->  n7 = 1\nn10 = 7 <->  n11 = 1"
    putStrLn "12911816171712"
    print "wow"
