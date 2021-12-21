module Day15 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char

import Data.PQueue.Prio.Min ( MinPQueue )
import qualified Data.PQueue.Prio.Min as PQ

type Node = (Int, Int)

type Graph = Map (Int, Int) Int

data PQentry = PQe Node PQentry | End

instance Show PQentry where
    show End = ""
    show (PQe p pqe) = show p ++ " " ++ show pqe

data Edge = Edge Node Node Int

instance Show Edge where
    show (Edge n1 n2 w) = show n1 ++ " and " ++ show n2 ++ " [" ++ show w ++ "]"

isInEdge :: Edge -> Node -> Bool
isInEdge (Edge n1 n2 _) node = n1 == node || n2 == node

normalizeEdge15 :: Edge -> Node -> Edge
normalizeEdge15 e@(Edge n1 n2 w) node
    | n1 == node = e
    | otherwise = Edge n2 n1 w

testPQe = PQe (0,0) (PQe (0,1) End) 

day15 :: IO()
day15 = do
    ss <- getFile "day15.txt"
    let graph = foldr (\x -> M.insert x (map (map read . tail . splitOn "") (lines ss)!!snd x!!fst x)) M.empty [(x,y) | x <- [0.. (length $ head $ lines ss) - 1  ], y <- [0.. (length $ lines ss) - 1] ] :: Map (Int, Int) Int
    let t xIter yIter = foldr (\x@(kx,ky) -> M.insert (kx + xIter *  isqrt (M.size graph), ky + yIter *  isqrt (M.size graph)) (((fromMaybe 0 (M.lookup x graph)) + xIter + yIter - 1) `mod` 9 + 1)) graph [(x,y) | x <- [0.. (length $ head $ lines ss) - 1  ], y <- [0.. (length $ lines ss) - 1] ] :: Map (Int, Int) Int
    let bigGraph = foldr M.union graph $ map (\(xIter, yIter) -> t xIter yIter) [(x,y) | x <- [0..4], y <- [0..4]]
    
    let (s, pq) = (S.empty, (PQ.insert 0 (PQe (0,0) End) PQ.empty)) 
    let (s1, pq1) = stepPath graph s pq 

    --part 1
    print $ findPath graph S.empty (0,0) (99,99) (PQ.insert 0 (PQe (0,0) End) PQ.empty)
    --part 2
    print $ findPath bigGraph S.empty (0,0) (499,499) (PQ.insert 0 (PQe (0,0) End) PQ.empty)


runXSteps :: Graph -> Set (Int, Int) -> MinPQueue Int PQentry -> Int -> (Set (Int, Int), MinPQueue Int PQentry)
runXSteps _ _ pq 0 = (S.empty, pq)
runXSteps graph s pq n = runXSteps graph newS newPq (n-1)
    where
        (newS, newPq) = stepPath graph s pq

findPath:: Graph -> Set (Int, Int) -> Node -> Node -> MinPQueue Int PQentry -> (Int, PQentry)
findPath graph visited start goal pq
    | PQ.size pq == 0 = (0, PQe (-1,-1) End)
    | node == goal = (costToHere, PQe node path)
    | otherwise = findPath graph updatedVisited start goal updatedPq
    where
        (costToHere, PQe node path) = PQ.findMin pq
        (updatedVisited, updatedPq) = stepPath graph visited pq

stepPath :: Graph -> Set (Int, Int) -> MinPQueue Int PQentry -> (Set (Int, Int), MinPQueue Int PQentry)
stepPath graph visited pq 
    | PQ.size pq == 0 = (visited, PQ.empty) 
    | S.member node visited = (visited, newPq)
    | otherwise = (updatedVisited, generateNewPaths graph updatedVisited newPq (costToHere, PQe node path))
    where
        ((costToHere, PQe node path), newPq) = PQ.deleteFindMin pq
        updatedVisited = node `S.insert` visited

generateNewPaths :: Graph -> Set (Int, Int) -> MinPQueue Int PQentry -> (Int, PQentry) -> MinPQueue Int PQentry
generateNewPaths _ _ _ (_, End) = PQ.empty 
generateNewPaths graph visited pq (costToHere, path@(PQe node p)) = foldr (\(nextNode, weight) -> PQ.insert (costToHere + weight) (PQe nextNode path)) pq newNodes
    where
        newNodes = filter (\x -> not $  S.member (fst x) visited) $ getOutgoingEdges graph node

getOutgoingEdges :: Graph -> Node -> [(Node, Int)]
getOutgoingEdges graph node = map (\x -> (x, fromMaybe (2^29 - 1) (M.lookup x graph))) moveableCoords
    where
        xpos = fst node
        ypos = snd node
        potential = [(xpos + x, ypos + y) :: Node | x <- [-1,0,1], y <- [-1,0,1], x /= y, abs x + abs y /= 2]
        moveableCoords = filter (\(x,y) -> and [x >= 0, x < isqrt (M.size graph), y >= 0, y < isqrt (M.size graph)]) potential
     
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral