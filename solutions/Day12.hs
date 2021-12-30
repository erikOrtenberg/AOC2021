module Day12 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char

type Graph = [[String]]
type Edge = [String]
data Path = Path [String] Bool
    deriving (Eq)

instance Show Path where
   show (Path path b) = show path

type Node = String

day12 :: IO()
day12 = do
    ss <- getFile "day12.txt"
    let graph = map (splitOn "-") (lines ss) 
    print graph
    print $ getEdges graph "start"  
    print $ length $ filter (\(Path x b) -> last x == "end") $ findPaths graph [(Path ["start"] True)]
    print $ length $ filter (\(Path x b) -> last x == "end") $ findPaths graph [(Path ["start"] False)]


findPaths :: Graph -> [Path] -> [Path]
findPaths g paths 
    | sum (map length newNodes) /= 0 = concatMap (findPaths g) $ zipWith generateNewPaths paths newNodes
    | otherwise = paths 
    where 
        newNodes = map (\(Path x b) -> filter (\y -> not $ pathContains (Path x b) y) $ map (!!1) (getEdges g (last x))) paths

generateNewPaths :: Path -> [Node] -> [Path]
generateNewPaths (Path path b) nodes 
    | last path /= "end" = map (\x -> (Path (path ++ [x]) (containsTwoLower (Path (path ++ [x]) b)))) nodes
    | otherwise = [(Path path b)]

containsTwoLower :: Path -> Bool
containsTwoLower (Path path b) 
    | b = b
    | otherwise = length (lowers \\ (nub lowers)) > 0 
    where
        lowers = filter (\x -> and $ map (not . isUpper) x) path

pathContains :: Path -> Node -> Bool
pathContains (Path path b) node
    | last path == "end" = True
    | node == "start" = True
    | isUp = False
    | not b = False 
    | otherwise = elem node path
    where
        isUp = and $ map isUpper node 

getEdges :: Graph -> Node -> [Edge]
getEdges g node = map (normalizeEdge node) $ filter (\x -> x!!0 == node || x!!1 == node) g

normalizeEdge :: Node -> Edge -> Edge
normalizeEdge node edge
    | edge!!0 == node = edge
    | otherwise = reverse edge