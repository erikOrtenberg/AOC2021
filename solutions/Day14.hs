module Day14 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S 
import Data.Char

day14 :: IO()
day14 = do
    ss <- getFile "day14.txt"
    let (s1, s2) = toZip $ map lines (splitOn "\n\n" ss)
    let polymer = head s1
    let polymer' = foldr (\x -> M.insertWith (+) x 1) M.empty $ createPolymerPairs polymer
    let rules' = foldr (\x -> M.insert (x!!0) ([x!!0!!0, x!!1!!0], [x!!1!!0, x!!0!!1])) M.empty $ map (splitOn " -> ") s2 :: Map String (String, String)

    let counts = M.elems $ M.insertWith (+) (last polymer) 1 $ foldr (\(x,y) -> M.insertWith (+) (x!!0) y) M.empty $ M.toList $ (iterate (applyRules' rules') polymer')!!10
    print $ foldr (\(x,y) -> M.insertWith (+) (x!!0) y) M.empty $ M.toList $ (iterate (applyRules' rules') polymer')!!10
    --part 1
    print $ maximum counts - (minimum counts)
    --part 2
    let counts' = M.elems $ M.insertWith (+) (last polymer) 1 $ foldr (\(x,y) -> M.insertWith (+) (x!!0) y) M.empty $ M.toList $ (iterate (applyRules' rules') polymer')!!40
    print $ maximum counts' - (minimum counts')

createPolymerPairs :: String -> [String]
createPolymerPairs (c1:[]) = []
createPolymerPairs (c1:c2:ss) = [c1, c2] : createPolymerPairs (c2:ss)

applyRules' :: Map String (String, String) -> Map String Int -> Map String Int
applyRules' rules input = foldr (\x y -> foldr (\z -> M.insertWith (+) z (getChainPartAmount x)) y (getNewChainParts x)) M.empty $ M.keys input
    where
        getNewChainParts x = fromZip $ fromMaybe ("","") $ M.lookup x rules
        getChainPartAmount x = fromMaybe 0 $ M.lookup x input

fromZip :: (a,a) -> [a]
fromZip (x,y) = [x,y]

toZip :: [[a]] -> ([a],[a])
toZip ls = (ls!!0, ls!!1)