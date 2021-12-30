module Day22 where

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

type Point = (Int,Int,Int)
type Range = (Int,Int)
type Cuboid = (Range,Range,Range)
type Sign = Int

data CubeStatus = On | Off
    deriving (Show, Eq)

pCubeStatus :: String -> CubeStatus
pCubeStatus s
    | take 2 s == "on" = On
    | otherwise = Off

pLine :: String -> (Cuboid, CubeStatus)
pLine s = ((splitRanges!!0, splitRanges!!1, splitRanges!!2), pCubeStatus s)
    where
        cutInput = last $ splitOn " " s
        splitRanges = map ((\(x:y:xs) -> (x,y)) . map read . splitOn ".." . drop 2) $ splitOn "," cutInput :: [Range]

createIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
createIntersection (xs1,ys1,zs1) (xs2,ys2,zs2)
    | uncurry (>) newXs || uncurry (>) newYs || uncurry (>) newZs = Nothing
    | otherwise = Just (newXs,newYs,newZs)
    where
        (minX1,maxX1) = xs1
        (minY1,maxY1) = ys1
        (minZ1,maxZ1) = zs1
        (minX2,maxX2) = xs2
        (minY2,maxY2) = ys2
        (minZ2,maxZ2) = zs2
        newXs = (max (fst xs1) (fst xs2), min (snd xs1) (snd xs2))
        newYs = (max (fst ys1) (fst ys2), min (snd ys1) (snd ys2))
        newZs = (max (fst zs1) (fst zs2), min (snd zs1) (snd zs2))

getIntersection :: (Cuboid, Sign) -> (Cuboid, Sign) -> [(Cuboid, Sign)]
getIntersection (c1,s1) (c2,s2)
    | isJust i = [(fromJust i, -s2)]
    | otherwise = []
    where
    i = createIntersection c1 c2

sumCuboids :: [(Cuboid, CubeStatus)] -> [(Cuboid, Sign)]
sumCuboids input = foldl (\xs (c,s) -> xs ++ concatMap (getIntersection (c, f s)) xs ++ g (c, f s)) [] input
    where
        --start = (fst first, f $ snd first)
        f c
            | c == On = 1
            | otherwise = -1
        g (c,s)
            | s < 0 = []
            | otherwise = [(c,s)]

getCuboidVolume :: (Cuboid, Sign) -> Int
getCuboidVolume ((xs,ys,zs),s) = (maxX-minX+1) * (maxY-minY+1) * (maxZ-minZ+1) * s
    where
        (minX,maxX) = xs
        (minY,maxY) = ys
        (minZ,maxZ) = zs

part1 :: [(Cuboid, CubeStatus)] -> Int
part1 input = sum $ map getCuboidVolume $ sumCuboids (take 20 input)

part2 :: [(Cuboid,CubeStatus)] -> Int
part2 input = sum $ map getCuboidVolume $ sumCuboids input

day22 :: IO()
day22 = do
    ss <- getFile "day22.txt"
    let ranges = map pLine $ lines ss

    --part 1
    print $ part1 ranges

    --part 2
    print $ part2 ranges