module Day19 where

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

type Vector = (Int, Int, Int)
type Point = (Int, Int, Int)
type Scanner = Set Point

instance (Num a, Num b, Num c) => Num (a,b,c)  where
    (x1,y1,z1) + (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
    (x1,y1,z1) - (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)
    (x1,y1,z1) * (x2,y2,z2) = (x1*x2,y1*y2,z1*z2)
    abs (x,y,z) = (abs x, abs y, abs z)

day19 :: IO()
day19 = do
    ss <- getFile "day19.txt"
    -- fuckery parsing, should be bog standard at this point, move on >:(
    let scanners = map (foldr S.insert S.empty) $ map (map ((\(x:y:z:xyzs) -> (x,y,z)) . map (\x -> read x :: Int) . splitOn ",") . tail . lines) $ splitOn "\n\n" ss :: [Scanner]

    --mapM_ (\p@(x,y) -> print $ show p ++ show (doScannersOverlap (scanners!!x) (scanners!!y))) [(x,y) | x <- [0..length scanners - 1], y <- [0..length scanners - 1], x /= y]

    let (scannerMap, scannerPoints) = sumScanners scanners [(0,0,0)]
    --part 1
    print $ S.size scannerMap

    --part 2
    print $ maximum $ [(\ (x, y)
        -> manhattanDistance (scannerPoints !! x) (scannerPoints !! y))
       (x, y) |
       x <- [0 .. length scannerPoints - 1],
       y <- [0 .. length scannerPoints - 1],
       x < y]


manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = (\(x,y,z) -> abs x + abs y + abs z) (p1-p2)

sumScanners :: [Scanner] -> [Point] -> (Scanner, [Point])
sumScanners (s0:s1:rest) scanPoints
    | S.size s0 /= S.size try = sumScanners (try:rest) (scanPoint:scanPoints)
    | otherwise = sumScanners (s0:rest ++ [s1]) scanPoints
    where
        (try, scanPoint) = addScanners s0 s1

sumScanners s scanPoints = (head s, scanPoints)

addScanners :: Scanner -> Scanner -> (Scanner, Point)
addScanners s1 s2
    | null overlap = (s1, (0,0,0))
    | otherwise = addScanners' s1 (head overlap)
    where
        overlap = getScannerOverlap s1 s2

addScanners' :: Scanner -> (Point, (Set Vector, Point)) -> (Scanner, Point)
addScanners' s1 (s1p,(vectors,s2p)) = (foldr (\x -> S.insert (s1p + x)) s1 $ S.elems vectors, s1p-s2p)

---

getScannerOverlap :: Scanner -> Scanner -> [(Point, (Set Vector, Point))]
getScannerOverlap s1 s2 = map (fromMaybe ((0,0,0), (S.empty, (0,0,0)))) overlap
    where
        overlap = filter isJust $ map (\x -> getScannerOverlap' s1 x s2) $ S.elems s1

getScannerOverlap' :: Scanner -> Point -> Scanner -> Maybe (Point, (Set Vector, Point))
getScannerOverlap' s1 p s2
    | not $ null testIntersections = Just (p, head testIntersections)
    | otherwise = Nothing
    where
        testIntersections = filter (\(vec,_) -> S.size (S.intersection refVectors vec) >= 11) testVectors
        refVectors = fst $ createVectors s1 p
        s2Rotations = getAllRotationPermutations s2
        testVectors = concatMap (\x -> map (createVectors x) (S.elems x)) s2Rotations :: [(Set Vector, Point)]

getAllRotationPermutations :: Scanner -> [Scanner]
getAllRotationPermutations vectors = concatMap rotationAlg facesUp
    where
        facesUp = (take 4 $ iterate rotateY vectors) ++ [rotateZ vectors] ++ [iterate rotateZ vectors !! 3]

rotationAlg :: Scanner -> [Scanner]
rotationAlg vectors = take 4 $ iterate rotateX vectors

rotateX :: Scanner -> Scanner
rotateX = S.map (\(x,y,z) -> (x,-z,y))

rotateY :: Scanner -> Scanner
rotateY = S.map (\(x,y,z) -> (z,y,-x))

rotateZ :: Scanner -> Scanner
rotateZ = S.map (\(x,y,z) -> (-y,x,z))

createVectors :: Scanner -> Point -> (Set Vector, Point)
createVectors s p = (foldr (\x -> S.insert (x - p)) S.empty rawPoints, p)
    where
        rawPoints = S.elems $ S.delete p s