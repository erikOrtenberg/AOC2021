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


xor :: Bool -> Bool -> Bool
xor b1 b2 = (b1 || b2) && not (b1 && b2)


day19 :: IO()
day19 = do
    ss <- readFile "input/day19.txt"
    -- fuckery parsing, should be bog standard at this point, move on >:(
    let scanners = map (foldr S.insert S.empty) $ map (map ((\(x:y:z:xyzs) -> (x,y,z)) . map (\x -> read x :: Int) . splitOn ",") . tail . lines) $ splitOn "\n\n" ss :: [Scanner]

    --mapM_ (\p@(x,y) -> print $ show p ++ show (doScannersOverlap (scanners!!x) (scanners!!y))) [(x,y) | x <- [0..length scanners - 1], y <- [0..length scanners - 1], x /= y]
    --part 1
    print $ S.size $ sumScanners scanners


    print "wow"


--tempf s = concatMap (getAllDirectionPermutations . createVectors s) (S.elems s)

sumScanners :: [Scanner] -> Scanner
sumScanners (s0:s1:rest)
    | S.size s0 /= S.size try = sumScanners (try:rest)
    | otherwise = sumScanners (s0:rest ++ [s1])
    where
        try = addScanners s0 s1
sumScanners s = head s

addScanners :: Scanner -> Scanner -> Scanner
addScanners s1 s2
    | null overlap = s1
    | otherwise = addScanners' s1 (head overlap)
    where
        overlap = getScannerOverlap s1 s2


addScanners' :: Scanner -> (Point, Set Vector) -> Scanner
addScanners' s1 (p,vectors) = foldr (\x -> S.insert (p + x)) s1 $ S.elems vectors

---

doScannersOverlap :: Scanner -> Scanner -> Bool
doScannersOverlap s1 s2 = not (null (getScannerOverlap s1 s2))

getScannerOverlap :: Scanner -> Scanner -> [(Point, Set Vector)]
getScannerOverlap s1 s2 = map (fromMaybe ((0,0,0), S.empty)) overlap
    where
        overlap = filter isJust $ map (\x -> getScannerOverlap' s1 x s2) $ S.elems s1

getScannerOverlap' :: Scanner -> Point -> Scanner -> Maybe (Point, Set Vector)
getScannerOverlap' s1 p s2
    | not $ null testIntersections = Just (p, head testIntersections)
    | otherwise = Nothing
    where
        testIntersections = filter (\vec -> S.size (S.intersection refVectors vec) >= 11) allTestRotations
        refVectors = createVectors s1 p
        allTestRotations = concatMap getAllRotationPermutations testVectors
        testVectors = map (createVectors s2) $ S.elems s2 :: [Set Vector]

getAllRotationPermutations :: Set Vector -> [Set Vector]
getAllRotationPermutations vectors = concatMap rotationAlg facesUp
    where
        facesUp = (take 4 $ iterate rotateY vectors) ++ [rotateZ vectors] ++ [iterate rotateZ vectors !! 3] 

rotationAlg :: Set Vector -> [Set Vector]
rotationAlg vectors = take 4 $ iterate rotateX vectors

rotateX :: Set Vector -> Set Vector
rotateX = S.map (\(x,y,z) -> (x,-z,y))

rotateY :: Set Vector -> Set Vector
rotateY = S.map (\(x,y,z) -> (z,y,-x))

rotateZ ::Set Vector -> Set Vector
rotateZ = S.map (\(x,y,z) -> (-y,x,z))


createVectors :: Scanner -> Point -> Set Vector
createVectors s p = foldr (\x -> S.insert (x - p)) S.empty rawPoints
    where
        rawPoints = S.elems $ S.delete p s