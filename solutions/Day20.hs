module Day20 where

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

type Point = (Int, Int)
type Image = Set Point
type Bounds = (Int, Int, Int, Int)

binToDec :: [Char] -> Int
binToDec [] = 0
binToDec (c:chs) = read [c] * 2 ^ length chs + binToDec chs


day20 :: IO()
day20 = do
    ss <- getFile "day20.txt"
    let input = lines ss
    let enhancement = V.generate (length (head input)) (head input !!) 
    let image = foldr S.insert S.empty $ [(x,y) | x <- [0.. length (input !! 2) - 1] , y <- [0.. length input - 3], drop 2 input !!y!!x == '#']
    putStrLn $ showImage image ++ "\n"

    --part 1
    print $ S.size $ runAlgXTimes enhancement False image 2

    --part 2
    print $ S.size $ runAlgXTimes enhancement False image 50

showImage :: Image -> String
showImage image = concatMap (\y -> [f image (x, y) | x <- [minX .. maxX]] ++ "\n" ) [y | y <- [minY .. maxY]]
    where
        f i p
            | S.member p i = '#'
            | otherwise  = '.'
        (minX,maxX,minY,maxY) = findBounds image

runAlgXTimes :: Vector Char -> Bool -> Image -> Int -> Image
runAlgXTimes _ _ image 0 = image
runAlgXTimes alg b image iterations = runAlgXTimes alg inverted newImage (iterations - 1)
    where
        bounds = findBounds image 
        (newImage, inverted) = applyImageAlg alg b image bounds


applyImageAlg :: Vector Char -> Bool -> Image -> Bounds -> (Image, Bool)
applyImageAlg alg inverted image bounds = (foldr (\p i -> insertIfAble i p (alg V.! getAlgIndex image inverted p bounds)) S.empty  [(x,y) | x <- [minX - 1 .. maxX + 1], y <- [minY - 1 .. maxY + 1]], invert inverted)
    where
        invert b
            | V.head alg == '#' && V.last alg == '.' = not b
            | V.head alg == '#' = True
            | otherwise = b
        points = [(x,y) | x <- [minX - 1 .. maxX + 1], y <- [minY - 1 .. maxY + 1]]
        (minX,maxX,minY,maxY) = bounds

findBounds :: Image -> (Int,Int,Int,Int)
findBounds image = (minimum xs, maximum xs, minimum ys, maximum ys)
    where
        (xs,ys) = unzip $ S.elems image

insertIfAble :: Image -> Point -> Char -> Image
insertIfAble i p c
    | c == '#' = S.insert p i
    | otherwise = i

getAlgIndex :: Image -> Bool -> Point -> Bounds -> Int
getAlgIndex image inverted (x,y) bounds = binToDec $ reverse $ foldr (\(dx, dy) s -> f image (x+dx,y+dy) : s) "" [(x,y) | y <- [1,0,-1], x <- [1,0,-1]]
    where
        f i p@(x,y)
            | x < minX || x > maxX || y < minY || y > maxY = if inverted then '1' else '0'
            | S.member p i = '1'
            | otherwise = '0'
            where
                (minX,maxX,minY,maxY) = bounds