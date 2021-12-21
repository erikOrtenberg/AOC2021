module Day17 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char

data Probe = Probe Point Vel
    deriving Show
type Vel = (Int, Int)
type Point = (Int, Int)
type Target =  [[Int]]

day17 :: IO()
day17 = do
    ss <- getFile "day17.txt"
    let xys = map (map read .(head . tail) . map (splitOn "..") . splitOn "=") (splitOn ", " (drop 12 (init ss))) ::[[Int]]
    
    let (lowX, highX) = getXVelRange xys
    let (lowY, highY) = getYVelRange xys
    
    --part 1
    print $ (highY + 1) * highY `div` 2
    --part 2
    print $ S.size $ S.filter (\x -> probeHitTarget xys (Probe (0,0) x)) $ foldr S.insert S.empty ([(x,y) | x <- [lowX .. highX], y <- [lowY .. highY]])

    --tests
    --let input = "23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5 25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7 8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6 26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3 20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8  25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7 25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6 8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4  24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5 7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3 23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5 27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5 8,-2    27,-8   30,-5   24,-7"
    --let reference = foldr S.insert S.empty $ map ((\x -> (read (x!!0), read (x!!1))) . splitOn ",") (words input) :: Set (Int, Int)
    --print reference
    --print (lowX,highX)
    --print (lowY,highY)

testTarget = [[20,30],[-10,-5]] :: [[Int]]

isInTarget :: Probe -> Target -> Bool
isInTarget probe@(Probe (x,y) (vx,vy)) target = x >= xMin && x <= xMax && y >= yMin && y <= yMax
    where
        (xMin,xMax,yMin,yMax) = (target!!0!!0, target!!0!!1, target!!1!!0, target!!1!!1)

moveProbe :: Probe -> Probe
moveProbe (Probe (x,y) (vx,vy)) = Probe (x+vx, y+vy) (decXVel vx, vy-1)
    where
        decXVel x 
            | x < 0 = x + 1
            | x > 0 = x - 1
            | otherwise = x

probeHitTarget :: Target -> Probe -> Bool 
probeHitTarget target probe@(Probe (x,y) (vx,vy))
    | isInTarget probe target = True 
    | x > xMax || y < yMin = False 
    | otherwise = probeHitTarget target (moveProbe probe)
    where
        (xMax, yMin) = (target!!0!!1, target!!1!!0)

getXVelRange :: Target -> (Int, Int)
getXVelRange target = (floor $ sqrt ((1/2)^2 + 2*fromIntegral xMin) , xMax)
    where
        (xMin,xMax) = (head (target!!0), head target!!1)

getYVelRange :: Target -> (Int, Int)
getYVelRange target = (yMin - 1 , -yMin - 1)
    where
        (yMin,yMax) = (head (target!!1), target!!1!!1)
