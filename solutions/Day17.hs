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
