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
import Data.PriorityQueue
import qualified Data.PriorityQueue as PQ

data PQentry = PQe (Int, Int) Int PQentry | End

instance Show PQentry where
    show End = ""
    show (PQe p wth pqe) = show p ++ " [" ++ show wth ++ "] " ++ show pqe

instance Eq PQentry where
    (==) :: PQentry -> PQentry -> Bool
    (==) (PQe p1 w1 pqe1) (PQe p2 w2 pqe2) = w1 == w2
    (==) _ _ = False

instance Ord PQentry where
    compare (PQe p1 w1 pqe1) (PQe p2 w2 pqe2) = compare w1 w2
    compare _ _ = EQ

day15 :: IO()
day15 = do
    ss <- getFile "day15.txt"
    print "wow"

stepPQ :: PriorityQueue PQentry -> (PQentry, PriorityQueue PQentry)  
stepPQ pq = do
    top <- PQ.dequeue pq
