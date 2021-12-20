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
import Data.PQueue.Prio.Max
import qualified Data.PQueue.Prio.Max as PQ

data PQentry = PQe (Int, Int) PQentry | End

instance Show PQentry where
    show End = ""
    show (PQe p pqe) = show p ++ " " ++ show pqe

day15 :: IO()
day15 = do
    ss <- getFile "day15.txt"
    print "wow"

stepPQ :: MaxPQueue Int PQentry -> (PQentry, MaxPQueue Int PQentry)  
stepPQ pq = (End ,pq)
