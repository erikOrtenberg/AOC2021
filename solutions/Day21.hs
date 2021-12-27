module Day21 where

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

type Player = Int 
type Score = Int
type Dice = (Int,Int)

day21 :: IO()
day21 = do
    ss <- readFile "../input/day21.txt"
    let input = lines ss
    print $ last $ last input
    let (player1, player2) = ((read [last $ head input],0), (read [last $ last input],0)) :: ((Player,Score),(Player,Score))
    let t@(p1,p2,d) = progressOneTurn (1,1) player1 player2
    print t
    print $ progressOneTurn d p1 p2
    --part 1
    print $ part1 (1,1) player1 player2
    print "wow"

cMod :: Int -> Int -> Int
cMod x y = ((x - 1) `mod` y) + 1

rollDice :: Dice -> Dice
rollDice (current, rolls) = ((current+1) `cMod` 100, rolls + 1)

rollDiracDice :: Dice -> Dice
rollDiracDice (current, rolls) = ((current+1) `cMod` 3, rolls + 1)

part1 :: Dice -> (Player, Score) -> (Player, Score) -> Int
part1 d p1 p2 
    | snd p1 >= 1000 = snd p2 * snd d
    | snd p2 >= 1000 = snd p1 * snd d
    | otherwise = part1 newD newP1 newP2
    where
        (newP1, newP2, newD) = progressOneTurn d p1 p2

progressOneTurn :: Dice -> (Player, Score) -> (Player, Score) -> ((Player, Score),(Player, Score), Dice)
progressOneTurn dice p1@(p1pos, p1score) p2@(p2pos, p2score) 
    | snd newP1 >= 1000 = (newP1, p2, diceThrows!!2)
    | snd newP2 >= 1000 = (newP1, newP2, diceThrows!!5)
    | otherwise = (newP1, newP2, diceThrows!!6)
    where 
        newP1 = ((p1pos + sum p1rolls) `cMod` 10, p1score + (p1pos + sum p1rolls) `cMod` 10)
        newP2 = ((p2pos + sum p2rolls) `cMod` 10, p2score + (p2pos + sum p2rolls) `cMod` 10)
        diceThrows = iterate rollDice dice
        p1rolls = map fst $ take 3 diceThrows
        p2rolls = map fst $ take 3 $ drop 3 diceThrows 