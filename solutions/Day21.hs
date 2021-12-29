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

instance (Num a, Num b) => Num (a,b)  where
    (x1,y1) + (x2,y2) = (x1+x2,y1+y2)

day21 :: IO()
day21 = do
    ss <- getFile "day21.txt"
    let input = lines ss
    let (player1, player2) = ((read [last $ head input],0), (read [last $ last input],0)) :: ((Player,Score),(Player,Score))
    let t@(p1,p2,d) = progressOneTurn (1,1) player1 player2
    --part 1
    print $ part1 (1,1) player1 player2
    --part 2
    print $ part2 player1 player2
    print "wow"

cMod :: Int -> Int -> Int
cMod x y = ((x - 1) `mod` y) + 1

rollDice :: Dice -> Dice
rollDice (current, rolls) = ((current+1) `cMod` 100, rolls + 1)

part2 :: (Player, Score) -> (Player, Score) -> Int
part2 p1 p2 = max p1Score p2Score
    where
        (p1Score, p2Score) = getParallellUnviverses p1 p2

getParallellUnviverses :: (Player, Score) -> (Player, Score) -> (Int, Int)
getParallellUnviverses p1 p2 = getParallellUnviverses' [(p1,p2,1)] (0,0)

getParallellUnviverses' :: [((Player, Score), (Player, Score), Int)] -> (Int, Int) -> (Int, Int)
getParallellUnviverses' boards wins
    | length boards == 0 = wins
    | otherwise = getParallellUnviverses' rBoards (wins + wSum)
    where
        newBoards = concatMap progressOneParallellUniverses boards
        wBoards = filter isWinning newBoards
        rBoards = filter (not . isWinning) newBoards
        wSum = foldr (\x y -> f x + y) (0,0) wBoards
        f (p1,p2,nrUni)
            | snd p1 >= 21 = (nrUni,0)
            | snd p2 >= 21 = (0,nrUni)
            | otherwise = (0,0)

isWinning :: ((Player, Score),(Player, Score), Int) -> Bool
isWinning (p1,p2,_) = snd p1 >= 21 || snd p2 >= 21

progressOneParallellUniverses :: ((Player, Score),(Player, Score), Int) -> [((Player, Score),(Player, Score), Int)]
progressOneParallellUniverses (p1, p2, nrUniverses) = p1Winning ++ remainingBoards
    where
        allMovePermsP1 = allMovePermutations p1
        p1Winning = map (\x -> (fst x,(0,0), snd x * nrUniverses)) $ filter (\x -> snd (fst x) >= 21) allMovePermsP1
        p1rest = filter (\x -> snd (fst x) < 21) allMovePermsP1
        allMovePermsP2 = allMovePermutations p2
        remainingBoards = [(fst x, fst y, snd x * snd y * nrUniverses) | x <- p1rest, y <- allMovePermsP2]


allMovePermutations :: (Player, Score) -> [((Player, Score), Int)]
allMovePermutations p1 = [threes, fours, fives, sixes, sevens, eights, nines]
    where
        threes = (f 3 p1, 1)
        fours = (f 4 p1, 3)
        fives = (f 5 p1, 6)
        sixes = (f 6 p1, 7)
        sevens = (f 7 p1, 6)
        eights = (f 8 p1, 3)
        nines = (f 9 p1, 1)
        f x p = ((fst p + x) `cMod` 10, snd p + (fst p + x) `cMod` 10)
{-
part2' :: Dice -> (Player, Score) -> (Player, Score) -> Map Player Int
part2' d p1 p2 
    | snd p1 >= 21 = M.singleton p1 1
    | snd p2 >= 21 = M.singleton p2 1
    | otherwise = map (M.unionsWith (+)) [part2' (1, d) ]
-}

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