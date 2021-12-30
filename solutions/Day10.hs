module Day10 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

day10 :: IO()
day10 = do
    ss <- getFile "day10.txt"

    --part 1
    let reducedBrackets = map (\x -> head (reduceBracket x [])) (filter isCorrupt (lines ss))
    print $ sum $ zipWith (*) (map (count reducedBrackets) [')',']','}','>']) [3,57,1197,25137]
    
    --part 2
    
    let incompleteLines = map reduceLine (filter (not . isCorrupt) (lines ss))
    let completedBrackets = sort $ map (\line -> foldl (\x y -> x*5 + (getClosedVal y)) 0 line) (map autoComplete incompleteLines)
    --print incompleteLines
    print $ completedBrackets!!(length completedBrackets `div` 2) 

getClosedVal :: Char -> Int
getClosedVal c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _ -> 0

autoComplete :: String -> String
autoComplete s = map inverse $ reverse s

isCorrupt :: String -> Bool
isCorrupt s = (isCorrupt' . reduceLine) s

isCorrupt' :: String -> Bool
isCorrupt' (c1:c2:s) 
    | c2 == ']' || c2 == ')' || c2 == '}' || c2 == '>' = True
    | otherwise = isCorrupt' (c2:s) 
isCorrupt' xs = False


inverse :: Char -> Char
inverse c = case c of
    ']' -> '['
    ')' -> '('
    '}' -> '{'
    '>' -> '<'
    '[' -> ']'
    '(' -> ')' 
    '{' -> '}'
    '<' -> '>'
    _ -> ' '

reduceBracket :: String -> [Char] -> [Char]
reduceBracket [] chs = [' '] 
reduceBracket (c:s) chs 
    | c == '[' || c == '(' || c == '{' || c == '<' = reduceBracket s (c:chs)
    | otherwise = case c of
        ']' -> f '[' ']'
        ')' -> f '(' ')'
        '}' -> f '{' '}'
        '>' -> f '<' '>'
        _   -> reduceBracket s chs
    where
        f open closed
            | head chs /= open = closed : (reduceBracket s (tail chs))
            | otherwise = reduceBracket s (tail chs)

reduceLine :: String -> String
reduceLine [] = []
reduceLine (c:s) 
    | c == '[' || c == '(' || c == '{' || c == '<' = f c (reduceLine s)
    | otherwise = c : (reduceLine s)
    where
        f :: Char -> String -> String
        f open rest = case open of
            '[' -> g '[' ']' rest
            '(' -> g '(' ')' rest
            '{' -> g '{' '}' rest
            '<' -> g '<' '>' rest
        g :: Char -> Char -> String -> String
        g open closed [] = [open]
        g open closed rest
            | closed == head rest = tail rest
            | otherwise = open : rest

count :: Eq a => [a] -> a -> Int
count ls cmp = length $ filter (==cmp) ls