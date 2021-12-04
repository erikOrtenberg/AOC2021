module Day4 where

import GetInput (getFile)
import Data.List
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

data BingoPoint = Point Integer Bool 
    deriving (Show, Eq)

type BingoRow = [BingoPoint] 

type BingoBoard = [BingoRow] 

day4 :: IO ()
day4 = do
    ss <- getFile("day4.txt")
    let input = map read $ splitOn "," ((lines ss)!!0) :: [Integer]
    --putStrLn $ concat $ map (\x -> show x ++ " ") input
    let boards = parseInput $ map words $ tail $ lines ss
    --putStrLn $ concat $ map (\x -> show x ++ "\n") $ parseInput $ map words $ tail $ lines ss
    let (winningBoard, lastInput) = findFirstWinningBoard input boards
    --putStrLn $ show winningBoard
    --putStrLn $ show lastInput
    putStrLn $ show $ lastInput * sumOfUnmarkedPoints winningBoard
    let (lastWinningBoard, lastWinningInput) = findLastWinningBoard input boards
    putStrLn $ show $ lastWinningInput * sumOfUnmarkedPoints lastWinningBoard



parseInput :: [[String]] -> [BingoBoard]
parseInput [] = []
parseInput (s:ss) 
    | s == [] = parseInput ss
    | otherwise = (createBingoBoard (length board) (concat board)) : (parseInput rest)
    where   
        (board, rest) = break (==[]) (s:ss)

sumOfUnmarkedPoints :: BingoBoard -> Integer
sumOfUnmarkedPoints board = sum rows 
    where
        rows = map (\(Point val _) -> val) $ filter (\(Point _ state) -> not state) (concat board)

findLastWinningBoard :: [Integer] -> [BingoBoard] -> (BingoBoard, Integer)
findLastWinningBoard [] _ = ([[]], 0)
findLastWinningBoard (current:points) boards 
    | (length boards) == 1 && (length winningBoards) == 1 = (updatedBoards!!0, current)
    | otherwise = findLastWinningBoard points (filter (not . checkBoard) updatedBoards)
    where
        updatedBoards = map (markBoard current) boards 
        winningBoards = filter checkBoard updatedBoards

findFirstWinningBoard :: [Integer] -> [BingoBoard] -> (BingoBoard, Integer)
findFirstWinningBoard [] _ = ([[]], 0)
findFirstWinningBoard (current:points) boards 
    | length winningBoards > 0 = (winningBoards!!0, current)
    | otherwise = findFirstWinningBoard points updatedBoards 
    where
        updatedBoards = map (markBoard current) boards 
        winningBoards = filter checkBoard updatedBoards

checkBoard :: BingoBoard -> Bool
checkBoard board = 0 < (length $ filter checkRowColumn board) + (length $ filter checkRowColumn (transpose board))

checkRowColumn :: [BingoPoint] -> Bool
checkRowColumn points = 0 == (length $ filter (\(Point _ state) -> not state) points)

markBoard :: Integer -> BingoBoard -> BingoBoard
markBoard val board = map (markRow val) board 

markRow :: Integer -> BingoRow -> BingoRow
markRow val row = map (markPoint val) row 

markPoint :: Integer -> BingoPoint -> BingoPoint
markPoint val (Point compare state) 
    | val == compare = Point compare True
    | otherwise = Point compare state

createBingoBoard :: Int -> [String] -> BingoBoard
createBingoBoard size input = chunksOf size $ map (\x -> Point x False) $ map read input

createBingoPoint :: String -> BingoPoint
createBingoPoint point = Point (read point) False 
