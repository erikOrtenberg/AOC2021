module Main where

import System.Exit
import System.IO

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

main :: IO ()
main = do 
    putStrLn "Välj en dag lmao:"
    hFlush stdout
    x <- getLine
    case x of 
        "1" -> putStrLn "Day 1:" >> day1
        "2" -> putStrLn "Day 2: " >> day2
        "3" -> putStrLn "Day 3: " >> day3
        "4" -> putStrLn "Day 4: " >> day4
        "5" -> putStrLn "Day 5: " >> day5
        "6" -> putStrLn "Day 6: " >> day6
        "7" -> putStrLn "Day 7: " >> day7
        "8" -> putStrLn "Day 8: " >> day8
        "9" -> putStrLn "Day 9: " >> day9
        "10" -> putStrLn "Day 10: " >> day7
        "11" -> putStrLn "Day 11: " >> day11
        "12" -> putStrLn "Day 12: " >> day12
        "13" -> putStrLn "Day 13: " >> day13
        "14" -> putStrLn "Day 14: " >> day14
        "15" -> putStrLn "Day 15: " >> day15
        "16" -> putStrLn "Day 16: " >> day16
        "17" -> putStrLn "Day 17: " >> day17
        "18" -> putStrLn "Day 18: " >> day18
        "19" -> putStrLn "Day 19: " >> day19
        "20" -> putStrLn "Day 20: " >> day20
        "21" -> putStrLn "Day 21: " >> day21
        "22" -> putStrLn "Day 22: " >> day22
        "23" -> putStrLn "Day 23: " >> day23
        "24" -> putStrLn "Day 24: " >> day24
        "25" -> putStrLn "Day 25: " >> day25
        "q" -> putStrLn "okej hejdå :)" >> exitSuccess 
        _   -> putStrLn "Idiotjävel, skriv rätt"
    --main