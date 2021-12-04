module Main where

import System.Exit

import Day1
import Day2
import Day3

main :: IO ()
main = do 
    putStrLn "Välj en dag lmao:"
    x <- getLine
    case x of 
        "1" -> putStrLn "Day 1:" >> day1
        "2" -> putStrLn "Day 2: " >> day2
        "3" -> putStrLn "Day 3: " >> day3
        "4" -> putStrLn "Day 4: " >> day2
        "5" -> putStrLn "Day 5: " >> day2
        "6" -> putStrLn "Day 6: " >> day2
        "7" -> putStrLn "Day 7: " >> day2
        "q" -> putStrLn "okej hejdå :)" >> exitSuccess 
        _   -> putStrLn "Idiotjävel, skriv rätt"
    main