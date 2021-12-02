module Main where

import System.Exit

import Day1
import Day2

main :: IO ()
main = do 
    putStrLn "Välj en dag lmao:"
    x <- getLine
    case x of 
        "1" -> day1
        "2" -> day2
        "q" -> putStrLn "okej hejdå :)" >> exitSuccess 
        _   -> putStrLn "Idiotjävel, skriv rätt"
    main