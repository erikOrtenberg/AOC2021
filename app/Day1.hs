module Day1 where

day1 :: IO ()
day1 = do
    input <- readFile "../input/day1.txt"
    let s = map read (words input) :: [Integer]
    putStrLn "Day 1:"
    putStrLn $ show $ length $ filter (>0) (zipWith (\x y -> y - x) s (drop 1 s))
    putStrLn $ show $ length $ filter (>0) (zipWith (\x y -> y - x) s (drop 3 s))
