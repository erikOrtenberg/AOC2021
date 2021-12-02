module Day1 where

day1 :: IO ()
day1 = do
    input <- readFile "../input/day1.txt"
    let s = map read (words input) :: [Integer]
    putStrLn $ show $ length $ filter (\x -> x>0) (zipWith (\x y -> y - x) s (tail s))
