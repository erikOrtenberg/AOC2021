module Main where


main :: IO ()
main = do
    input <- readFile "../input.txt"
    let s = map read (words input) :: [Integer]
    --mapM_ putStrLn $ map show (filter (\x -> x>0) [y-x | x <- s, y <- (removeFirst s)])
    putStrLn (show $ length (filter (\x -> x>0) (zipWith difference s (removeFirst s))))
    

difference :: Integer -> Integer -> Integer
difference x y = y - x

removeFirst :: [Integer] -> [Integer]
removeFirst [] = []
removeFirst (x:xs) = xs