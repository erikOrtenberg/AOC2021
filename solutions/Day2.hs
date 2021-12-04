module Day2 where

import GetInput (getFile)

day2 :: IO ()
day2 = do
    ss <- getFile "day2.txt"
    let x =  map (getValue . words) (lines ss)
    putStrLn $ show $ (\(xs,ys) -> (sum xs) * (sum ys)) (unzip x)
    putStrLn $ show $ (\(xs,ys) -> sum xs) (unzip x) * (getDepth x 0)

getDepth :: [(Integer, Integer)] -> Integer -> Integer 
getDepth [] n = 0
getDepth ((0,y):xys) n = getDepth xys (n + y) 
getDepth ((x,0):xys) n = n*x + getDepth xys n 

getValue :: [String] -> (Integer, Integer)
getValue ("forward":ss) = (read (head  ss), 0)
getValue ("down":ss) = (0, read (head ss))
getValue ("up":ss) = (0, -(read (head ss)))
getValue _ = (0,0)
 