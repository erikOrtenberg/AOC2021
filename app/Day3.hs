module Day3 where

import GetInput (getFile)
import Data.List

day3 :: IO ()
day3 = do
    ss <- getFile "day3.txt"
    let xs = map (compareColumn 0) $ transpose . words $ ss
    putStrLn $ show $ (binToDec xs) * (binToDec (bitwiseInverse xs)) 
    let ys = words ss
    putStrLn $ show $ (binToDec (f 0 1 ys)) * (binToDec (f 0 0 ys))

f :: Int -> Integer -> [String] -> String
f _ _ (s:[]) = s
f i choice ss = f (i+1) choice filtered
    where
        filtered = filterBitMask i choice ss

filterBitMask :: Int -> Integer -> [String] -> [String]
filterBitMask i choice ss = filter (\chs -> chs!!i == compareColumn choice (map (!!i) ss)) ss

binToDec :: [Char] -> Integer
binToDec [] = 0
binToDec (c:chs) = (read [c]) * 2 ^ (length chs) + binToDec chs

bitwiseInverse :: [Char] -> [Char]
bitwiseInverse [] = []
bitwiseInverse (c:chs) 
    | c == '1' = '0' : bitwiseInverse chs
    | otherwise = '1' : bitwiseInverse chs

compareColumn :: Integer -> [Char] -> Char
compareColumn 0 chs 
    | length zeros > length ones = '0'
    | otherwise = '1' 
    where
        ones = filter (=='1') chs 
        zeros = filter (=='0') chs
compareColumn 1 chs 
    | length zeros > length ones = '1'
    | otherwise = '0' 
    where
        ones = filter (=='1') chs 
        zeros = filter (=='0') chs