module Day16 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char

type Version = String
type TypeID = String

data Packet = Packet Header PacketData

instance Show Packet where
    show (Packet h d) = "{" ++ show h ++ " Data: " ++ show d ++ "}"

data Header = Header Version TypeID

instance Show Header where
    show h = "Version: " ++ show (getVersion h) ++ " TypeID: " ++ show (getTypeID h)

data PacketData = Literal Int | SubPackets [Packet]

instance Show PacketData where
    show (Literal i) = show i
    show (SubPackets ps) = concatMap show ps

binToInt :: String -> Int
binToInt [] = 0
binToInt (c:chs) = read [c] * 2 ^ length chs + binToInt chs

getVersion :: Header -> Int
getVersion (Header v _) = binToInt v

getTypeID :: Header -> Int
getTypeID (Header _ t) = binToInt t

day16 :: IO()
day16 = do
    ss <- getFile "day16.txt"
    let bin = concatMap hexToBin ss
    let (packet, s) = createPacket bin   
    --part 1
    print $ sumVersionNumber packet
    --part 2
    print $ applyOps packet

applyOps :: Packet -> Int
applyOps (Packet _ (Literal x)) = x
applyOps (Packet h (SubPackets ps))
    | idelement == -1 = op (applyOps (ps!!0)) (applyOps (ps!!1))
    | otherwise = foldl op idelement $ map applyOps ps
    where
        (op, idelement) = typeIDToOp h
sumVersionNumber :: Packet -> Int
sumVersionNumber (Packet h (Literal _)) = getVersion h
sumVersionNumber (Packet h (SubPackets ps)) = getVersion h + sum (map sumVersionNumber ps)

createPacket :: String -> (Packet, String)
createPacket s
    | getTypeID header == 4 = (Packet header literal, restOfInputAfterLiteral)
    | otherwise = (Packet header subPackets, restOfInputAfterSubPackets)
    where
        (header, restOfInput1) = createHeader s
        (literal, restOfInputAfterLiteral) = createLiteral restOfInput1
        (subPackets, restOfInputAfterSubPackets) = createSubPackets restOfInput1

createHeader :: String -> (Header, String)
createHeader s = (Header (take 3 s) (take 3 $ drop 3 s), drop 6 s)

createLiteral :: String -> (PacketData, String)
createLiteral s = (Literal (binToInt extractedLiteral), drop (5 * length extractedLiteral `div` 4) s)
    where
        extractedLiteral = createLiteral' s

createLiteral' :: String -> String
createLiteral' [] = ""
createLiteral' (c:s)
    | c == '0' = take 4 s
    | otherwise = take 4 s ++ createLiteral' (drop 4 s)

createSubPackets :: String -> (PacketData, String)
createSubPackets [] = (SubPackets [], "")
createSubPackets (c:s)
    | c == '0' = (SubPackets ps1, s1)
    | otherwise  = (SubPackets ps2, s2)
    where
        lengthTypeID0 = binToInt (take 15 s)
        lengthTypeID1 = binToInt (take 11 s)
        (ps1, s1) = createSubPacketsTypeID0 (drop 15 s) lengthTypeID0
        (ps2, s2) = createSubPacketsTypeID1 (drop 11 s) lengthTypeID1

createSubPacketsTypeID0 :: String -> Int -> ([Packet], String)
createSubPacketsTypeID0 s count
    | count <= 0 = ([], s)
    | diff <= count = (p:ps, lastS)
    | otherwise = ([], s)
    where
        diff = length s - length newS
        (p, newS) = createPacket s
        (ps, lastS) = createSubPacketsTypeID0 newS (count - diff)

createSubPacketsTypeID1 :: String -> Int -> ([Packet], String)
createSubPacketsTypeID1 s 0 = ([], s)
createSubPacketsTypeID1 s count = (p:ps, lastS)
    where
        (p, newS) = createPacket s
        (ps, lastS) = createSubPacketsTypeID1 newS (count-1)

hexToBin :: Char -> String
hexToBin ch = case ch of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"
    _ -> ""

typeIDToOp :: Header -> (Int -> Int -> Int, Int)
typeIDToOp header = case typeID of
    0 -> ((+), 0)
    1 -> ((*), 1)
    2 -> (min, 2^29 - 1)
    3 -> (max, -2^29)
    5 -> (\a b -> fromBool (a>b), -1)
    6 -> (\a b -> fromBool (a<b), -1)
    7 -> (\a b -> fromBool (a==b), -1)
    _ -> error "non-supported operation"
    where
        typeID = getTypeID header

fromBool :: Bool -> Int
fromBool b
    | b = 1
    | otherwise = 0