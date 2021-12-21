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

    --tests
    {-
    print $ createPacket "110100101111111000101000"
    print $ createPacket "00111000000000000110111101000101001010010001001000000000"
    print $ createPacket "11101110000000001101010000001100100000100011000001100000"
    print $ createPacket $ concatMap hexToBin "8A004A801A8002F478"
    print $ sumVersionNumber (fst (createPacket $ concatMap hexToBin "8A004A801A8002F478")) == 16
    print $ createPacket $ concatMap hexToBin "620080001611562C8802118E34"
    print $ sumVersionNumber (fst (createPacket $ concatMap hexToBin "620080001611562C8802118E34")) == 12
    print $ createPacket $ concatMap hexToBin "C0015000016115A2E0802F182340"
    print $ sumVersionNumber (fst (createPacket $ concatMap hexToBin "C0015000016115A2E0802F182340")) == 23
    print $ createPacket $ concatMap hexToBin "A0016C880162017C3686B18A3D4780"
    print $ sumVersionNumber (fst (createPacket $ concatMap hexToBin "A0016C880162017C3686B18A3D4780")) == 31
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "C200B40A82"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "C200B40A82") == 3 
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "04005AC33890"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "04005AC33890") == 54 
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "880086C3E88112"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "880086C3E88112") == 7 
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "CE00C43D881120"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "CE00C43D881120") == 9 
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "D8005AC2A8F0"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "D8005AC2A8F0") == 1 
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "F600BC2D8F"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "F600BC2D8F") == 0
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "9C005AC2F8F0"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "9C005AC2F8F0") == 0 
    print $ applyOps $ fst $ createPacket $ concatMap hexToBin "9C0141080250320F1802104A08"
    print $ applyOps (fst $ createPacket $ concatMap hexToBin "9C0141080250320F1802104A08") == 1
    -}

   
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