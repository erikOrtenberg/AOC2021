module Day18 where

import GetInput (getFile)
import Data.List
import Data.List.Split (chunksOf, splitOn, wordsBy)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char
import Text.Read (readMaybe)

data SnailNumber = SN SnailNumber SnailNumber Int | Literal Int Int | Empty
    deriving Eq

instance Show SnailNumber where
    show (SN sn1 sn2 x) = "[" ++ show sn1 ++ "," ++ show sn2 ++"]"
    show (Literal val _) = show val
    --show (Literal val index) = "(" ++ show val ++ "," ++ show index ++ ")"
    show Empty = "empty"

(¤) :: SnailNumber -> SnailNumber -> SnailNumber
(¤) sn1@(SN _ _ d1) sn2@(SN _ _ d2) = reduceSnailNumber $ insertIndicies $ insertDepth (SN sn1 sn2 (-1))
(¤) _ _  = Empty

magnitude :: SnailNumber -> Int
magnitude (Literal val _) = val
magnitude (SN sn1 sn2 _) = 3 * magnitude sn1 + 2 * magnitude sn2
magnitude _ = 0

reduceSnailNumber :: SnailNumber -> SnailNumber
reduceSnailNumber sn
    | isJust (findExplodingNode sn) = reduceSnailNumber $ explode sn
    | isJust (findSplittingNode sn) = reduceSnailNumber $ split sn
    | otherwise = sn

explode :: SnailNumber -> SnailNumber
explode sn = case explodingNode of
    (SN (Literal val1 index1) (Literal val2 index2) d) -> insertIndicies $ insertDepth $ removeExplodedNode (addToLiteralAt (addToLiteralAt sn (index1-1) val1) (index2+1) val2) (index1,index2)
    _ -> sn
    where
        explodingNode = fromMaybe Empty $ findExplodingNode sn

split :: SnailNumber -> SnailNumber
split sn = case splittingNode of
    (Literal val1 index1) -> insertIndicies $ insertDepth $ splitNode sn index1
    _ -> sn
    where
        splittingNode = fromMaybe Empty $ findSplittingNode sn

splitNode :: SnailNumber -> Int -> SnailNumber
splitNode l@(Literal val index) searchIndex
    | index == searchIndex = SN (Literal (val `div` 2) (-1)) (Literal (ceiling $ fromIntegral val / 2) (-1)) (-1)
    | otherwise = l
splitNode (SN sn1 sn2 d) p = SN (splitNode sn1 p) (splitNode sn2 p) d
splitNode sn _ = sn

removeExplodedNode :: SnailNumber -> (Int, Int) -> SnailNumber
removeExplodedNode sn@(SN (Literal _ index1) (Literal _ index2) d) (searchIndex1, searchIndex2)
    | index1 == searchIndex1 && index2 == searchIndex2 = Literal 0 (-1)
    | otherwise = sn
removeExplodedNode (SN sn1 sn2 d) p = SN (removeExplodedNode sn1 p) (removeExplodedNode sn2 p) d
removeExplodedNode sn _ = sn

numberOfLiterals :: SnailNumber -> Int
numberOfLiterals (Literal _ _) = 1
numberOfLiterals (SN sn1 sn2 _) = numberOfLiterals sn1 + numberOfLiterals sn2
numberOfLiterals _ = 0

addToLiteralAt :: SnailNumber -> Int -> Int -> SnailNumber
addToLiteralAt l@(Literal x i) index val
    | i == index = Literal (x+val) i
    | otherwise = l
addToLiteralAt (SN sn1 sn2 d) index val = SN newSn1 newSn2 d
    where
        newSn1 = addToLiteralAt sn1 index val
        newSn2 = addToLiteralAt sn2 index val
addToLiteralAt _ _ _ = Empty

findExplodingNode :: SnailNumber -> Maybe SnailNumber
findExplodingNode sn@(SN sn1 sn2 depth)
    | isExplodable sn = Just sn
    | otherwise =
        case findExplodingNode sn1 of
            Just explo1@(SN (Literal v1 i1) (Literal v2 i2) d) -> Just explo1
            _ -> case findExplodingNode sn2 of
                    Just explo2@(SN (Literal v1 i1) (Literal v2 i2) d) -> Just explo2
                    _ -> Nothing
findExplodingNode sn = Just Empty

findSplittingNode :: SnailNumber -> Maybe SnailNumber
findSplittingNode sn@(SN sn1 sn2 depth)
    | isJust left = left
    | otherwise  = findSplittingNode sn2
    where 
        left = findSplittingNode sn1
findSplittingNode l@(Literal val index) 
    | isSplittable l = Just l
    | otherwise = Nothing
findSplittingNode _ = Nothing 

isSplittable :: SnailNumber -> Bool
isSplittable (Literal val index)
    | val >= 10 = True
isSplittable sn = False

isExplodable :: SnailNumber -> Bool
isExplodable (SN (Literal _ _) (Literal _ _) depth)
    | depth >= 4 = True
isExplodable sn = False


day18 :: IO()
day18 = do
    ss <- getFile "day18.txt"
    let snailNumbers = map createSnailNumber $ lines ss
    --part 1
    print $ magnitude $ foldl1 (¤) snailNumbers
    --part 2
    print $ findMaxMagnitude snailNumbers

findMaxMagnitude :: [SnailNumber] -> Int
findMaxMagnitude [] = 0
findMaxMagnitude (sn:[]) = 0
findMaxMagnitude (sn:sns) = maximum $ (map (\x -> max (magnitude (sn ¤ x)) (magnitude (x ¤ sn))) sns) ++ [(findMaxMagnitude sns)]

-- BEWAVRE! BELOW LIES FAFF (parsing)
createSnailNumber :: String -> SnailNumber
createSnailNumber s = insertIndicies $ createSnailNumber' s 0

createSnailNumber' :: String -> Int -> SnailNumber
createSnailNumber' s depth
    | isJust (readMaybe s :: Maybe Int) = Literal (read s) (-1)
    | otherwise = SN (createSnailNumber' left (depth + 1)) (createSnailNumber' right (depth + 1)) depth
    where
        (left, right) = extractTuple s

insertDepth :: SnailNumber -> SnailNumber
insertDepth sn = insertDepth' sn 0

insertDepth' :: SnailNumber -> Int -> SnailNumber
insertDepth' (SN sn1 sn2 _) depth = SN newSn1 newSn2 depth
    where
        newSn1 = insertDepth' sn1 (depth+1)
        newSn2 = insertDepth' sn2 (depth+1)
insertDepth' sn depth = sn


insertIndicies :: SnailNumber -> SnailNumber
insertIndicies sn = newSn
    where
        (newSn, _) = insertIndicies' sn 0

insertIndicies' :: SnailNumber -> Int -> (SnailNumber, Int)
insertIndicies' (Literal x _) index = (Literal x index, index + 1)
insertIndicies' (SN sn1 sn2 d) index = (SN newSn1 newSn2 d, nextIndex2)
    where
        (newSn1, nextIndex1) = insertIndicies' sn1 index
        (newSn2, nextIndex2) = insertIndicies' sn2 nextIndex1
insertIndicies' _ _ = (Empty, -1)


extractTuple :: String -> (String, String)
extractTuple [] = ("", "")
extractTuple (c:s) = (first, second)
    where
        (first, rest) = extractChunk ("", s) 0
        (second, _) = extractChunk ("", tail rest) 0

extractChunk :: (String, String) -> Int -> (String, String)
extractChunk (build, []) _ = (build, [])
extractChunk (build, c:rest) nrOfBrackets
    | nrOfBrackets == 0 && length build /= 0 && not (isDigit c) = (build, c:rest)
    | otherwise = case c of
                  '[' -> extractChunk (build ++ [c], rest) (nrOfBrackets + 1)
                  ']' -> extractChunk (build ++ [c], rest) (nrOfBrackets - 1)
                  _ -> extractChunk (build ++ [c], rest) nrOfBrackets