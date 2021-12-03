module Day3.Exo1
    ( solution
    ) where

import Data.Char  (digitToInt)


solution :: String -> Int
solution =  (\ [x,y] -> x*y ). map reversedBinaryToInt . getOppositeBinary . countstoBinary . reverse . countOccurences . words

countOccurences :: [String] -> [Int]
countOccurences (h:binaries) = foldl sumBinaryToCounts (map (const 0) h) $ h:binaries

sumBinaryToCounts :: [Int] -> String -> [Int]
sumBinaryToCounts = zipWith $ \ count char -> count - 1 + 2* digitToInt char

countstoBinary ::  [Int] -> [Int]
countstoBinary = map $ fromEnum . (>0)

getOppositeBinary :: [Int] ->[[Int]]
getOppositeBinary binary = [binary, map (1 -) binary]

reversedBinaryToInt :: [Int] -> Int      
reversedBinaryToInt = reversedBinaryToIntWithPow 1
    where             
        reversedBinaryToIntWithPow :: Int -> [Int] -> Int
        reversedBinaryToIntWithPow pow [] = 0
        reversedBinaryToIntWithPow pow (h : t) = h * pow + reversedBinaryToIntWithPow (2 * pow) t
