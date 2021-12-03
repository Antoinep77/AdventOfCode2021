module Day3.Exo2
  ( solution,
  )
where

import Data.Char (digitToInt)
import Data.Bits

data BitCriteria a = Least a | Most a


solution =  (\[x,y] -> x*y) . map ( reversedBinaryToInt . reverse . recursiveFilter) . (\x -> [Least x, Most x]) . toBinaries . words


toBinaries :: [String] -> [[Int]]
toBinaries = map $ map digitToInt

firstBitDifference :: [[Int]] -> Int
firstBitDifference = sum . map ( \(h:t) -> 2 * h - 1)


recursiveFilter :: BitCriteria [[Int]] -> [Int]
recursiveFilter (Most l) = recursiveFilterWithAcc False [] l
recursiveFilter (Least l) = recursiveFilterWithAcc True [] l

recursiveFilterWithAcc :: Bool -> [Int] -> [[Int]] -> [Int]
recursiveFilterWithAcc _ acc [lastBinary] = reverse acc ++ lastBinary
recursiveFilterWithAcc isLeast acc binaries = recursiveFilterWithAcc isLeast (firstBit:acc)
                                                    $ map tail
                                                    $ filter ( \(h : t) -> h == firstBit) 
                                                    binaries
                        where firstBit = fromEnum $ xor isLeast $ firstBitDifference binaries >= 0


reversedBinaryToInt :: [Int] -> Int
reversedBinaryToInt = reversedBinaryToIntWithPow 1
    where
        reversedBinaryToIntWithPow :: Int -> [Int] -> Int
        reversedBinaryToIntWithPow pow [] = 0
        reversedBinaryToIntWithPow pow (h : t) = h * pow + reversedBinaryToIntWithPow (2 * pow) t

