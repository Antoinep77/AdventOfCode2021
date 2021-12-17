module Day10.Exo2
  ( solution,
  )
where

import Utils
import Data.List


solution :: String -> Int
solution = median . filter (/=0) . map score . words

median :: [Int] -> Int
median = (\(list,len) -> list !! div len 2)
          . (\list -> (list,length list))
          . sort

isOpening :: Char -> Bool
isOpening c = elem c ['(', '{', '[' , '<']


isClosingFor :: Char -> Char -> Bool
isClosingFor '(' ')' = True
isClosingFor '{' '}' = True
isClosingFor '[' ']' = True
isClosingFor '<' '>' = True
isClosingFor _ _ = False

charScore :: Char -> Int
charScore '(' = 1
charScore '[' = 2
charScore '{' = 3
charScore '<' = 4
charScore _ = 0

reverseCompletingStringScore :: String -> Int
reverseCompletingStringScore [] = 0
reverseCompletingStringScore (c:t) = charScore c + 5 * reverseCompletingStringScore t


score :: String -> Int
score s = scoreWithPile s []
  where
    scoreWithPile [] pile = reverseCompletingStringScore (reverse pile)
    scoreWithPile (c:t) []  | isOpening c = scoreWithPile t [c]
                            | otherwise = 0
    scoreWithPile (c:t) (cp:pile) | isOpening c = scoreWithPile t (c:cp:pile)
                                  | isClosingFor cp c = scoreWithPile t pile
                                  | otherwise = 0


