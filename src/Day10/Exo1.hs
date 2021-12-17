module Day10.Exo1
  ( solution,
  )
where

import Utils


solution :: String -> Int
solution =  sum . map score . words

isOpening :: Char -> Bool
isOpening c = elem c ['(', '{', '[' , '<']


isClosingFor :: Char -> Char -> Bool
isClosingFor '(' ')' = True
isClosingFor '{' '}' = True
isClosingFor '[' ']' = True
isClosingFor '<' '>' = True
isClosingFor _ _ = False

charScore :: Char -> Int
charScore ')' = 3
charScore ']' = 57
charScore '}' = 1197
charScore '>' = 25137
charScore _ = 0



score :: String -> Int
score s = scoreWithPile s []
  where 
    scoreWithPile [] _ = 0
    scoreWithPile (c:t) []  | isOpening c = scoreWithPile t [c]
                            | otherwise = charScore c
    scoreWithPile (c:t) (cp:pile) | isOpening c = scoreWithPile t (c:cp:pile)
                                  | isClosingFor cp c = scoreWithPile t pile
                                  | otherwise = charScore c



