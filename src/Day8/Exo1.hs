module Day8.Exo1
  ( solution,
  )
where

import Utils

solution :: String -> Int
solution = length . filter (\l -> length l `elem` [2,3,4,7] ). concatMap (words . last . split "|") . split "\n"

