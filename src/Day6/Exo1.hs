module Day6.Exo1
  ( solution,
  )
where

import Utils

solution :: String -> Int
solution = length . foldl (.) id (map (const transformFish) [1..80]) . map read . split ","

transformFish :: [Int] -> [Int]
transformFish [] = []
transformFish (0:t) = 6:8:transformFish t
transformFish (n:t) = (n-1):transformFish t