module Day1.Exo1
  ( solution,
  )
where

solution :: String -> Int
solution = sum . map fromEnum . mapIncreased . map read . words

mapIncreased :: [Int] -> [Bool]
mapIncreased (h1 : h2 : t) = (h1 < h2) : mapIncreased (h2 : t)
mapIncreased _ = []
