module Day1.Exo1
    ( solution
    ) where
        
solution :: String -> Int
solution = foldl (+) 0 . map fromEnum . mapIncreased . fmap read . words

mapIncreased :: [Int] -> [Bool]
mapIncreased (h1:h2:t) =(h1 < h2) :  (mapIncreased (h2:t))
mapIncreased _ = []
