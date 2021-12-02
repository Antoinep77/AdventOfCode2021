module Day1.Exo2
    ( solution
    ) where
        

solution :: String -> Int
solution = foldl (+) 0 . map fromEnum . mapIncreased . fmap read . words

mapIncreased :: [Int] -> [Bool]
mapIncreased (h1:h2:h3:h4:t) =(h1+h2+h3 < h2+h3+h4) :  (mapIncreased (h2:h3:h4:t))
mapIncreased _ = []
