module Utils
  ( split,
  )
where

split :: String -> String -> [String]
split sep str = filter (/= "") (splitWithAcc "" sep str)
  where
    splitWithAcc :: String -> String -> String -> [String]
    splitWithAcc acc sep "" = [reverse acc]
    splitWithAcc acc [x1, y1] (x : y : rest)
      | x == x1 && y == y1 = reverse acc : splitWithAcc "" [x1, y1] rest
      | otherwise = splitWithAcc (x : acc) [x1, y1] (y : rest)
    splitWithAcc acc [x1] (x : rest)
      | x == x1 = reverse acc : splitWithAcc "" [x1] rest
      | otherwise = splitWithAcc (x : acc) [x1] rest
    splitWithAcc acc sep (h : t) = splitWithAcc (h : acc) sep t