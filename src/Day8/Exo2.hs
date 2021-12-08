module Day8.Exo2
  ( solution,
  )
where

import Utils

solution =  sum . map getNumberFromLine . split "\n"

getNumberFromLine :: String -> Int
getNumberFromLine line = read $ map (valueBasedOnKnown val1 val2 val3) $ words secondPart
  where (val1,val2,val3) = find147 $ words firstPart
        [firstPart, secondPart] =  split "|" line

find147 :: [String] -> (String,String,String)
find147 [] = ("","","")
find147 (h:t) | length h == 2 = (h,prev2,prev3)
              | length h == 4 = (prev1,h,prev3)
              | length h == 3 = (prev1,prev2,h)
              | otherwise = (prev1,prev2,prev3)
              where (prev1,prev2,prev3) = find147 t

valueBasedOnKnown :: String -> String -> String -> String -> Char
valueBasedOnKnown known1 known4 known7 toFind  
  | length toFind == 2 = '1'
  | length toFind == 3 = '7'
  | length toFind == 4 = '4'
  | length toFind == 5 = if includes1 then '3' else if almostIncludes4 then '5' else '2'
  | length toFind == 6 = if includes4 then '9' else if includes1 then '0' else '6'
  | length toFind == 7 = '8'
  where includes1 = all (`elem` toFind) known1 
        includes4 = all  (`elem` toFind) known4 
        almostIncludes4 = length ( filter (`elem` toFind) known4 ) == 3
