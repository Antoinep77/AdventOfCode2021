module Day7.Exo2
  ( solution,
  )
where

import Utils
import Data.List (sort)

solution :: String -> Int
solution =  sum
            . map ( (\n ->div ((n+1)*n) 2) . abs )
            . (\ (list,med) -> map (med -) list)
            . (\(list,len) -> (list, div (sum list) len))
            . (\list -> (list,length list))
            . sort . map read . split ","

