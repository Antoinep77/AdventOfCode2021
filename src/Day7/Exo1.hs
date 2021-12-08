module Day7.Exo1
  ( solution,
  )
where

import Utils
import Data.List (sort)

solution :: String -> Int
solution =  sum
            . (\ (list,med) ->map (abs . (med -)) list)
            . (\(list,len) -> (list,list !! div len 2))
            . (\list -> (list,length list))
            . sort . map read . split ","

