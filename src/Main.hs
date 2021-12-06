module Main where


import System.IO  
import Control.Monad

import Day6.Exo2


main :: IO ()
main = do  
        let list = []
        handle <- openFile "src/Day6/input.txt" ReadMode
        contents <- hGetContents handle
        let response = solution contents
        print response
        hClose handle   
