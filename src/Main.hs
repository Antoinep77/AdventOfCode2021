module Main where


import System.IO  
import Control.Monad

import Day10.Exo2


main :: IO ()
main = do  
        let list = []
        handle <- openFile "src/Day10/input.txt" ReadMode
        contents <- hGetContents handle
        let response = solution contents
        print response
        hClose handle   
