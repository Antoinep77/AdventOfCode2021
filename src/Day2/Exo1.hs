module Day2.Exo1
    ( solution
    ) where

data Direction a = Forward a | Down a | Up a
data Position = Position Int Int

createDirection :: String -> Int -> Direction Int
createDirection "forward" x = Forward x
createDirection "down" x = Down x 
createDirection "up" x = Up x 

sumDirection :: Position -> Direction Int -> Position
sumDirection (Position x y)  (Forward z) = Position (x+z) y
sumDirection (Position x y) (Up z) = Position x (y-z)
sumDirection (Position x y) (Down z) = Position x (y+z)

mapInputToData :: [String] -> [Direction Int]
mapInputToData (direction:x:t) = createDirection direction (read x) : (mapInputToData t)
mapInputToData _ = []

multiplyPosition :: Position -> Int
multiplyPosition (Position x y) = x*y

solution :: String -> Int
solution = multiplyPosition . foldl sumDirection (Position 0 0) . mapInputToData. words
