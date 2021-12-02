module Day2.Exo2
    ( solution
    ) where

data Direction a = Forward a | Down a | Up a
data Position = Position Int Int Int

createDirection :: String -> Int -> Direction Int
createDirection "forward" x = Forward x
createDirection "down" x = Down x 
createDirection "up" x = Up x 

sumDirection :: Position -> Direction Int -> Position
sumDirection (Position x y aim)  (Forward z) = Position (x+z) (y+ aim*z) aim
sumDirection (Position x y aim) (Up z) = Position x y (aim-z)
sumDirection (Position x y aim) (Down z) = Position x y (aim+z)

mapInputToData :: [String] -> [Direction Int]
mapInputToData (direction:x:t) = createDirection direction (read x) : (mapInputToData t)
mapInputToData _ = []

multiplyPosition :: Position -> Int
multiplyPosition (Position x y aim) = x*y

solution :: String -> Int
solution = multiplyPosition . foldl sumDirection (Position 0 0 0) . mapInputToData. words
