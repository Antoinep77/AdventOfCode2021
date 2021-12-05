{-# LANGUAGE TupleSections #-}
module Day5.Exo2
  ( solution,
  )
where

import Data.Maybe
import Utils

data Line = HorizontalLine ((Int,Int),(Int,Int)) | VerticalLine ((Int,Int),(Int,Int)) 
            | LeftDiagonalLine ((Int,Int),(Int,Int)) | RightDiagonalLine ((Int,Int),(Int,Int))
            | OtherLine

getLineCoords :: Line -> Maybe ((Int,Int),(Int,Int))
getLineCoords (HorizontalLine x) = Just x
getLineCoords (VerticalLine x) = Just x
getLineCoords (LeftDiagonalLine x) = Just x
getLineCoords (RightDiagonalLine x) = Just x
getLineCoords OtherLine = Nothing

solution :: String -> Int
solution = uncurry countOverlap . (\f x -> (f x, x)) maxCoordinates . parseInput


parseInput :: String -> [Line]
parseInput = map (toLine . map ( map read . split ",") . split "->") . split "\n"

toLine :: [[Int]] -> Line
toLine [[x1,y1],[x2,y2]]
  | x1 == x2 && y1 <= y2 = VerticalLine ((x1,y1),(x2,y2))
  | x1 == x2 = VerticalLine ((x2,y2),(x1,y1))
  | y1 == y2  && x1 <= x2 = HorizontalLine ((x1,y1),(x2,y2))
  | y1 == y2 = HorizontalLine ((x2,y2),(x1,y1))
  | x1 - x2 == y1 - y2 && x1 <= x2 = LeftDiagonalLine ((x1,y1),(x2,y2))
  | x1 - x2 == y1 - y2 = LeftDiagonalLine  ((x2,y2),(x1,y1))
  | x1 - x2 == y2 - y1 && x1 <= x2 = RightDiagonalLine ((x1,y1),(x2,y2))
  | x1 - x2 == y2 - y1 = RightDiagonalLine  ((x2,y2),(x1,y1))
  | otherwise = OtherLine

isInLine :: (Int,Int) -> Line -> Bool
isInLine (x,y) (HorizontalLine ((x1,y1),(x2,y2))) = y == y1 && x1 <= x && x <= x2
isInLine (x,y) (VerticalLine ((x1,y1),(x2,y2))) = x == x1 && y1 <= y && y <= y2
isInLine (x,y) (LeftDiagonalLine ((x1,y1),(x2,y2))) = x - x1 == y - y1 
                                                    && x1 <= x && x <= x2
                                                    && y1 <= y && y <= y2
isInLine (x,y) (RightDiagonalLine ((x1,y1),(x2,y2))) = x - x1 == y1 - y 
                                                    && x1 <= x && x <= x2
                                                    && y2 <= y && y <= y1
isInLine _ _ = False

maxCoordinates :: [Line] -> (Int,Int)
maxCoordinates [] = (0,0)
maxCoordinates (OtherLine : t) = maxCoordinates t
maxCoordinates (l : t) = (maxX,maxY)
    where maxX = max (max x1 x2) recMaxX
          maxY = max (max y1 y2) recMaxY
          Just ((x1,y1),(x2,y2)) = getLineCoords l
          (recMaxX,recMaxY) = maxCoordinates t

countOverlap :: (Int,Int) -> [Line] -> Int
countOverlap (xMax,yMax) = countOverlapGrid $ concatMap (\x -> map (x,) [0..yMax])  [0..xMax]
  where
    countOverlapGrid :: [(Int,Int)] ->  [Line] -> Int
    countOverlapGrid [] _ = 0
    countOverlapGrid (coord:t) lines = fromEnum isOverlap + countOverlapGrid t lines
      where isOverlap = sum (map (fromEnum . isInLine coord) lines) > 1
