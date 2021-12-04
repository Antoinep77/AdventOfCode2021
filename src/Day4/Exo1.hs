module Day4.Exo1
  ( solution,
  )
where

import Data.Maybe

newtype Board a = Board [[a]]

instance Functor Board where
  fmap f (Board x) = Board $ fmap (fmap f) x

instance Show a => Show (Board a) where
  show (Board x) = "Board " ++ show x

solution :: String -> Int
solution = uncurry (*) . fmap computeBoardSum . uncurry getWinningBoard . parseInput

split :: String -> String -> [String]
split sep str = filter (/= "") (splitWithAcc "" sep str)
  where
    splitWithAcc :: String -> String -> String -> [String]
    splitWithAcc acc sep "" = [reverse acc]
    splitWithAcc acc [x1, y1] (x : y : rest)
      | x == x1 && y == y1 = reverse acc : splitWithAcc "" [x1, y1] (y : rest)
      | otherwise = splitWithAcc (x : acc) [x1, y1] (y : rest)
    splitWithAcc acc [x1] (x : rest)
      | x == x1 = reverse acc : splitWithAcc "" [x1] rest
      | otherwise = splitWithAcc (x : acc) [x1] rest
    splitWithAcc acc sep (h : t) = splitWithAcc (h : acc) sep t

parseInput :: String -> ([Int], [Board (Maybe Int)])
parseInput str = (values, boards)
  where
    boards = map (Board . map (map (Just . read) . words) . split "\n") . tail $ blocks
    values = map read . split "," . head $ blocks
    blocks = split "\n\n" str

computeBoardSum :: Maybe (Board (Maybe Int)) -> Int
computeBoardSum (Just (Board lines)) = sum $ map (sum . map maybeToInt) lines
  where
    maybeToInt :: Maybe Int -> Int
    maybeToInt Nothing = 0
    maybeToInt (Just x) = x

removeBoardValue :: Int -> Board (Maybe Int) -> Board (Maybe Int)
removeBoardValue val = fmap valueToNothing
  where
    valueToNothing (Just x) = if x == val then Nothing else Just x
    valueToNothing _ = Nothing

isLineWinning :: [Maybe Int] -> Bool
isLineWinning = all isNothing

getBoardLines :: Board (Maybe Int) -> [[Maybe Int]]
getBoardLines (Board x) = x ++ getBoardVerticalLines x
  where
    getBoardVerticalLines :: [[Maybe Int]] -> [[Maybe Int]]
    getBoardVerticalLines ([] : t) = []
    getBoardVerticalLines x = map head x : getBoardVerticalLines (map tail x)

isBoardWinning :: Board (Maybe Int) -> Bool
isBoardWinning b = any (all isNothing) (getBoardLines b)

getWinningBoard :: [Int] -> [Board (Maybe Int)] -> (Int, Maybe (Board (Maybe Int)))
getWinningBoard (val : rest) boards = if isJust winningBoard then (val, winningBoard) else getWinningBoard rest newBoards
  where
    winningBoard :: Maybe (Board (Maybe Int))
    winningBoard = foldl (\prev b -> if isBoardWinning b then Just b else prev) Nothing newBoards
    newBoards = map (removeBoardValue val) boards