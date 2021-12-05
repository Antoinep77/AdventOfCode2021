module Day4.Exo2
  ( solution,
  )
where

import Data.Maybe
import Utils

newtype Board a = Board [[a]]

instance Functor Board where
  fmap f (Board x) = Board $ fmap (fmap f) x

instance Show a => Show (Board a) where
  show (Board x) = "Board " ++ show x

solution :: String -> Int
solution = uncurry (*) . fmap computeBoardSum . uncurry getLoosingBoard . parseInput

parseInput :: String -> ([Int], [Board (Maybe Int)])
parseInput str = (values, boards)
  where
    boards = map (Board . map (map (Just . read) . words) . split "\n") . tail $ blocks
    values = map read . split "," . head $ blocks
    blocks = split "\n\n" str

computeBoardSum :: Board (Maybe Int) -> Int
computeBoardSum (Board lines) = sum $ map (sum . map maybeToInt) lines
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

getLoosingBoard :: [Int] -> [Board (Maybe Int)] -> (Int, Board (Maybe Int))
getLoosingBoard (val : rest) boards = if null filteredNewBoards then (val, head newBoards) else getLoosingBoard rest filteredNewBoards
  where
    filteredNewBoards = filter (not . isBoardWinning) newBoards
    newBoards = map (removeBoardValue val) boards