{-# LANGUAGE TupleSections #-}
module Day6.Exo2
  ( solution,
  )
where
  import Utils
  import qualified Data.Map as Map

  solution :: String -> Int
  solution = sum. map snd. Map.toList . foldl (.) id (map (const transformFish) [1..256]). fishToMap . map read . split ","

  fishToMap :: [Int] -> Map.Map Int Int
  fishToMap [] = Map.fromList $ map (,0) [0..8]
  fishToMap (n:t)= Map.insert n (prevVal + 1) recMap
    where Just prevVal = Map.lookup n recMap
          recMap = fishToMap t

  transformFish ::  Map.Map Int Int -> Map.Map Int Int
  transformFish m = Map.fromList list
    where list = map ( fmap( (\(Just x) -> x) . f) . (\x -> (x,x))) [0..8]
          f:: Int -> Maybe Int
          f 6 =  (+) <$> Map.lookup 0 m <*> Map.lookup 7 m
          f 8 = Map.lookup 0 m
          f x = Map.lookup (x+1) m