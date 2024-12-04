module Day1.Code where

import Control.Arrow
import Data.Bool
import System.Directory

readInput :: IO [(Int, Int)]
readInput = do
    listDirectory "./src/Day1" >>= print
    readFile "./src/Day1/input.txt" >>= \file -> do
        let rows = lines file
        pure $ (read . (!! 0) &&& read . (!! 1)) . words <$> rows

part1 :: [(Int, Int)] -> Int
part1 = sum . fmap abs . uncurry (zipWith (-)) . (quicksort *** quicksort) . unzip

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = quicksort (filter (< x) xs) <> (x : quicksort (filter (>= x) xs))

part2 :: [(Int, Int)] -> Int
part2 = sum . uncurry (liftA2 countIfEqual) . unzip

countIfEqual :: Int -> Int -> Int
countIfEqual x = bool 0 x . (== x)

(.*) = (.) . (.)