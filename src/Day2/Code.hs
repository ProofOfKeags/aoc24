module Day2.Code where

import Data.Functor
import Data.List (nub)

type Report = [Int]

(<&&>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

readInput :: IO [Report]
readInput = fmap (fmap read . words) . lines <$> readFile "./src/Day2/input.txt"

isMonotonic :: Report -> Bool
isMonotonic = (== 1) . length . nub . filter (/= 0) . fmap signum . (zipWith (-) =<< drop 1)

isSmooth :: Report -> Bool
isSmooth = all (((> 0) <&&> (< 4)) . abs) . (zipWith (-) =<< drop 1)

isSafe :: Report -> Bool
isSafe = isMonotonic <&&> isSmooth

part1 :: [Report] -> Int
part1 = length . filter isSafe

dampen :: Report -> [Report]
dampen r = [0 .. length r - 1] <&> \idx -> take idx r <> drop (idx + 1) r

part2 :: [Report] -> Int
part2 = length . filter (any isSafe) . fmap dampen