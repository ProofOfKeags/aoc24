{-# LANGUAGE OverloadedStrings #-}

module Day4.Code where

import Data.Functor
import Data.List
import qualified Data.Text as Text
import Data.Traversable (for)

readInput :: IO [[Char]]
readInput = lines <$> readFile "./src/Day4/input.txt"

xmas :: Text.Text
xmas = "XMAS"

right :: [[Char]] -> [[Char]]
right = id

left :: [[Char]] -> [[Char]]
left = fmap reverse

down :: [[Char]] -> [[Char]]
down = right . transpose

up :: [[Char]] -> [[Char]]
up = left . transpose

padRight :: [[Char]] -> [[Char]]
padRight m = fmap (replicate (length m - 1) '.' <>) m

shiftLeft :: Int -> [Char] -> [Char]
shiftLeft i r = drop i r <> replicate i '.'

shearRight :: [[Char]] -> [[Char]]
shearRight m = zipWith shiftLeft [0 .. length m - 1] (padRight m)

shearLeft :: [[Char]] -> [[Char]]
shearLeft m = zipWith shiftLeft (reverse [0 .. length m - 1]) (padRight m)

upRight :: [[Char]] -> [[Char]]
upRight = up . shearLeft

upLeft :: [[Char]] -> [[Char]]
upLeft = up . shearRight

downRight :: [[Char]] -> [[Char]]
downRight = down . shearRight

downLeft :: [[Char]] -> [[Char]]
downLeft = down . shearLeft

count :: [[Char]] -> Int
count = sum . fmap (subtract 1 . length . Text.splitOn xmas . Text.pack)

part1 :: [[Char]] -> Int
part1 = foldr (liftA2 (+) . (count .)) (const 0) [right, left, up, down, upRight, downRight, upLeft, downLeft]

type Window = [[Char]]
(<<$>>) = fmap fmap fmap

windows :: [[Char]] -> [Window]
windows m = do
    let get x y = m !! x !! y
    i <- [1 .. length m - 2]
    j <- [1 .. length (m !! i) - 2]
    let relativeCoords =
            [ [(i - 1, j - 1), (i, j - 1), (i + 1, j - 1)]
            , [(i - 1, j + 0), (i, j + 0), (i + 1, j + 0)]
            , [(i - 1, j + 1), (i, j + 1), (i + 1, j + 1)]
            ]
    [uncurry get <<$>> relativeCoords]

isXmas :: Window -> Bool
isXmas [[a, _, c], [_, e, _], [g, _, i]] =
    e == 'A'
        && ((a == 'M' && i == 'S') || (a == 'S' && i == 'M'))
        && ((c == 'M' && g == 'S') || (c == 'S' && g == 'M'))
isXmas _ = False

part2 :: [[Char]] -> Int
part2 = length . filter id . fmap isXmas . windows