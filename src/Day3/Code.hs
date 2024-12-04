{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3.Code where

import Control.Arrow
import Data.Attoparsec.Text
import Data.Either
import Data.Text as Text
import Data.Text.IO

testInputPart1 :: Text
testInputPart1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

readInput :: IO Text
readInput = Data.Text.IO.readFile "./src/Day3/input.txt"

mulParser :: Parser (Int, Int)
mulParser = liftA2 (,) (string "mul(" *> decimal <* char ',') (decimal <* char ')')

part1 :: Text -> Int
part1 = sum . fmap (uncurry (*)) . rights . fmap (parseOnly mulParser) . tails

testInputPart2 :: Text
testInputPart2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = second (Text.drop (Text.length needle)) (breakOn needle haystack)

part2Enabled :: Text -> Int
part2Enabled "" = 0
part2Enabled t = uncurry (+) . (part1 *** part2Disabled) . splitOnceOn "don't()" $ t

part2Disabled :: Text -> Int
part2Disabled "" = 0
part2Disabled t = part2Enabled . snd . splitOnceOn "do()" $ t

part2 :: Text -> Int
part2 = part2Enabled