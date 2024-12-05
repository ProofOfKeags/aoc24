{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Day5.Code where

import Control.Arrow
import Data.Bool
import Data.Either.Combinators
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Tuple

type OrdRule = (Int, Int)
type Update = [Int]

readInput :: IO ([OrdRule], [Update])
readInput = do
  contents <- Text.readFile "./src/Day5/input.txt"
  let (section1, section2) = second (drop 1) . break Text.null $ Text.lines contents
  let rules = (decimalText *** decimalText . Text.drop 1) . Text.break (== '|') <$> section1
  let updates = fmap decimalText . Text.splitOn "," <$> section2
  pure (rules, updates)
 where
  decimalText = fst . fromRight' . Text.decimal

type DepGraph = HashMap.HashMap Int (HashSet.HashSet Int)

mkDepGraph :: [OrdRule] -> DepGraph
mkDepGraph = HashMap.fromListWith HashSet.union . fmap (second HashSet.singleton . swap)

isValidOrder :: DepGraph -> Update -> Bool
isValidOrder graph = all (maybe True (uncurry checkRest) . uncons) . tails
 where
  checkRest :: Int -> [Int] -> Bool
  checkRest x = not . any (flip HashSet.member (fromMaybe HashSet.empty $ HashMap.lookup x graph))

getMid :: Update -> Int
getMid update = update !! (length update `div` 2)

part1 :: ([OrdRule], [Update]) -> Int
part1 (rules, updates) = sum . fmap getMid $ filter (isValidOrder (mkDepGraph rules)) updates

depCmp :: DepGraph -> Int -> Int -> Ordering
depCmp graph a b = maybe LT (bool LT GT . HashSet.member b) (HashMap.lookup a graph)

part2 :: ([OrdRule], [Update]) -> Int
part2 (rules, updates) = sum . fmap (getMid . sortBy (depCmp graph)) $ filter (not . isValidOrder graph) updates
 where
  graph = mkDepGraph rules
