module One where

import           Data.Char   (isDigit)
import           Data.List
import           Data.Maybe  (fromMaybe)
import           Data.Monoid
import           Text.Read   (readMaybe)

data Symbol = Add | Sub

one :: IO ()
one = do
    str <- readFile "./data/one.txt"
    let part1 = parseAndTotal $ parseLines str
    let part2 = case firstRepeat $ parseLines str of
            Just n -> "Found the answer which is " ++ show n
            _      -> "Did not find"
    print $ "Part 1: " ++ show part1 ++ ", Part2: " ++ show part2

parseLines :: String -> [Integer]
parseLines str = map parse $ lines str

parseAndTotal :: [Integer] -> Integer
parseAndTotal = sum

parse :: String -> Integer
parse "" = 0
parse str = case symbol str of
    Add -> nums str
    _   -> -1 * nums str

symbol :: String -> Symbol
symbol s = if '-' `elem` s then Sub else Add

nums :: String -> Integer
nums str = fromMaybe 0 $ readMaybe $ filter isDigit str


filterNonDupes :: [Integer] -> [Integer]
filterNonDupes as = filter (isDuplicate as) as

isDuplicate :: [Integer] -> Integer -> Bool
isDuplicate as a = length matches > 1 where
    matches = findIndices (`elem` [a]) as

scanThem :: [Integer] -> [Integer]
scanThem = scanl (flip (+)) 0

firstDuplicate :: [Integer] -> [Integer]
firstDuplicate [] = []
firstDuplicate all = thing [] all where
    thing :: [Integer] -> [Integer] -> [Integer]
    thing _ [] = []
    thing found (a:as) = if a `elem` found
                       then [a]
                       else thing (a : found) as

firstRepeat :: [Integer] -> Maybe Integer
firstRepeat parsed = safeHead $ firstDuplicate $ scanThem $ cycle parsed

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a
