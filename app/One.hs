module One where

import Data.Char (isDigit)
import Data.List (elem,findIndices)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data Symbol = Add | Sub

one :: IO ()
one = do 
    str <- readFile "./data/one.txt"
    let part1 = parseAndTotal $ parseLines str
    let part2 = firstRepeat $ parseLines str
    print $ "Part 1: " ++ show part1 ++ ", Part2: " ++ show part2

parseLines :: String -> [Integer]
parseLines str = map parse $ lines str

parseAndTotal :: [Integer] -> Integer
parseAndTotal parsed = foldr (+) 0 parsed

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
isDuplicate as a = (length matches) > 1 where 
    matches = findIndices (`elem` [a]) as

scanThem :: [Integer] -> [Integer]
scanThem parsed = scanl (flip (+)) 0 parsed

firstRepeat :: [Integer] -> Integer
firstRepeat parsed = head $ filterNonDupes $ scanThem $ parsed
