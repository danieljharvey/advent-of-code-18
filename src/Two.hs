module Two where

import           Data.List
import           Data.Monoid
import           Data.Tuple

two :: IO ()
two = do
    ans1 <- exercise1
    print ans1

twos :: String -> Int
twos = countGroups 2

threes :: String -> Int
threes = countGroups 3

countGroups :: Int -> String -> Int
countGroups i str = if l > 0 then 1 else 0 where
    l = length $ filter (\a -> length a == i) $ group $ sort str

allTwos :: [String] -> Int
allTwos strs = getSum $ foldMap Sum $ twos <$> strs

allThrees :: [String] -> Int
allThrees strs = getSum $ foldMap Sum $ threes <$> strs

checkSum :: [String] -> Int
checkSum strs = allTwos strs * allThrees strs

exercise1 :: IO Int
exercise1 = readFile "./data/two.txt" >>= \str -> pure $ checkSum $ lines str

-- part 2

type CompareTuple a = (a,[a])

listOfLists :: (Eq a) => [a] -> [CompareTuple a]
listOfLists [] = []
listOfLists as = fmap (separateMe as) as

separateMe :: (Eq a) => [a] -> a -> CompareTuple a
separateMe as a = (a, leftovers) where
    leftovers = filter (/= a) as

