module Two where

import           Data.List
import           Data.Monoid
import           Data.Tuple
import           Data.Maybe (fromMaybe)

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
type AnswerTuple a = (a,a)

listOfLists :: [a] -> [CompareTuple a]
listOfLists [] = []
listOfLists as = fmap (copyMe as) as

copyMe :: [a] -> a -> CompareTuple a
copyMe = flip (,)

differences :: String -> String -> Int
differences a b = length $ filter (== True) $ zipWith (/=) a b

countTrue :: [Bool] -> Int
countTrue = foldl (\i v -> if v then i + 1 else i) 0

hasAnswer :: CompareTuple String -> [AnswerTuple String]
hasAnswer (a,as) = fmap (\c -> (a,c)) matching where
    matching = filter (\b -> (differences a b) == 1) as

exercise2Logic :: [String] -> Maybe (AnswerTuple String)
exercise2Logic lines = case listOfLists lines >>= hasAnswer of 
       (x:_) -> Just x
       _ -> Nothing

combineDiffs :: String -> String -> String
combineDiffs a b = filter (`elem` b) a

formatAns :: Maybe (AnswerTuple String) -> String
formatAns ans = fromMaybe "Could not find a pair" formattedAns where
    formattedAns = fmap (\a -> combineDiffs (fst a) (snd a)) ans

onlyTwo :: [String] -> [AnswerTuple String]
onlyTwo lines = listOfLists lines >>= hasAnswer

exercise2 :: IO String
exercise2 = readFile "./data/two.txt"
    >>= (\strs -> pure $ formatAns $ exercise2Logic $ lines strs)


