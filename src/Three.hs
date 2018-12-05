module Three where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe      (Maybe (..), fromMaybe)
import           Data.Monoid
import           Data.Tuple

three :: IO ()
three = print "num"

-- #1 @ 935,649: 22x22

type Point = (Int, Int)
type Index = Int
type Width = Int
type Height = Int

data Fabric = Fabric { index  :: Index
                     , point  :: Point
                     , width  :: Width
                     , height :: Height
} deriving (Show, Eq)

parse :: String -> Maybe Fabric
parse str = case readAll (splitOnNonDigit str) of
    [i,x,y,w,h] -> Just $ Fabric { index = i, point = (x,y), width = w, height = h}
    _ -> Nothing

splitOnNonDigit :: String -> [String]
splitOnNonDigit str = filter (\a -> length a > 0) $ splitOneOf "# x@,:" str

readAll :: [String] -> [Int]
readAll as = fmap read as
