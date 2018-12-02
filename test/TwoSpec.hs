module TwoSpec where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck
import           Two

spec = describe "Two" $ do
    it "Counts the twos" $
        twos "poo" `shouldBe` 1
    it "Counts the twos even in the wrong order" $
        twos "opo" `shouldBe` 1
    it "Counts the threes" $
        threes "pooo" `shouldBe` 1
    it "Counts number that have twos" $
        allTwos ["poohh","ooh","no"] `shouldBe` 2
    it "Counts all those that have threes" $
        allThrees ["pooo","sdfsdf","sssddd"] `shouldBe` 2
    it "list of lists" $
        listOfLists [1,2,3] `shouldBe` [(1,[2,3]), (2, [1,3]), (3, [1,2])]
    it "Seperate me" $
        separateMe [1,2,3] 1 `shouldBe` (1, [2,3])
    it "Seperate me by index not value" $
        separateMe [2,2,3] 1 `shouldBe` (2, [2,3])
