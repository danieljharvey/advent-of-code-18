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
        listOfLists [1,2,3] `shouldBe` [(1,[1,2,3]), (2, [1,2,3]), (3, [1,2,3])]
    it "Seperate me" $
        copyMe [1,2,3] 1 `shouldBe` (1, [1,2,3])
    it "Seperate me by index not value" $
        copyMe [2,2,3] 1 `shouldBe` (1, [2,2,3])
    it "counts differences between strings" $
        differences "abc" "abd" `shouldBe` 1
    it "counts order of letters" $
        differences "abc" "acb" `shouldBe` 2
    it "counts zero differences between same string" $
        differences "abc" "abc" `shouldBe` 0
    it "combines diffs" $
        combineDiffs "dog" "log" `shouldBe` "og"
    it "counts diffs right" $
        differences "tqyvfuogzarflkpcxdewsmjhxi" "tqyvjuogzarflkpcadewsmjhxi" `shouldBe` 2 
    it "does not have a valid answer" $
        hasAnswer ("poop", ["poop","pddlp","laop"]) `shouldBe` []
    it "has a valid answer" $
        hasAnswer ("abcd", ["abcd", "abce", "azci"]) `shouldBe` [("abcd", "abce")]
    it "tries the whole part 2" $
        exercise2Logic ["abcd", "abce", "azci", "poop"] `shouldBe` Just ("abcd", "abce")
