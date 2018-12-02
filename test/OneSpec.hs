module OneSpec where

import           Control.Exception (evaluate)
import           One
import           Test.Hspec
import           Test.QuickCheck
spec = describe "One" $ do
        it "Finds the first duplicate in a list" $
            firstDuplicate [1,2,3,4,5,6,1] `shouldBe` [1]
        it "Finds another duplicate in a list" $
            firstDuplicate [1,2,3,4,5,6,5] `shouldBe` [5]
        it "Finds the first number again by cycling around the list" $
            firstRepeat [-100,110] `shouldBe` Just 0
