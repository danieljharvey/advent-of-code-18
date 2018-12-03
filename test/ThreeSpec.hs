module ThreeSpec where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck
import           Three

spec = describe "Three" $ do
    it "Seems OK" $
        True `shouldBe` True
