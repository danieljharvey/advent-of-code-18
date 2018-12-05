module ThreeSpec where

import           Control.Exception (evaluate)
import           Data.Maybe        (Maybe (..))
import           Test.Hspec
import           Test.QuickCheck
import           Three

spec = describe "Three" $ do
    it "Parses a line" $
        (parse "#1 @ 935,649: 22x22") `shouldBe` Just Fabric { index = 1, point = (935, 649), width = 22, height = 22 }
    it "Splits the string up" $
        splitOnNonDigit "#1 @ 935,649: 22x22" `shouldBe` ["1","935","649","22","22"]
    it "Reads them too" $
        readAll (splitOnNonDigit "#1 @ 935,649: 22x22") `shouldBe` [1,935,649,22,22]
