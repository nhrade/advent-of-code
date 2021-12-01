import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Sonar

main :: IO ()
main = hspec $ do
    describe "Sonar.countIncreasing" $ do
        it "counts the number of increasing numbers" $ do
            Sonar.countIncreasing [1,3,5,2,1,6] `shouldBe` 3