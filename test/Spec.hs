import Bomberman
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main:: IO ()
main = hspec spec

spec::Spec
spec = do
        describe "soma dos quadrados" $ do
                it "teste" $
                    [1,2,3,4,5,3,7,8,3] `shouldBe` [1,2,3,4,5,3,7,8,3]
