module Spec where
    
import Test.Hspec
import qualified Level1Spec
import qualified Level2Spec

main = hspec $ do
    Level1Spec.spec
    Level2Spec.spec
