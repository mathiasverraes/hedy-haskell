import qualified Level1Spec
import qualified Level2Spec
import           Test.Hspec

main = hspec $ do
    Level1Spec.spec
    Level2Spec.spec
