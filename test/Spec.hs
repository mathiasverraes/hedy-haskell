import qualified Level1.Spec as L1
import qualified Level2.Spec as L2
import           Test.Hspec

main = hspec $ do
    L1.spec
    L2.spec
