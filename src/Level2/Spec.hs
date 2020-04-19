module Level2.Spec
    ( spec
    ) where

import           Data.Either
import qualified Data.Map.Strict       as M
import           Flow
import           Level2
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Error
import           Text.RawString.QQ

simpleAST = [Is "greeting" "Hello", Is "name" "Hedy", Print ["greeting", " ", "name", "!"]]

spec :: Spec
spec =
    describe "Level2" $ do
        describe "Interpreter" $ do
            it "replaces variables" $ do
                let chunks = ["greeting", " ", "name", "!"]
                let vars = M.fromList [("name", "Hedy"), ("greeting", "Hi")]
                replaceVarsInChunks vars chunks `shouldBe` "Hi Hedy!"
            it "replaces variables that occur twice" $ do pending
            it "doesn't replaces variables recursively" $ do pending
            it "doesn't replace substrings" $ do pending
        describe "Parser" $ do
            it "parses print" $ do
                let ast = Print ["Hello", " ", "Hedy", "!"]
                parse pPrint "" "print Hello Hedy!" `shouldParse` ast
            it "parses variable assignment" $ do
                let ast = Is "foo" "bar"
                parse pAssign "" "foo is bar" `shouldParse` ast
            it "parses variables as chunks" $ do
                pending
                let script = "name is Hedy;print Hello name!"
                let expected = [Is "name" "Hedy", Print ["Hello ", "name", "!"]]
                parse pProgram "" script `shouldParse` expected
