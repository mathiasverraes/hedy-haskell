{-# LANGUAGE QuasiQuotes #-}
module Level2Spec (spec) where

import           Data.Either
import           Flow
import           Level2
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec       as P
import           Text.Megaparsec.Error
import           Text.RawString.QQ
import qualified Data.Map.Strict       as M

simpleAST =
    [ Assign "greeting" "Hello"
    , Assign "name" "Hedy"
    , Print [Var "greeting", Literal " ", Var "name", Literal "!"]
    ]


spec :: Spec
spec =
    describe "Level2" $ do
        describe "Lang" $ do
            it "replaces variables" $ do
                let chunks = [Var "greeting", Literal " ", Var "name", Literal "!"]
                let vars = M.fromList [("name", "Hedy"), ("greeting", "Hi")]
                replaceVars vars chunks `shouldBe` "Hi Hedy!"
            it "replaces variables that occur twice" $ do pending
            it "doesn't replaces variables recursively" $ do pending
            it "doesn't replace substrings" $ do pending
        describe "Parser" $ do
            it "parses print" $ do
                let ast = PrintLiteral "Hello Hedy!"
                parse pPrint "" "print Hello Hedy!" `shouldParse` ast
            it "parses variable assignment" $ do
                let ast = Assign "foo" "bar"
                parse pAssign "" "foo is bar" `shouldParse` ast
            xit "parses variables as chunks" $ do
                let script = "name is Hedy;print Hello name!"
                let expected = 
                        [ Assign "name" "Hedy" 
                        , Print [Literal "Hello ", Var "name", Literal "!"]]
                parse pProgram "" script `shouldParse` expected


