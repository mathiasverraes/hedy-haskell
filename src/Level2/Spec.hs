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

spec :: Spec
spec =
    describe "Level2" $ do
        describe "Interpreter" $ do
            it "replaces variables" $ do
                let chunks = ["greeting", " ", "name", "!"]
                let vars = M.fromList [("name", Scalar "Hedy"), ("greeting", Scalar "Hi")]
                replaceVarsInChunks vars chunks `shouldBe` "Hi Hedy!"
            it "replaces variables that occur twice" $ do
                let chunks = ["foo", " and ", "foo"]
                let vars = M.fromList [("foo", Scalar "bar")]
                replaceVarsInChunks vars chunks `shouldBe` "bar and bar"
            it "doesn't replaces variables recursively" $ do
                let chunks = ["hi", " ", "foo"]
                let vars = M.fromList [("foo", Scalar "bar"), ("bar", Scalar "buz")]
                replaceVarsInChunks vars chunks `shouldBe` "hi bar"
            it "doesn't replace substrings" $ do
                let chunks = ["fool"]
                let vars = M.fromList [("foo", Scalar "bar")]
                replaceVarsInChunks vars chunks `shouldBe` "fool"
            it "pretty prints lists" $ show (List ["foo", "bar"]) `shouldBe` "['foo', 'bar']"
        describe "Parser" $ do
            describe "print" $ do
                it "parses print" $ do
                    let ast = Print ["Hello", " ", "Hedy", "!"]
                    parse pPrint "" "print Hello Hedy!" `shouldParse` ast
                it "parses naked prints" $ do
                    let ast = [Print ["foo"], Print [""]]
                    parse pProgram "" "print foo\nprint\n" `shouldParse` ast
            describe "is" $ do
                it "parses variable assignment" $ do
                    let ast = Is "foo" (Scalar "bar")
                    parse pIs "" "foo is bar" `shouldParse` ast
                it "parses variables as chunks" $ do
                    let script = "name is Hedy\nprint Hello name!"
                    let expected = [Is "name" (Scalar "Hedy"), Print ["Hello", " ", "name", "!"]]
                    parse pProgram "" script `shouldParse` expected
            describe "ask" $
                it "parses ask statements" $ do
                    let script = "name is ask What's your name?"
                    let ast = [Ask "name" "What's your name?"]
                    parse pProgram "" script `shouldParse` ast
            describe "lists" $
                it "assigns a list of strings to a variable" $ do
                    let script = "animals is dog, cat, kangaroo"
                    let ast = [Is "animals" (List ["dog", "cat", "kangaroo"])]
                    parse pProgram "" script `shouldParse` ast
