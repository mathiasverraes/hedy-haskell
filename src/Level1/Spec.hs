{-# LANGUAGE QuasiQuotes #-}
module Level1.Spec (spec) where

import           Data.Either
import           Flow
import           Level1
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec       as P
import           Text.Megaparsec.Error
import           Text.RawString.QQ

spec :: Spec
spec =
    describe "Level1" $ 
      describe "Parser" $ do
        it "parses a single line"
            <| parse pProgram "" "print Hello world!" `shouldParse` [Print (Expr "Hello world!")]
        it "fails for missing space"
            <| parse pProgram "" `shouldFailOn` "printHello"     
        it "parses a simple hedy script"
            <| parse pProgram "" simpleScript `shouldParse` simpleAST
        it "parses empty lines"
            <| parse pProgram "" emptyLines `shouldParse` emptyLinesAST
        it "accepts empty statements"
            <| parse pProgram "" "print\n" `shouldParse` [Print (Expr ""), NoOp]
        it "accepts empty statements with space"
            <| parse pProgram "" "print \n" `shouldParse` [Print (Expr ""), NoOp]
        it "fails for bad input"
            <| parse pProgram "" `shouldFailOn` badScript
        it "fails for bad input2"
            <| parse pProgram "" `shouldFailOn` badScript2

simpleScript = [r|print Hello world!
ask What's for dinner?
echo Enjoy your |]

simpleAST =
    [ Print (Expr "Hello world!")
    , Ask (Expr "What's for dinner?")
    , Echo (Expr "Enjoy your ")
    ]

emptyLines = [r|

print Hello world!

ask What's for dinner?

echo Enjoy your

|]

emptyLinesAST =
    [ Print (Expr "Hello world!")
    , NoOp
    , Ask (Expr "What's for dinner?")
    , NoOp
    , Echo (Expr "Enjoy your")
    , NoOp
    ]

badScript = [r|
print Hello world!
Bad input
echo Enjoy your
|]

badScript2 = [r|
print Hello world!
print
Bad input
echo Enjoy your
|]

