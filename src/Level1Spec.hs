{-# LANGUAGE QuasiQuotes #-}
module Level1Spec (spec) where
    
import Text.RawString.QQ
import Test.Hspec
import Data.Either
import Text.Megaparsec.Error
import Test.Hspec.Megaparsec
import Level1
import qualified Text.Megaparsec as P
import Flow

spec :: Spec
spec =
    describe "Level1Lang" $ do
        it "parses a single line"
            <| parse pProgram "" "print Hello world!" `shouldParse` [Print (Expr "Hello world!")]
        it "parses a simple hedy script"
            <| parse pProgram "" simpleScript `shouldParse` simpleAST
        it "parses empty lines"
            <| parse pProgram "" emptyLines `shouldParse` emptyLinesAST
        it "accepts empty statements"
            <| parse pProgram "" "print" `shouldParse` [Print (Expr "")]
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
    , Echo (Expr "Enjoy your ")
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

