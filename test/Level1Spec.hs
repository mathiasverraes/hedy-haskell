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
            <| parse pAll "" "print Hello world!" `shouldParse` [Print (Expr "Hello world!")]
        it "parses a simple hedy script"  
            <| parse pAll "" simpleScript `shouldParse` simpleAST
        it "parses empty lines"
            <| parse pAll "" emptyLines `shouldParse` simpleAST    
        it "accepts empty statements"  
            <| parse pAll "" "print" `shouldParse` [Print (Expr "")]
        it "fails for bad input"  
            <| parse pAll "" `shouldFailOn` badScript
        it "fails for bad input2"  
            <| parse pAll "" `shouldFailOn` badScript2
            
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

