{-# LANGUAGE QuasiQuotes #-}
module Level1Spec (spec) where
    
import Text.RawString.QQ
import Test.Hspec
import Data.Either
import Text.Megaparsec.Error

import Level1

spec :: Spec
spec = do 
    describe "Level1Lang" $ do
        it "parses a single line" $ do         
            parse "" "print Hello world!" `shouldBe` Right [Print (Expr "Hello world!")]
            
        it "parses a simple hedy script" $ do         
            parse "" simpleScript `shouldBe` Right simpleAST

        it "fails for bad input" $ do    
            let parsed = parse "test" badScript
            either errorBundlePretty show parsed `shouldContain` "expecting \"ask\", \"echo\", \"print\", or end of input"
            
        it "ignores comments" $ do
            parse "" withComments `shouldBe` Right simpleAST

simpleScript = [r|
print Hello world!
ask What's for dinner?
echo Enjoy your 
|]

simpleAST = 
    [ Print (Expr "Hello world!")
    , Ask (Expr "What's for dinner?")
    , Echo (Expr "Enjoy your ")
    ]   

badScript = [r|
print Hello world!
Bad input
echo Enjoy your 
|]   


withComments = [r|
print Hello world!

// line comment
ask What's for dinner?

/*
 * block comment
 */

echo Enjoy your 
|]   
