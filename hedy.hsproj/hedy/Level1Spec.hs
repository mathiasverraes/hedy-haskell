{-# LANGUAGE QuasiQuotes #-}
module Level1Spec (spec) where
    
import Text.RawString.QQ
import Test.Hspec
import Data.Either

import Level1

spec :: Spec
spec = do 
    describe "Level1Lang" $ do
        it "parses a simple hedy script" $ do         
            parse "test" script01 `shouldBe` Right ast01

        it "fails for bad input" $ do    
            parse "test" script02 `shouldSatisfy` isLeft
            
        it "ignores comments" $ do
            parse "test" script03 `shouldBe` Right ast03

script01 = [r|
print Hello world!
ask What's for dinner?
echo Enjoy your 
|]

ast01 = 
    [ Print (Expr "Hello world!")
    , Ask (Expr "What's for dinner?")
    , Echo (Expr "Enjoy your ")
    ]   

script02 = [r|
print Hello world!
Bad input
echo Enjoy your 
|]   


script03 = [r|
print Hello
// line comment
/*
 * block comment
 */
print world
|]   

ast03 = 
    [ Print (Expr "Hello")
    , Print (Expr "world")
    ]   