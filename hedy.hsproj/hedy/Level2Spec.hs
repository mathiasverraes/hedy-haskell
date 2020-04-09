{-# LANGUAGE QuasiQuotes #-}
module Level2Spec (spec) where
    
import Text.RawString.QQ
import Test.Hspec
import Data.Either
import Text.Megaparsec.Error
import qualified Data.Map.Strict as M


import Level2Lang

spec :: Spec
spec = do 
    describe "Level2Lang" $ do
        it "replaces variables" $ do
            let vars = M.fromList [("name", "Hedy"), ("greeting", "Hi")]         
            replaceVars "greeting name!" vars `shouldBe` "Hi Hedy!"
        
        it "replaces variables that occur twice" $ do
            let vars = M.fromList [("name", "Hedy")]         
            replaceVars "name name" vars `shouldBe` "Hedy Hedy"  

        it "doesn't replaces variables recursively" $ do
            let vars = M.fromList [("first", "second"), ("second", "third")]         
            replaceVars "first second" vars `shouldBe` "second third"  
            
        it "doesn't replace substrings" $ do
             let vars = M.fromList [("foo", "bar")]
             replaceVars "foo shmfoo" vars `shouldBe` "bar shmfoo"  
             replaceVars "foo shmfoo" vars `shouldNotBe` "bar shmbar"  

simpleAST = 
    [ Assign "greeting" "Hello"
    , Assign "name" "Hedy"
    , Print (Expr "greeting name!")
    ]
