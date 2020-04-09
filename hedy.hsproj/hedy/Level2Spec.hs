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

       

simpleAST = 
    [ Assign "greeting" "Hello"
    , Assign "name" "Hedy"
    , Print (Expr "greeting name!")
    ]
