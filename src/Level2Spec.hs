{-# LANGUAGE QuasiQuotes #-}
module Level2Spec (spec) where

import           Data.Either
import qualified Data.Map.Strict       as M
import           Level2Lang
import           Test.Hspec
import           Text.Megaparsec.Error
import           Text.RawString.QQ

simpleAST =
    [ Assign "greeting" "Hello"
    , Assign "name" "Hedy"
    , Print [ChVar "greeting", ChStr " ", ChVar "name", ChStr "!"]
    ]


spec :: Spec
spec =
    describe "Level2Lang" $ do
        it "replaces variables" $ do
            let chunks = [ChVar "greeting", ChStr " ", ChVar "name", ChStr "!"]
            let vars = M.fromList [("name", "Hedy"), ("greeting", "Hi")]
            replaceVars vars chunks `shouldBe` "Hi Hedy!"
        it "replaces variables that occur twice" $ do pending
        it "doesn't replaces variables recursively" $ do pending
        it "doesn't replace substrings" $ do pending



