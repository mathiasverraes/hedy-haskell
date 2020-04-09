{-# LANGUAGE OverloadedStrings #-}
module Level2Lang where
    
import Control.Monad.State.Lazy
import System.Environment
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Expr = Expr String deriving (Show, Eq)
type VarName = String
type VarValue = String
data Stmt = Print Expr
            | Assign VarName VarValue
            | Ask VarName Expr
            deriving (Show, Eq)
type Program = [Stmt]    
type Vars = M.Map VarName VarValue

exec :: Stmt -> StateT Vars IO ()
exec (Print (Expr s)) = do
    vars <- get
    let s' = replaceVars s vars
    lift $ putStrLn s'
exec (Ask varName (Expr s)) = do
    lift $ putStrLn s
    answer <- lift getLine
    modify $ M.insert varName answer
exec (Assign name value) = do
    modify $ M.insert name value

execAll :: Program -> StateT Vars IO ()
execAll = mapM_ exec

run :: Program -> IO ()
run program = evalStateT (execAll program) initialState
    where initialState = M.empty

replaceVars :: String -> Vars -> String
replaceVars s vars = M.foldrWithKey' replace s vars
    where 
        replace name val s = T.unpack $ T.replace (T.pack name) (T.pack val) (T.pack s)
        -- @todo maybe just swap String for Text everywhere?

--(k -> a -> b -> b) -> b -> Map k a -> b
-- foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> bSource


    