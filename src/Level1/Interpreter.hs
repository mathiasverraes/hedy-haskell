module Level1.Interpreter where

import           Control.Monad.State.Lazy
import           System.Environment

newtype Expr = Expr String deriving (Show, Eq)
data Stmt = NoOp
            | Print Expr
            | Ask Expr
            | Echo Expr
            deriving (Show, Eq)
type Program = [Stmt]

exec :: Stmt -> StateT String IO ()
exec NoOp = return ()
exec (Print (Expr s)) = lift $ putStrLn s
exec (Ask (Expr s)) = do
    lift $ putStrLn s
    answer <- lift getLine
    put answer
exec (Echo (Expr s)) = do
    answer <- get
    lift $ putStrLn (s ++ answer)

execAll :: Program -> StateT String IO ()
execAll = mapM_ exec

run :: Program -> IO ()
run program = evalStateT (execAll program) initialState
    where initialState = ""
