module Level1Lang where
    
import Control.Monad.State.Lazy
import System.Environment

data Expr = Expr String deriving (Show, Eq)
data Stmt = Print Expr
            | Ask Expr
            | Echo Expr
            deriving (Show, Eq)
type Program = [Stmt]    

exec :: Stmt -> StateT String IO ()
exec (Print (Expr s)) = do
    lift $ putStrLn s
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
