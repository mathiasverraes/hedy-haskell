module Level1a (main') where
    
import Control.Monad.State.Lazy
import System.Environment


main' = evalStateT (execAll sampleProgram) initialState
        

initialState = ""

sampleProgram :: Program
sampleProgram = 
    [ Print (Expr "Hello world!")
    , Ask (Expr "What is your name")
    , Echo (Expr "Hi ")
    ]

data Expr = Expr String deriving (Show)
data Stmt = Print Expr
            | Ask Expr
            | Echo Expr
            deriving (Show)
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

execAll :: [Stmt] -> StateT String IO ()
execAll = mapM_ exec

