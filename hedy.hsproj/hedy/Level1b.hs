module Level1b where
    
-- import Control.Applicative
-- import Control.Monad
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State.Lazy
import System.Environment


main = do
    
    putStrLn "Running Hedy Level 1"
    let parseResult = parse pPrint "level1" sample
    case parseResult of
        Right program -> execute $ evalStateT program ""
        _ -> putStrLn "error"
    return ()    

sample = "print Hello worldaa!"


program :: StateT String IO ()
program = do 
    execute $ Print (Expr "Hello world!")
    execute $ Ask (Expr "What is your name")
    execute $ Echo (Expr "Hi ")


data Expr = Expr String deriving (Show)
data Stmt = Print Expr
            | Ask Expr
            | Echo Expr
            deriving (Show)
    

execute :: Stmt -> StateT String IO ()
execute (Print (Expr s)) = do
    lift $ putStrLn s
execute (Ask (Expr s)) = do
    lift $ putStrLn s
    answer <- lift getLine
    put answer
execute (Echo (Expr s)) = do
    answer <- get
    lift $ putStrLn (s ++ answer)



type Parser = Parsec Void String

pPrint :: Parser Stmt
pPrint = do
    string "print"
    spaceChar
    expr <- manyTill printChar eof
    return $ Print (Expr expr)
    

