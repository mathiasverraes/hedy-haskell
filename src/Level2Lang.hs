{-# LANGUAGE OverloadedStrings #-}

module Level2Lang where

import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as M
import           Data.Maybe
import qualified Data.Text                as T
import           System.Environment

data Chunk =
    Literal String
    | Var VarName
    deriving (Show, Eq)

type VarName = String

data Stmt
    = Print [Chunk]
    | PrintLiteral String
    | Assign VarName String
    | Ask VarName String
    deriving (Show, Eq)

type Program = [Stmt]

type VarDictionary = M.Map VarName String

replaceVarsInChunks :: VarDictionary -> [Chunk] -> String
replaceVarsInChunks dict = foldl go ""
  where
    go :: String -> Chunk -> String
    go init (Var varName) = do
        let val = M.lookup varName dict
        init ++ fromMaybe "" val
    go init (Literal str) = init ++ str

exec :: Stmt -> StateT VarDictionary IO ()
exec (Print chunks) = do
    vars <- get
    let str = replaceVarsInChunks vars chunks
    exec (PrintLiteral str)
exec (PrintLiteral s) = lift $ putStrLn s
exec (Ask varName str) = do
    lift $ putStrLn str
    answer <- lift getLine
    modify $ M.insert varName answer
exec (Assign varName value) = modify $ M.insert varName value

execAll :: Program -> StateT VarDictionary IO ()
execAll = mapM_ exec

run :: Program -> IO ()
run program = evalStateT (execAll program) M.empty
