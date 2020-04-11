{-# LANGUAGE OverloadedStrings #-}

module Level2Lang where

import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as M
import           Data.Maybe
import qualified Data.Text                as T
import           System.Environment

data Chunk =
    ChStr String
    | ChVar VarName
    deriving (Show, Eq)

type VarName = String

data Stmt
    = Print [Chunk]
    | PrintS String
    | Assign VarName String
    | Ask VarName String
    deriving (Show, Eq)

type Program = [Stmt]

type Vars = M.Map VarName String

replaceVars :: Vars -> [Chunk] -> String
replaceVars vars = foldl go ""
  where
    go :: String -> Chunk -> String
    go init (ChVar varName) = do
        let val = M.lookup varName vars
        init ++ fromMaybe "" val
    go init (ChStr str) = init ++ str

exec :: Stmt -> StateT Vars IO ()
exec (Print chunks) = do
    vars <- get
    let str = replaceVars vars chunks
    exec (PrintS str)
exec (PrintS s) = lift $ putStrLn s
exec (Ask varName str) = do
    lift $ putStrLn str
    answer <- lift getLine
    modify $ M.insert varName answer
exec (Assign varName value) = modify $ M.insert varName value

execAll :: Program -> StateT Vars IO ()
execAll = mapM_ exec

run :: Program -> IO ()
run program = evalStateT (execAll program) M.empty
