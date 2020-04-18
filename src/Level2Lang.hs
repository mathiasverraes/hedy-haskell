{-# LANGUAGE OverloadedStrings #-}

module Level2Lang where

import           Control.Monad.State.Lazy
import qualified Data.Map.Strict          as M
import           Data.Maybe
import qualified Data.Text                as T
import           System.Environment

type Chunk = String

type VarName = String

data Stmt
    = Print [Chunk]
    | Is VarName String
    | Ask VarName String
    | NoOp
    deriving (Show, Eq)

type Program = [Stmt]

type VarDictionary = M.Map VarName String

replaceVarsInChunks :: VarDictionary -> [Chunk] -> String
replaceVarsInChunks dict = foldl go ""
  where
    go init chunk = init ++ M.findWithDefault chunk chunk dict

exec :: Stmt -> StateT VarDictionary IO ()
exec (Print chunks) = do
    vars <- get
    let s = replaceVarsInChunks vars chunks
    lift $ putStrLn s
exec (Ask varName str) = do
    lift $ putStrLn str
    answer <- lift getLine
    modify $ M.insert varName answer
exec (Is varName value) = modify $ M.insert varName value

execAll :: Program -> StateT VarDictionary IO ()
execAll = mapM_ exec

run :: Program -> IO ()
run program = evalStateT (execAll program) M.empty
