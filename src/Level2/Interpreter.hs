{-# LANGUAGE OverloadedStrings #-}

module Level2.Interpreter where

import           Control.Monad.State.Lazy
import           Data.List                (intercalate)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import qualified Data.Text                as T
import           Level2.AST
import           System.Environment

type VarDictionary = M.Map VarName Expr

replaceVarsInChunks :: VarDictionary -> [Chunk] -> String
replaceVarsInChunks dict = foldl go ""
  where
    go init chunk = init ++ show (M.findWithDefault (Scalar chunk) chunk dict)

exec :: Stmt -> StateT VarDictionary IO ()
exec (Print chunks) = do
    vars <- get
    let s = replaceVarsInChunks vars chunks
    lift $ putStrLn s
exec (Ask varName str) = do
    lift $ putStrLn str
    answer <- lift getLine
    modify $ M.insert varName (Scalar answer)
exec (Is varName expr) = modify $ M.insert varName expr
exec NoOp = lift $ putStrLn ""

execAll :: Program -> StateT VarDictionary IO ()
execAll = mapM_ exec

run :: Program -> IO ()
run program = evalStateT (execAll program) M.empty
