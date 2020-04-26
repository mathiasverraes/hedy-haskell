{-# LANGUAGE OverloadedStrings #-}

module Level2.Interpreter where

import           Control.Monad.State.Lazy
import           Data.List                (intercalate)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import qualified Data.Text                as T
import           Level2.AST
import           System.Environment
import           System.Random            (randomRIO)

random :: [a] -> IO a
random xs = (xs !!) <$> randomRIO (0, length xs - 1)

type VarDictionary = M.Map VarName Expr

replaceVars :: VarDictionary -> [Chunk] -> IO String
replaceVars dict = foldM go ""
  where
    go init (ChunkStr s) = return $ init ++ readScalar s dict
    go init (ChunkRandom s) = do
        result <- readRandom s dict
        return $ init ++ result
    readScalar s dict = show (M.findWithDefault (Scalar s) s dict)
    readRandom varName dict =
        case M.lookup varName dict of
            Nothing         -> return "???"
            Just (Scalar s) -> return s
            Just (List xs)  -> random xs

exec :: Stmt -> StateT VarDictionary IO ()
exec (Print chunks) = do
    dict <- get
    let s = replaceVars dict chunks
    lift $ s >>= putStrLn
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
