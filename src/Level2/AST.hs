module Level2.AST where

import           Data.List (intercalate)

type VarName = String

data Chunk
    = ChunkStr String
    | ChunkRandom VarName
    deriving (Show, Eq)

data Expr
    = Scalar String
    | List [String]
    deriving (Eq)

instance Show Expr where
    show (Scalar s) = s
    show (List xs) = "[" ++ intercalate ", " (wrap '\'' <$> xs) ++ "]"
      where
        wrap x xs = x : xs ++ [x]

data Stmt
    = Print [Chunk]
    | Is VarName Expr
    | Ask VarName String
    | NoOp
    deriving (Show, Eq)

type Program = [Stmt]
