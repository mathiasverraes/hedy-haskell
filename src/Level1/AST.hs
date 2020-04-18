module Level1.AST where

newtype Expr =
    Expr String
    deriving (Show, Eq)

data Stmt
    = NoOp
    | Print Expr
    | Ask Expr
    | Echo Expr
    deriving (Show, Eq)

type Program = [Stmt]