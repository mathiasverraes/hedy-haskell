module Level2.AST where

type Chunk = String

type VarName = String

data Stmt
    = Print [Chunk]
    | Is VarName String
    | IsList VarName [String]
    | Ask VarName String
    | NoOp
    deriving (Show, Eq)

type Program = [Stmt]
