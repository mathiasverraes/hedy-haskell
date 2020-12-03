module Foo where

import Control.Monad.Combinators.Expr

import           Text.Megaparsec

import Control.Applicative hiding (many)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators (many)
     

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Char


sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)
  
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (L.symbol' "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = undefined -- TODO
  