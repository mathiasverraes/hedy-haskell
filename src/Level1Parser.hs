module Level1Parser where

import           Data.Void
import           Level1Lang
import           Text.Megaparsec            hiding (parse)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: Parser Program -> String -> String -> Either (ParseErrorBundle String Void) Program
parse parser filename body = M.parse parser filename (body ++ "\n")
-- @todo the "\n" is a little hack, figure out how to get rid of it

pString :: Parser String
pString = many (alphaNumChar <|> markChar <|> punctuationChar <|> symbolChar <|> char ' ' <|> char '\t') <?> "a string"

pExpr :: Parser Expr
pExpr = Expr <$> pString <?> "an expression"

pStmt :: String -> (Expr -> Stmt) -> Parser Stmt
pStmt keyword stmtConstructor = do
    let stmt = string keyword *> string " " *> pExpr <* eol
    let nakedStmt = Expr <$> hidden (string (keyword ++ "\n") *> string "")
    expr <- (stmt <|> nakedStmt) <?> "a statement"
    return $ stmtConstructor expr

pPrint, pAsk, pEcho, pNoOp :: Parser Stmt
pPrint = pStmt "print" Print
pAsk = pStmt "ask" Ask
pEcho = pStmt "echo" Echo
pNoOp = hidden (some spaceChar) >> return NoOp

pProgram :: Parser Program
pProgram = do
    space
    program <- many (pNoOp <|> pPrint <|> pAsk <|> pEcho)
    eof
    return program
