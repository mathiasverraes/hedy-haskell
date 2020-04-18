module Level1.Parser where

import           Data.Void
import           Level1.AST
import           Text.Megaparsec            hiding (parse)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: Parser Program -> String -> String -> Either (ParseErrorBundle String Void) Program
parse parser filename body = M.parse parser filename (body ++ "\n")
-- @todo the "\n" is a little hack, figure out how to get rid of it

pSpace :: Parser Char
pSpace = char ' ' <|> char '\t'

pString :: Parser String
pString = many (alphaNumChar <|> markChar <|> punctuationChar <|> symbolChar <|> pSpace) <?> "a string"

pEmpty :: Parser String
pEmpty = return ""

pStmt :: String -> (Expr -> Stmt) -> Parser Stmt
pStmt keyword stmtConstructor = do
    let stmt = string keyword *> pSpace *> pString <* eol
    let nakedStmt = string keyword *> eol
    let withSpace = string keyword *> many pSpace <* eol
    s <- (try stmt <|> try withSpace <|> nakedStmt) <?> "a statement"
    return $ stmtConstructor (Expr s)

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
