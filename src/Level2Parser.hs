module Level2Parser where

import qualified Data.Map.Strict            as Map
import           Data.Void
import           Level2Lang
import           Text.Megaparsec            hiding (parse)
import qualified Text.Megaparsec            as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: Parser a -> String -> String -> Either (ParseErrorBundle String Void) a
parse parser filename body = M.parse parser filename (body ++ "\n")
-- @todo the "\n" is a little hack, figure out how to get rid of it

pSpace :: Parser Char
pSpace = char ' ' <|> char '\t'

pString :: Parser String
pString = many (alphaNumChar <|> markChar <|> punctuationChar <|> symbolChar <|> pSpace) <?> "a string"

pEmpty :: Parser String
pEmpty = return ""

pStmt :: String -> (String -> Stmt) -> Parser Stmt
pStmt keyword stmtConstructor = do
    let stmt = string keyword *> pSpace *> pString <* eol
    let nakedStmt = string keyword *> eol
    let withSpace = string keyword *> many pSpace <* eol
    s <- (try stmt <|> try withSpace <|> nakedStmt) <?> "a statement"
    return $ stmtConstructor s

pVarname :: Parser String
pVarname = some alphaNumChar <?> "a variable name"

pAssign :: Parser Stmt
pAssign = do
    varname <- pVarname
    pSpace
    string "is"
    pSpace
    value <- pString
    eol
    return $ Is varname value

pPrint :: Parser Stmt
pPrint = do
    let stmt = string "print" *> pSpace *> pChunks <* eol
    let withSpace :: Parser Chunk
        withSpace = (: []) <$> string "print" *> many pSpace <* eol
    let nakedStmt :: Parser Chunk
        nakedStmt = (: []) <$> string "print" *> eol
    --s <- (try stmt <|> try withSpace <|> nakedStmt) <?> "a string to print"
    s <- stmt <?> "a string to print"
    return $ Print s

pNoOp :: Parser Stmt
pNoOp = hidden (some spaceChar) >> eol >> return NoOp

{-
pAsk = pStmt "ask" Ask
-}
pProgram :: Parser Program
pProgram = do
    space
    --program <- many (pNoOp <|> pPrint <|> pAsk <|> pEcho)
    program <- many (try pAssign <|> try pPrint <|> pNoOp)
    eof
    return program

pChunks :: Parser [Chunk]
pChunks = do
    let notVarname = some (markChar <|> punctuationChar <|> symbolChar <|> pSpace)
    many (pVarname <|> notVarname)
