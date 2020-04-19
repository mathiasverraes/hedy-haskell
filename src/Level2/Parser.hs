module Level2.Parser where

import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Void
import           Level2.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pSpace :: Parser Char
pSpace = char ' ' <|> char '\t'

pEmpty :: Parser String
pEmpty = return ""

pEnd :: Parser String
pEnd = string "\n" <|> string "\r\n" <|> (string "" <* eof) <?> "the end of the line"

pLiteral :: Parser String
pLiteral = many (alphaNumChar <|> markChar <|> punctuationChar <|> symbolChar <|> pSpace) <?> "a string literal"

pVarName, pNotVarName :: Parser String
pVarName = some alphaNumChar <?> "a variable name"
pNotVarName = some (markChar <|> punctuationChar <|> symbolChar <|> pSpace)

pAssign :: Parser Stmt
pAssign = do
    varname <- pVarName
    pSpace
    string "is"
    pSpace
    value <- pLiteral
    pEnd
    return $ Is varname value

pPrint :: Parser Stmt
pPrint = do
    string "print"
    chunks <- optional (pSpace *> pChunks)  <?> "a string to print"
    pEnd
    return $ Print $ fromMaybe [""] chunks

pAsk :: Parser Stmt
pAsk = do
    varName <- pVarName
    pSpace
    string "is"
    pSpace
    string "ask"
    pSpace
    literal <- pLiteral
    pEnd
    return $ Ask varName literal


pNoOp :: Parser Stmt
pNoOp = hidden (some spaceChar) >> pEnd >> return NoOp

pProgram :: Parser Program
pProgram = do
    space
    program <- many (try pAssign <|> try pPrint <|> pNoOp)
    eof
    return program

pChunks :: Parser [Chunk]
pChunks = many (pVarName <|> pNotVarName)
