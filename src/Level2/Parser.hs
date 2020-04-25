{-# LANGUAGE MultiWayIf #-}

module Level2.Parser where

import           Data.Char                  (isPunctuation, isSpace)
import           Data.List                  (dropWhileEnd)
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Void
import           Flow
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

pLiteralWithoutComma :: Parser String
pLiteralWithoutComma =
    many (alphaNumChar <|> markChar <|> punctuationChar' <|> symbolChar <|> pSpace) <?> "a string literal"
  where
    isPunctuation' :: Char -> Bool
    isPunctuation' c =
        if c == ','
            then False
            else isPunctuation c
    punctuationChar' = satisfy isPunctuation'

pListOfLiteral :: Parser [String]
pListOfLiteral = pLiteralWithoutComma `sepBy` string ","

pVarName, pNotVarName :: Parser String
pVarName = some alphaNumChar <?> "a variable name"

pNotVarName = some (markChar <|> punctuationChar <|> symbolChar <|> pSpace)

pChunks :: Parser [Chunk]
pChunks = many (pVarName <|> pNotVarName)

pPrint :: Parser Stmt
pPrint = do
    string "print"
    chunks <- optional (pSpace *> pChunks) <?> "a string to print"
    pEnd
    return $ Print $ fromMaybe [""] chunks

pIs :: Parser Stmt
pIs = do
    varName <- pVarName
    pSpace
    string "is"
    pSpace
    values <- pListOfLiteral
    pEnd
    return $
        if | length values > 1 -> IsList varName $ trim `fmap` values
           | length values == 1 -> Is varName $ head values
           | otherwise -> Is varName ""
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace

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
pProgram =
    let statements = [pAsk, pIs, pPrint, pNoOp]
     in do space
           program <- many $ choice (try <$> statements)
           eof
           return program
