{-# LANGUAGE QuasiQuotes #-}
module Level1Parser (parse) where
    
import Level1Lang
import Text.RawString.QQ
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

parse :: String -> String -> Either (ParseErrorBundle String Void) Program
parse filename body = M.parse pAll filename (body ++"\n") 
-- @todo the "\n" is a little hack, figure out how to get rid of it

sampleProgram' = [r|print Hello world!
ask What is your name?
echo Hi 
|]

pPrint :: Parser Stmt
pPrint = do
    keyword "print"
    expr <- someTill printChar eol
    return $ Print (Expr expr)
    
-- We could remove duplication here, but these will evolve separately anyway

pAsk :: Parser Stmt
pAsk = do
    keyword "ask"
    expr <- someTill printChar eol
    return $ Ask (Expr expr)

pEcho :: Parser Stmt
pEcho = do
    keyword "echo"
    expr <- someTill printChar eol
    return $ Echo (Expr expr)
    
pAll :: Parser Program
pAll = do
    sc -- for all space at the start of the file
    program <- many (pPrint <|> pAsk <|> pEcho)
--    eof
    return program

-- https://markkarpov.com/tutorial/megaparsec.html#lexing
-- space consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")
  
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keyword :: String -> Parser String
keyword s = lexeme (string s <* notFollowedBy alphaNumChar)