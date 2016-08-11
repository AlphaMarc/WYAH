module LambdaCalc.Parser where


import           Text.Parsec
import           Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token    as Tok

import           LambdaCalc.Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
          where style = haskellStyle { Tok.reservedNames = []
                                     , Tok.reservedOpNames = ["\\","->","+","-","*","/","="]
                                     , Tok.commentLine = "#"}



type Parser a = Parsec String () a

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

natural :: Parser Integer
natural = Tok.natural lexer

-- removes first whiteSpace then applies given parser until end of file
contents :: Parser a -> Parser a
contents p = do
        Tok.whiteSpace lexer
        r <- p
        eof
        return r

-- parse variable definition
variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

-- parse lambda
lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args





-- | parse a number
-- >>> runParser number () "" "1234"
-- Right (Lit (LInt 1234))
number :: Parser Expr
number = do
  n <- natural
  return $ Lit (LInt n)


-- | parse a boolean
-- >>> runParser number () "" "true"
-- Right (Lit (LBool True))
--
-- >>> runParser number () "" "false"
-- Right (Lit (LBool False))
bool :: Parser Expr
bool = true <|> false
    where true = do {reserved "true"; return $ Lit (LBool True)}
          false = do {reserved "false"; return $ Lit (LBool False)}


-- this is the unit parser for our untyped Lambda Calculus
term :: Parser Expr
term = parens expr
   <|> variable
   <|> number
   <|> lambda


-- applied many times to parse expression
expr :: Parser Expr
expr = do
    ts <- many1 term
    return $ foldl1 App ts


parseExpr :: String -> Either ParseError Expr
parseExpr = runParser (contents expr) () "<stdin>" 
