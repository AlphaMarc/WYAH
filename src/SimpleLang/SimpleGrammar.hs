module SimpleLang.SimpleGrammar where


import           Control.Applicative
import           Control.Monad
import           SimpleLang.NanoParsec

--- We can now define our own little language our base grammar is based arount the Expr type
data Expr =
  Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int

eval :: Expr -> Int
eval e = case e of
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y
  Sub x y -> eval x - eval y
  Lit x   -> x

int :: Parser Expr
int = fmap Lit number

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixop :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixop s op = reserved s >> return op

addop :: Parser (Expr -> Expr ->Expr)
addop = infixop "+" Add <|> infixop "-" Sub

mulop :: Parser (Expr -> Expr ->Expr)
mulop = infixop "*" Mul

run :: String -> Either String Expr
run = runParser expr

repl :: IO ()
repl = forever $ do
  putStr "> "
  a <- getLine
  print . either id (show . eval) $ run a
