module SimpleLang.Syntax where

import           Data.Functor.Identity
import           Data.Maybe            (fromMaybe)
import qualified SimpleLang.Parser     as P
import           Text.Parsec
import qualified Text.Parsec.Expr      as Ex
import qualified Text.Parsec.Token     as Tok

data Expr =
    Tr
    | Fl
    | Zero
    | IsZero Expr
    | Succ Expr
    | Pred Expr
    | If Expr Expr Expr
    deriving (Eq, Show)

-----------------
-- Parsing --
-----------------



prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (P.reservedOp s >> return f)

-- table of operations for our language
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      prefixOp "succ" Succ
    , prefixOp "pred" Pred
    , prefixOp "iszero" IsZero
    ]
  ]


-- Constants :
true, false, zero :: P.Parser Expr
true  = P.reserved "true" >> return Tr
false = P.reserved "false" >> return Fl
zero  = P.reserved "0" >> return Zero

ifthen :: P.Parser Expr
ifthen = do
    P.reserved "if"
    cond <- expr
    P.reservedOp "then"
    tr <- expr
    P.reserved "else"
    fl <- expr
    return (If cond tr fl)


factor :: P.Parser Expr
factor =
        true
    <|> false
    <|> zero
    <|> ifthen
    <|> P.parens expr

expr :: P.Parser Expr
expr = Ex.buildExpressionParser table factor


contents :: P.Parser a -> P.Parser a
contents p = do
      Tok.whiteSpace P.lexer
      r <- p
      eof
      return r


-- The toplevel function we'll expose from our Parse module is parseExpr
-- which will be called as the entry point in our REPL.
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"


-----------------
-- Evaluation --
-----------------
isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> eval' t
  Succ t                    -> Succ <$> eval' t
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> eval' t
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval' t
  _                         -> Nothing


-- we need that function to be able to evaluate multiple times
nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)



eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"
