{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import           Control.Applicative
import           Control.Monad
import           Data.Char

data Parser a = Parser {parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> Either String a
runParser p s = case parse p s of
  [(res, [])] -> Right res
  [(_, str)] -> Left "the Parser failed to parse the entire text"
  _ -> Left "Parser error"

item :: Parser Char
item = Parser $ \s ->
    case s of
      [] -> []
      (s:ss) -> [(s,ss)]


instance Functor Parser where
  fmap f (Parser cs) = Parser (\s ->
                            do
                              (a,b) <- cs s
                              return (f a, b))


instance Applicative Parser where
  (Parser f) <*> (Parser g) = Parser (\s ->
                                do
                                  (k, s1) <- f s
                                  (a, s2) <- g s1
                                  return (k a, s2))

  pure a = Parser (\s -> [(a, s)])


instance Monad Parser where
  p >>= f = Parser (concatMap (\(a, s') -> parse (f a) s') . parse p)


-- allows us to use a parser q if parsing with p returned an empty list
instance Alternative Parser where
  empty = mzero
  p <|> q = Parser (\s ->
                 case parse p s of
                   [] -> parse q s
                   res  -> res)

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= (\a -> if f a then return a else mzero)

oneOf :: [Char] -> Parser Char
oneOf l = satisfy (`elem` l)


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                  <|> return a


char :: Char -> Parser Char
char c = satisfy (c==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string = mapM char
