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
