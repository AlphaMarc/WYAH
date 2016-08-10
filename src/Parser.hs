module Parser where

import           Data.Functor.Identity
import           Text.Parsec
import           Text.Parsec.Language  as Lang
import           Text.Parsec.Token     as Tok


-- definition of some properties of our language
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames Lang.haskellDef
  , Tok.reservedOpNames = reservedOpNames Lang.haskellDef
  , Tok.caseSensitive   = True
  }

type Parser a = Parsec String () a

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep ::
