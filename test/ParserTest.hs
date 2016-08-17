module ParserTest where

import           Data.Char

import           LambdaCalc.Parser
import           LambdaCalc.Syntax

import           Text.Parsec

prop_ParseInt i = runParser number () "" (show i) == Right (Lit (LInt i))
                   where types = i :: Integer

prop_ParseBool b = runParser bool () "" (map toLower . show $ b) == Right (Lit (LBool b))
                   where types = b :: Bool
