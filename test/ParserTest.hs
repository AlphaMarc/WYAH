module ParserTest where

import           LambdaCalc.Parser
import           LambdaCalc.Syntax
import           Test.QuickCheck
import           Text.Parsec

prop_ParseInt i = runParser number () "" (show i) == Right (Lit (LInt i))
                   where types = i :: Integer
