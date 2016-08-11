module LambdaCalc.Pretty where

import           Text.PrettyPrint

-- we create a type class because we intend to pretty print over multiple data types
class Pretty p where
  ppr :: Int -> p -> Doc
  pp :: p -> Doc
  pp = ppr 0
