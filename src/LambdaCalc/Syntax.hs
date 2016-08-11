module LambdaCalc.Syntax where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving (Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show)
