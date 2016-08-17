module LambdaCalc.Eval where

import           Data.Map          as Map

import           LambdaCalc.Syntax


data Value =
  VInt Integer
  | VBool Bool
  | VClosure String Expr Scope

type Scope = Map String Value

emptyScope = Map.empty


instance Show Value where
  show (VInt a) = show a
  show (VBool a) = show a
  show (VClosure s ex sc) = "<<closure>>"


-- | evaluate a simple expression
-- >>> eval emptyScope (Lit (LInt 1))
-- 1
-- >>> eval emptyScope (Lit (LBool True))
-- True
-- >>> eval (extend emptyScope "x" (VInt 1)) (Var "x")
-- 1
--
-- (\x . x) 1
-- >>> eval emptyScope (App (Lam "x" (Var "x")) (Lit (LInt 1)))
-- 1
--
-- (\x y . x) 1 2
-- >>> eval emptyScope (App (App (Lam "x" (Lam "y" (Var "x"))) (Lit (LInt 1))) (Lit (LInt 2)))
-- 1
--
-- (\x y z . x z (y z)) (\x y . x) (\x y . x)
-- >>> eval emptyScope (App (App (Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))) (Lam "x" (Lam "y" (Var "x")))) (Lam "x" (Lam "y" (Var "x"))))
-- <<closure>>
eval :: Scope -> Expr -> Value
eval _ (Lit (LInt x))  = VInt x
eval _ (Lit (LBool b)) = VBool b
eval env (Var name)    = env ! name
eval env (Lam x body)  = VClosure x body env
eval env (App a b)     = betaRed (eval env a) (eval env b)

betaRed :: Value -> Value -> Value
betaRed (VClosure name exp1 scope) exp2 =
  eval (extend scope name exp2) exp1
betaRed _ _ = error "tried to evaluate a non closure"

-- | this function extends the scope with another local variable
extend :: Scope -> String -> Value -> Scope
extend sc s v = insert s v sc

-- | this function runs the evaluator on the given expression
runEval :: Expr -> String
runEval = show . eval emptyScope
