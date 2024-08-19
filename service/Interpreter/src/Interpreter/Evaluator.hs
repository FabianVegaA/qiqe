module Interpreter.Evaluator where

import Interpreter.Parser

newtype Evaluator = Evaluator {runEvaluator :: Expr -> Expr}

eval :: AST -> AST
eval (AST defs) = AST $ map evalDef defs

evalDef :: Definition -> Definition
evalDef (ValueDefinition def) = ValueDefinition $ evalValDef def
evalDef (ExprDefinition expr) = ExprDefinition $ evalExpr expr

evalValDef :: ValueDefinition -> ValueDefinition
evalValDef (NameDefinition name expr) = NameDefinition name $ evalExpr expr

evalExpr :: Expr -> Expr
evalExpr (LambdaExpr names expr) = foldr SingleLambdaExpr (evalExpr expr) names
evalExpr (ApplyExpr expr exprs) = foldl SingleApplyExpr (evalExpr expr) (map evalExpr exprs)
evalExpr op@(BinOpExpr _ _ _) = evalOpt op
evalExpr (IfExpr e1 e2 e3) = IfExpr (evalExpr e1) (evalExpr e2) (evalExpr e3)
evalExpr (ListExpr exprs) = evalList $ map evalExpr exprs
evalExpr (LetInExpr defs expr) = evalLetIn defs expr
evalExpr expr = expr -- TODO: Should I transform the literals to lambda expressions?


-- | Evaluate let in expressions
-- | Case for single let in expression
-- |  let x = 1 in x + 1 => (λx.x + 1) 1 => 1 + 1 => 2
-- | Case for multiple let in expressions
-- |  let x = 1; y = 2 in x + y => (λx. (λy. x + y) 2) 1 => (λy. 1 + y) 2 => 1 + 2 => 3
-- | Case for single let in expression with recursion
-- |  This use the applicative order U combinator to avoid infinite recursion
-- |  let f = λx. f x in f 1 => ((λu. u u)(λf. (λx. (f f) x))) 1
-- TODO: Implement the mutual recursion
evalLetIn :: [(ValName, Expr)] -> Expr -> Expr
evalLetIn defs expr = foldr (uncurry evalLetIn') (evalExpr expr) defs
  where
      apply :: Expr -> Expr -> Expr
      apply e1 e2 = SingleApplyExpr e1 e2

      lambda :: String -> Expr -> Expr
      lambda arg e1 = SingleLambdaExpr arg e1

      get :: String -> Expr
      get name = IdentifierExpr name

      delta :: Expr
      delta = lambda "u" $ apply (get "u") (get "u")

      fix :: ValName -> Expr -> Expr
      fix name expr = apply delta $  
        lambda "self" $ apply
          (lambda name expr)
          (lambda "x" $ apply (apply (get "self") (get "self")) $ get "x")

      evalLetIn' :: ValName -> Expr -> Expr -> Expr
      evalLetIn' name expr result = case expr of
        (LambdaExpr _ _) ->
          apply (lambda name result) $ fix name (evalExpr expr)
        _ -> apply (lambda name result) (evalExpr expr)


evalList :: [Expr] -> Expr
evalList [] = LitExpr NilLitExpr
evalList (x:xs) = evalExpr $ ApplyExpr cons [x, evalList xs]
  where
    nil :: Expr
    nil = LitExpr NilLitExpr

    cons :: Expr
    cons = evalExpr $ LambdaExpr ["h", "t", "x"] (
        IfExpr 
          (IdentifierExpr "x") 
          (IdentifierExpr "h") 
          (IdentifierExpr "t")
      )

evalOpt :: Expr -> Expr
evalOpt (BinOpExpr op e1 e2) = let 
  e1' = evalExpr e1
  e2' = evalExpr e2
  in case op of
    LComposeExpr -> evalCompose e1' e2'
    RComposeExpr -> evalCompose e2' e1'
    LPipeExpr -> evalPipe e2' e1'
    RPipeExpr ->  evalPipe e1' e2'
  where
    evalCompose f1 f2 = SingleLambdaExpr "x" (SingleApplyExpr f1 (SingleApplyExpr f2 (IdentifierExpr "x")))
    evalPipe e1 e2 = SingleApplyExpr e2 e1
