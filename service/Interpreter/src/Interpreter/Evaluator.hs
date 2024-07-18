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

evalLetIn :: [(ValName, Expr)] -> Expr -> Expr
evalLetIn defs expr = foldr (uncurry evalLetIn') (evalExpr expr) defs
  where
      evalLetIn' :: ValName -> Expr -> Expr -> Expr
      evalLetIn' name expr result = SingleApplyExpr (SingleLambdaExpr name result) (evalExpr expr)

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
