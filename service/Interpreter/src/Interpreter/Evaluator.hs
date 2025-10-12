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
-- | Case for mutual recursion (implemented)
-- |  For mutually recursive functions, we apply the fix combinator to each function
-- |  but evaluate them in an environment where all mutually recursive names are bound.
-- |  Example: let f = λx. g x; g = λy. f y in f 1
-- |  This becomes: (λf. (λg. f 1) (fix g')) (fix f')
-- |  where f' and g' have access to each other through the nested lambdas
evalLetIn :: [(ValName, Expr)] -> Expr -> Expr
evalLetIn defs expr = evalLetInWithEnv defs (evalExpr expr)
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

      -- Check if an expression references a given name (used for detecting recursion)
      references :: ValName -> Expr -> Bool
      references name (IdentifierExpr n) = n == name
      references name (LambdaExpr names e) = name `notElem` names && references name e
      references name (SingleLambdaExpr n e) = n /= name && references name e
      references name (ApplyExpr e es) = references name e || any (references name) es
      references name (SingleApplyExpr e1 e2) = references name e1 || references name e2
      references name (IfExpr e1 e2 e3) = references name e1 || references name e2 || references name e3
      references name (BinOpExpr _ e1 e2) = references name e1 || references name e2
      references name (ListExpr es) = any (references name) es
      references name (LetInExpr innerDefs e) =
        let boundNames = map fst innerDefs
        in (name `notElem` boundNames && references name e) ||
           any (\(n, ex) -> n /= name && references name ex) innerDefs
      references _ _ = False

      -- Evaluate let-in with proper handling of mutual recursion
      evalLetInWithEnv :: [(ValName, Expr)] -> Expr -> Expr
      evalLetInWithEnv [] result = result
      evalLetInWithEnv allDefs result =
        let defNames = map fst allDefs
            -- For each definition, check if it references any name in the let block
            isRecursive (name, defExpr) =
              any (\otherName -> references otherName defExpr) defNames
            -- Separate recursive and non-recursive definitions
            (recursiveDefs, nonRecursiveDefs) =
              let rec = filter isRecursive allDefs
                  nonRec = filter (not . isRecursive) allDefs
              in (rec, nonRec)
            -- Process non-recursive definitions first (simple substitution)
            withNonRec = foldr (\(name, defExpr) r ->
                                  apply (lambda name r) (evalExpr defExpr))
                               result nonRecursiveDefs
            -- For recursive definitions, apply fix combinator in a shared environment
            -- The key is to bind all recursive names simultaneously
            withRec = foldr (\(name, defExpr) r ->
                              apply (lambda name r)
                                    (if isLambdaDef defExpr
                                     then fix name (evalExpr defExpr)
                                     else evalExpr defExpr))
                            withNonRec recursiveDefs
        in withRec
        where
          isLambdaDef (LambdaExpr _ _) = True
          isLambdaDef _ = False


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
