{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Compiler where

import Interpreter.Parser
import Data.Text (Text, intercalate, pack, null)

import Prelude hiding (null)

newtype Compiler = Compiler {runCompiler :: AST -> Text}

data CompileError
    = FailedCompiler
    deriving (Show)

compile :: AST -> Either CompileError Text
compile ast = 
  if null code 
    then Left FailedCompiler 
    else Right code
  where
    code = runCompiler eval ast

eval :: Compiler
eval = Compiler evalAST
  where
    evalAST (AST defs) = intercalate ";" $ map evalDef defs

    evalDef (ValueDefinition def) = evalValDef def
    evalDef (ExprDefinition expr) = evalExpr expr

evalExpr :: Expr -> Text
evalExpr (LitExpr lit) = evalLit lit
evalExpr (IdentifierExpr name) = evalName name
evalExpr (IfExpr e1 e2 e3) = "(" <> evalExpr e1 <> ")?" <> evalExpr e2 <> ":" <> evalExpr e3
evalExpr (LambdaExpr names expr) = foldr (\name acc -> "((" <> evalName name <> ")=>" <> acc <> ")") (evalExpr expr) names
evalExpr (ApplyExpr expr exprs) = "(" <> evalExpr expr <> ")" <> foldMap (\e -> "(" <> evalExpr e <> ")") exprs
evalExpr op@(BinOpExpr _ _ _) = evalOpt op

evalOpt :: Expr -> Text
evalOpt (BinOpExpr op e1 e2) = case op of
  LComposeExpr -> evalCompose e1 e2
  RComposeExpr -> evalCompose e2 e1
  LPipeExpr -> evalPipe e2 e1
  RPipeExpr ->  evalPipe e1 e2
  where
    evalCompose f1 f2 = "((x)=>((" <> evalExpr f1 <>  ")(" <> evalExpr f2 <>"(x))))"
    evalPipe e1 e2 = evalExpr (ApplyExpr e2 [e1])

evalValDef :: ValueDefinition -> Text
evalValDef (NameDefinition name expr) = "const " <> evalName name <> "=" <> evalExpr expr

evalLit :: Literal -> Text
evalLit (IntLitExpr i) = pack $ show i
evalLit (FloatLitExpr f) = pack $ show f
evalLit (BoolLitExpr b) = if b then "true" else "false"
evalLit (StringLitExpr s) = "\"" <> pack s <> "\""

evalName :: ValName -> Text
evalName = pack
