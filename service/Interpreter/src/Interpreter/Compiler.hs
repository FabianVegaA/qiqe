{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Compiler where

import Interpreter.Parser
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import Text.Printf (printf)
import Control.Monad (mapM)

newtype Compiler = Compiler {runCompiler :: AST -> Either CompileError Text}

data CompileError
    = FailedCompiler
    | UnevaluatedAST Expr
    | EmptyAST
    deriving (Show)

compile :: AST -> Either CompileError Text
compile ast = runCompiler eval ast

eval :: Compiler
eval = Compiler evalAST
  where
    evalAST :: AST -> Either CompileError Text
    evalAST (AST defs) = do
      defs <- mapM evalDef defs
      pure $ intercalate ";\n" defs

    evalDef :: Definition -> Either CompileError Text
    evalDef (ValueDefinition def) = evalValDef def
    evalDef (ExprDefinition expr) = evalExpr expr

evalExpr :: Expr -> Either CompileError Text
evalExpr (LitExpr lit) = pure $ evalLit lit
evalExpr (IdentifierExpr name) = pure $ evalName name
evalExpr (IfExpr e1 e2 e3) = do
  e1Gen <- evalExpr e1
  e2Gen <- evalExpr e2
  e3Gen <- evalExpr e3
  pure . pack $ printf "(%s)?(%s):(%s)" e1Gen e2Gen e3Gen
evalExpr (SingleLambdaExpr name expr) = do 
  bodyGen <- evalExpr expr
  pure . pack $ printf "(%s)=>(%s)" (evalName name) bodyGen
evalExpr (SingleApplyExpr expr1 expr2) = do 
  funcGen <- evalExpr expr1
  argGen <- evalExpr expr2
  pure . pack $ printf "(%s)(%s)" funcGen argGen
evalExpr expr = Left $ UnevaluatedAST expr

evalValDef :: ValueDefinition -> Either CompileError Text
evalValDef (NameDefinition name expr) = do 
  assignGen <- evalExpr expr
  pure . pack $ printf "const %s=%s" (evalName name) assignGen

evalLit :: Literal -> Text
evalLit (IntLitExpr i) = pack $ show i
evalLit (FloatLitExpr f) = pack $ show f
evalLit (BoolLitExpr b) = if b then "true" else "false"
evalLit (StringLitExpr s) = pack $ printf "\"%s\"" s
evalLit NilLitExpr = "null"

evalName :: ValName -> Text
evalName "eval" = pack "eval"
evalName val = let 
  suffix_name = pack $ printf "%s__qq" val
  in T.replace "'" "__prime__" suffix_name
