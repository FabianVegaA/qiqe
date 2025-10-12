{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.CompilerWasm where

import Interpreter.Parser
import Data.Text (Text, intercalate, pack, unpack)
import qualified Data.Text as T
import Text.Printf (printf)
import Control.Monad (mapM)
import Data.List (nub)
import qualified Data.Map as Map

-- WebAssembly Types
data WasmType
    = I32Type
    | F32Type
    | F64Type
    | FuncType [WasmType] [WasmType]
    deriving (Show, Eq)

-- WebAssembly Instructions
data WasmInstruction
    = ConstI32 Int
    | ConstF32 Float
    | ConstF64 Double
    | LocalGet Int
    | LocalSet Int
    | LocalTee Int
    | GlobalGet String
    | GlobalSet String
    | Call String
    | CallIndirect WasmType
    | If WasmType [WasmInstruction] [WasmInstruction]
    | Block WasmType [WasmInstruction]
    | Loop WasmType [WasmInstruction]
    | Br Int
    | BrIf Int
    | Return
    | Drop
    | Select
    | I32Add | I32Sub | I32Mul | I32DivS | I32RemS
    | I32Eq | I32Ne | I32LtS | I32LeS | I32GtS | I32GeS
    | F32Add | F32Sub | F32Mul | F32Div
    | F32Eq | F32Ne | F32Lt | F32Le | F32Gt | F32Ge
    deriving (Show, Eq)

-- Compilation Context
data CompileContext = CompileContext
    { localVars :: Map.Map ValName Int
    , localCount :: Int
    , functionName :: String
    , nextLambdaId :: Int
    } deriving (Show)

-- WebAssembly Compiler
newtype CompilerWasm = CompilerWasm {runCompilerWasm :: AST -> Either CompileErrorWasm Text}

data CompileErrorWasm
    = FailedCompilerWasm
    | UnevaluatedASTWasm Expr
    | TypeMismatchWasm WasmType WasmType
    | UndefinedVariableWasm ValName
    | EmptyASTWasm
    deriving (Show)

-- Main compilation function
compileWasm :: AST -> Either CompileErrorWasm Text
compileWasm ast = runCompilerWasm evalWasm ast

evalWasm :: CompilerWasm
evalWasm = CompilerWasm evalASTWasm
  where
    evalASTWasm :: AST -> Either CompileErrorWasm Text
    evalASTWasm (AST defs) = do
      let ctx = CompileContext Map.empty 0 "main" 0
      (moduleBody, _) <- evalDefsWasm ctx defs
      pure $ wrapInModule moduleBody

-- Evaluate definitions
evalDefsWasm :: CompileContext -> [Definition] -> Either CompileErrorWasm (Text, CompileContext)
evalDefsWasm ctx [] = pure ("", ctx)
evalDefsWasm ctx (def:defs) = do
  (defCode, ctx') <- evalDefWasm ctx def
  (defsCode, ctx'') <- evalDefsWasm ctx' defs
  pure (defCode <> "\n" <> defsCode, ctx'')

evalDefWasm :: CompileContext -> Definition -> Either CompileErrorWasm (Text, CompileContext)
evalDefWasm ctx (ValueDefinition def) = evalValDefWasm ctx def
evalDefWasm ctx (ExprDefinition expr) = do
  (exprCode, exprType, ctx') <- evalExprWasm ctx expr
  let mainFunc = generateMainFunction exprCode exprType
  pure (mainFunc, ctx')

-- Evaluate value definitions (functions and constants)
evalValDefWasm :: CompileContext -> ValueDefinition -> Either CompileErrorWasm (Text, CompileContext)
evalValDefWasm ctx (NameDefinition name (SingleLambdaExpr arg expr)) = do
  let funcCtx = ctx { localVars = Map.fromList [(arg, 0)], localCount = 1, functionName = name }
  (bodyCode, bodyType, _) <- evalExprWasm funcCtx expr
  let funcDef = generateFunction name arg bodyCode bodyType
  pure (funcDef, ctx { nextLambdaId = nextLambdaId ctx + 1 })
evalValDefWasm ctx (NameDefinition name expr) = do
  (exprCode, exprType, ctx') <- evalExprWasm ctx expr
  let globalDef = generateGlobal name exprCode exprType
  pure (globalDef, ctx')

-- Evaluate expressions with type inference
evalExprWasm :: CompileContext -> Expr -> Either CompileErrorWasm (Text, WasmType, CompileContext)
evalExprWasm ctx (LitExpr lit) = do
  let (code, wasmType) = evalLitWasm lit
  pure (code, wasmType, ctx)
evalExprWasm ctx (IdentifierExpr name) = do
  case Map.lookup name (localVars ctx) of
    Just localIdx -> pure (pack $ printf "local.get %d" localIdx, I32Type, ctx)
    Nothing -> pure (pack $ printf "global.get $%s" name, I32Type, ctx)
evalExprWasm ctx (IfExpr e1 e2 e3) = do
  (e1Code, e1Type, ctx1) <- evalExprWasm ctx e1
  (e2Code, e2Type, ctx2) <- evalExprWasm ctx1 e2
  (e3Code, e3Type, ctx3) <- evalExprWasm ctx2 e3
  if e2Type == e3Type
    then do
      let ifCode = pack $ printf "%s\n(if (result %s)\n  (then %s)\n  (else %s)\n)"
                          e1Code (wasmTypeToText e2Type) e2Code e3Code
      pure (ifCode, e2Type, ctx3)
    else Left $ TypeMismatchWasm e2Type e3Type
evalExprWasm ctx (SingleLambdaExpr arg expr) = do
  let lambdaName = "lambda_" ++ show (nextLambdaId ctx)
  let lambdaCtx = ctx { localVars = Map.fromList [(arg, 0)], localCount = 1, functionName = lambdaName }
  (bodyCode, bodyType, _) <- evalExprWasm lambdaCtx expr
  let lambdaFunc = generateFunction lambdaName arg bodyCode bodyType
  let funcRef = pack $ printf "i32.const %d" (nextLambdaId ctx)
  pure (lambdaFunc <> "\n" <> funcRef, FuncType [I32Type] [bodyType], ctx { nextLambdaId = nextLambdaId ctx + 1 })
evalExprWasm ctx (SingleApplyExpr expr1 expr2) = do
  (funcCode, funcType, ctx1) <- evalExprWasm ctx expr1
  (argCode, argType, ctx2) <- evalExprWasm ctx1 expr2
  case funcType of
    FuncType [expectedArgType] [retType] ->
      if argType == expectedArgType
        then do
          let callCode = pack $ printf "%s\n%s\ncall_indirect (type $func_type)"
                                argCode funcCode
          pure (callCode, retType, ctx2)
        else Left $ TypeMismatchWasm expectedArgType argType
    _ -> do
      -- Assume it's a named function call
      let callCode = pack $ printf "%s\ncall $%s" argCode (extractFuncName funcCode)
      pure (callCode, I32Type, ctx2)
evalExprWasm ctx (BinOpExpr op e1 e2) = do
  (e1Code, e1Type, ctx1) <- evalExprWasm ctx e1
  (e2Code, e2Type, ctx2) <- evalExprWasm ctx1 e2
  if e1Type == e2Type
    then do
      let (opCode, resultType) = evalBinOpWasm op e1Type
      let binCode = pack $ printf "%s\n%s\n%s" e1Code e2Code opCode
      pure (binCode, resultType, ctx2)
    else Left $ TypeMismatchWasm e1Type e2Type
evalExprWasm ctx (ListExpr exprs) = do
  -- Simplified list handling - just evaluate all expressions and return the last one
  case exprs of
    [] -> pure ("i32.const 0", I32Type, ctx)
    [expr] -> evalExprWasm ctx expr
    (expr:rest) -> do
      (_, _, ctx1) <- evalExprWasm ctx expr
      evalExprWasm ctx1 (ListExpr rest)
evalExprWasm _ expr = Left $ UnevaluatedASTWasm expr

-- Evaluate literals
evalLitWasm :: Literal -> (Text, WasmType)
evalLitWasm (IntLitExpr i) = (pack $ printf "i32.const %d" i, I32Type)
evalLitWasm (FloatLitExpr f) = (pack $ printf "f32.const %f" f, F32Type)
evalLitWasm (BoolLitExpr True) = ("i32.const 1", I32Type)
evalLitWasm (BoolLitExpr False) = ("i32.const 0", I32Type)
evalLitWasm (StringLitExpr s) =
  -- Simplified string handling - return pointer to string in linear memory
  (pack $ printf "i32.const %d" (hash s), I32Type)
  where hash = abs . foldr (\c acc -> fromEnum c + acc * 31) 0
evalLitWasm NilLitExpr = ("i32.const 0", I32Type)

-- Evaluate binary operations
evalBinOpWasm :: BinOp -> WasmType -> (Text, WasmType)
evalBinOpWasm LPipeExpr I32Type = ("call $pipe_i32", I32Type)
evalBinOpWasm RPipeExpr I32Type = ("call $rpipe_i32", I32Type)
evalBinOpWasm LComposeExpr _ = ("call $compose", FuncType [I32Type] [I32Type])
evalBinOpWasm RComposeExpr _ = ("call $rcompose", FuncType [I32Type] [I32Type])

-- Helper functions
wasmTypeToText :: WasmType -> Text
wasmTypeToText I32Type = "i32"
wasmTypeToText F32Type = "f32"
wasmTypeToText F64Type = "f64"
wasmTypeToText (FuncType _ _) = "funcref"

extractFuncName :: Text -> String
extractFuncName code =
  -- Simple function name extraction - in practice, this would be more sophisticated
  case T.words code of
    (name:_) -> T.unpack name
    [] -> "unknown"

generateFunction :: String -> String -> Text -> WasmType -> Text
generateFunction name arg bodyCode bodyType =
  pack $ printf "(func $%s (param $%s i32) (result %s)\n  %s\n)"
                name arg (wasmTypeToText bodyType) (T.unpack bodyCode)

generateGlobal :: String -> Text -> WasmType -> Text
generateGlobal name initCode wasmType =
  pack $ printf "(global $%s (mut %s) (%s))"
                name (wasmTypeToText wasmType) (T.unpack initCode)

generateMainFunction :: Text -> WasmType -> Text
generateMainFunction bodyCode bodyType =
  pack $ printf "(func $main (result %s)\n  %s\n)\n(export \"main\" (func $main))"
                (wasmTypeToText bodyType) (T.unpack bodyCode)

wrapInModule :: Text -> Text
wrapInModule moduleBody =
  "(module\n" <>
  "  ;; Function type definitions\n" <>
  "  (type $func_type (func (param i32) (result i32)))\n" <>
  "  \n" <>
  "  ;; Linear memory\n" <>
  "  (memory $mem 1)\n" <>
  "  (export \"memory\" (memory $mem))\n" <>
  "  \n" <>
  "  ;; Function table for indirect calls\n" <>
  "  (table $func_table 100 funcref)\n" <>
  "  \n" <>
  "  ;; Helper functions for pipe operations\n" <>
  "  (func $pipe_i32 (param $x i32) (param $f i32) (result i32)\n" <>
  "    local.get $x\n" <>
  "    local.get $f\n" <>
  "    call_indirect (type $func_type)\n" <>
  "  )\n" <>
  "  \n" <>
  "  (func $rpipe_i32 (param $f i32) (param $x i32) (result i32)\n" <>
  "    local.get $x\n" <>
  "    local.get $f\n" <>
  "    call_indirect (type $func_type)\n" <>
  "  )\n" <>
  "  \n" <>
  "  ;; User-defined functions and globals\n" <>
  moduleBody <>
  "\n)"

-- Variable name sanitization for WebAssembly
sanitizeName :: ValName -> Text
sanitizeName "eval" = "eval__qq"
sanitizeName name =
  let suffix_name = pack $ printf "%s__qq" name
  in T.replace "'" "__prime__" suffix_name
