{-# LANGUAGE FlexibleInstances #-}
{-# language OverloadedStrings     #-}

module Interpreter.CodeGen where

import Interpreter.Token (Token)
import Interpreter.Lexer (lexer)
import Interpreter.Parser (parse, ParseError(..), AST)
import Interpreter.Evaluator (eval)
import Interpreter.Compiler (compile, CompileError(..))
import qualified Data.Text as T

import Data.Aeson

data ResultCodeGen a = ResultCodeGen
  { resOutput :: Maybe a
  , resErr :: T.Text
  , resStatus :: Bool
  }

instance Functor ResultCodeGen where
  fmap f (ResultCodeGen (Just output) err status) = ResultCodeGen (Just (f output)) err status
  fmap _ (ResultCodeGen Nothing err status) = ResultCodeGen Nothing err status

instance Applicative ResultCodeGen where
  pure a = ResultCodeGen (Just a) "" True

  ResultCodeGen (Just f) err1 status1 <*> ResultCodeGen (Just a) err2 status2 = ResultCodeGen (Just (f a)) (err1 <> err2) (status1 && status2)
  ResultCodeGen Nothing err1 status1 <*> _ = ResultCodeGen Nothing err1 status1
  ResultCodeGen (Just _) err1 status1 <*> ResultCodeGen Nothing err2 status2 = ResultCodeGen Nothing (err1 <> err2) (status1 && status2)

instance Monad ResultCodeGen where
  ResultCodeGen Nothing err1 status1 >>= _ = ResultCodeGen Nothing err1 status1
  ResultCodeGen (Just a) err1 status1 >>= f =
    let ResultCodeGen b err2 status2 = f a
    in ResultCodeGen b (err1 <> err2) (status1 && status2)

instance ToJSON (ResultCodeGen T.Text) where
    toJSON (ResultCodeGen output err status) = object [ "output" .= output, "error" .= err, "status" .= status ]

runCodeGen :: T.Text -> ResultCodeGen T.Text
runCodeGen code = do
  tokens <- tryLexer code
  ast <- tryParse tokens
  ast' <- pure $ eval ast
  tryCompile ast'

  where
    tryLexer :: T.Text -> ResultCodeGen [Token]
    tryLexer src = case lexer $ T.unpack src of
      Left err -> ResultCodeGen Nothing (T.pack $ show err) False
      Right tokens -> pure tokens

    tryParse :: [Token] -> ResultCodeGen AST
    tryParse tokens = case parse tokens of
      Left FailedParser -> let
        msg = "Failed to parse the tokens. Perhaps there's a syntax error in your code."
        in ResultCodeGen Nothing msg False
      Left (AmbiguousParse ps) -> let
        defs = map fst ps
        msg = "Ambiguous parse. Conflicting definitions: " <> (T.show defs)
        in ResultCodeGen Nothing msg False
      Right ast -> pure ast

    tryCompile :: AST -> ResultCodeGen T.Text
    tryCompile ast = case compile ast of
      Left err -> let
        msg = case err of
          FailedCompiler -> "Failed to compile the AST."
          UnevaluatedAST expr -> "Failed to evaluate the expression: " <> (T.show expr)
          EmptyAST -> "The AST is empty."
        in ResultCodeGen Nothing msg False
      Right script -> pure script
