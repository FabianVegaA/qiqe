{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)

import Mu.GRpc.Server
import Mu.Server

import Schema

import Interpreter.Token (Token)
import Interpreter.Lexer (lexer)
import Interpreter.Parser (parse, ParseError(..), AST)
import Interpreter.Evaluator (eval)
import Interpreter.Compiler (compile, CompileError(..))

main :: IO ()
main = do
  putStrLn "Starting server on port 50051"
  runGRpcApp msgProtoBuf 50051 server

server :: ServerIO info InterpreterService _
server = singleService ( method @"run" $ run )

run :: InterpreterRequest -> ServerErrorIO InterpreterResponse
run (InterpreterRequest code) = alwaysOk $ do
  _ <- liftIO $ putStrLn $ "Received code: " <> unpack code
  let ResultCodeGen _output _err _status = runCodeGen code
  _ <- liftIO $ putStrLn $ "Output: " <> unpack (fromMaybe mempty _output)
  pure $ InterpreterResponse (fromMaybe "" _output) _err _status

data ResultCodeGen a = ResultCodeGen
  { resOutput :: Maybe a
  , resErr :: Text
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

runCodeGen :: Text -> ResultCodeGen Text
runCodeGen code = do
  tokens <- tryLexer code
  ast <- tryParse tokens
  ast' <- pure $ eval ast
  tryCompile ast'

  where 
    tryLexer :: Text -> ResultCodeGen [Token]
    tryLexer src = case lexer $ unpack src of
      Left err -> ResultCodeGen Nothing (pack $ show err) False
      Right tokens -> pure tokens

    tryParse :: [Token] -> ResultCodeGen AST
    tryParse tokens = case parse tokens of
      Left FailedParser -> let
        msg = "Failed to parse the tokens. Perhaps there's a syntax error in your code."
        in ResultCodeGen Nothing msg False
      Left (AmbiguousParse ps) -> let
        defs = map fst ps
        msg = "Ambiguous parse. Conflicting definitions: " <> pack (show defs)
        in ResultCodeGen Nothing msg False
      Right ast -> pure ast

    tryCompile :: AST -> ResultCodeGen Text
    tryCompile ast = case compile ast of 
      Left err -> let
        msg = case err of
          FailedCompiler -> "Failed to compile the AST."
          UnevaluatedAST expr -> "Failed to evaluate the expression: " <> pack (show expr)
          EmptyAST -> "The AST is empty."
        in ResultCodeGen Nothing msg False
      Right script -> pure script
