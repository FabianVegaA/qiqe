{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import GHC.IO.Handle.Types (Handle)
import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO (stdout)
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Mu.GRpc.Server
import Mu.Server

import Schema

import Interpreter.Token (Token)
import Interpreter.Lexer (lexer)
import Interpreter.Parser (parse, ParseError(..), AST)
import Interpreter.Evaluator (eval)
import Interpreter.Compiler (compile, CompileError(..))

debug, info :: MonadIO m => String -> m ()
info = liftIO . infoM "Codegen"
debug = liftIO . debugM "Codegen"

codegenStreamHandler :: IO (GenericHandler Handle)
codegenStreamHandler = let 
  formatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"
  in do 
    handler <- streamHandler stdout DEBUG 
    return $ setFormatter handler formatter

main :: IO ()
main = do
  handler <- codegenStreamHandler
  updateGlobalLogger "Codegen" (addHandler handler)
  updateGlobalLogger "Codegen" (setLevel DEBUG)

  info "Starting the gRPC server on 0.0.0.0:50051"
  runGRpcApp msgProtoBuf 50051 server

server :: ServerIO info InterpreterService _
server = singleService ( method @"run" $ run )

run :: InterpreterRequest -> ServerErrorIO InterpreterResponse
run (InterpreterRequest code) = alwaysOk $ do
  debug $ "Received code: " <> unpack code
  let ResultCodeGen _output _err _status = runCodeGen code
  debug $ "Output: " <> unpack (fromMaybe _err _output)
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
