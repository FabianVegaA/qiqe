{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Data.Text (Text, pack, unpack)

import Mu.GRpc.Server
import Mu.Server

import Schema

import Interpreter.Lexer (lexer)
import Interpreter.Parser (parse, ParseError(..))

main :: IO ()
main = do
  putStrLn "Starting server on port 50051"
  runGRpcApp msgProtoBuf 50051 server

server :: ServerIO info InterpreterService _
server = singleService ( method @"run" $ run )

run :: InterpreterRequest -> ServerErrorIO InterpreterResponse
run (InterpreterRequest code) = alwaysOk $ do
  let (output, err, status) = runCode code
  pure $ InterpreterResponse output err status

runCode :: Text -> (Text, Text, Bool)
runCode code = case lexer $ unpack code of
  Left err -> ("", pack $ show err, False)
  Right res -> 
    -- (pack $ show res, "", True)
    case parse res of
      Right astRest -> (pack $ show astRest, "", True)
      Left FailedParser -> let 
        err' = pack $ show FailedParser
        msg = "Parse error with '" <> err' <> "'. Maybe the error is in the lexer? \n\tLexer output: " <> pack (show res)
        in ("", msg, False)
      Left (AmbiguousParse as) -> let
        defs = map fst as
        err' = pack $ show defs
        msg = "Ambiguous parse. Maybe the error is in the parser? \n\tParser output: " <> pack (show defs)
        in ("", msg, False)

