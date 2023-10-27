{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}

module Schema where

import Data.Text (Text)
import GHC.Generics

import Mu.Schema
import Mu.Quasi.GRpc

grpc "InterpreterSchema" id "../protos/interpreter.proto"

data InterpreterRequest = InterpreterRequest
  { code :: Text
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   InterpreterSchema "InterpreterRequest"
             , FromSchema InterpreterSchema "InterpreterRequest" )

data InterpreterResponse = InterpreterResponse
  { output :: Text
  , error  :: Text
  , status :: Bool
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   InterpreterSchema "InterpreterResponse"
             , FromSchema InterpreterSchema "InterpreterResponse" )
