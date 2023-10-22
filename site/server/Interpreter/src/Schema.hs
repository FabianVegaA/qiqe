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

-- import Data.Text as T
-- import GHC.Generics

import Mu.Quasi.GRpc
import Mu.Schema

grpc "TheSchema" id "Interpreter.proto"

-- A. Map to Haskell types
-- data Message
--   = Message { ... }
--   deriving ( Eq, Show, Generic
--            , ToSchema   TheSchema "Message"
--            , FromSchema TheSchema "Message" )

-- B. Use optics
type Message = Term TheSchema (TheSchema :/: "Message")
