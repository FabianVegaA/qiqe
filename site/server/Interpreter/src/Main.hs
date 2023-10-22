{-# language FlexibleContexts      #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Mu.GRpc.Server
import Mu.Server

import Schema

main :: IO ()
main = do
    putStrLn "Starting Server"
    runGRpcApp msgProtoBuf 8080 server

server :: MonadServer m => SingleServerT i Service m _
server = singleService ()