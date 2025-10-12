{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
import Yesod

import GHC.IO.Handle.Types (Handle)
import Data.Text (Text, pack, unpack)
import Control.Monad.IO.Class (liftIO)
import System.IO (stdout)
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Data.Aeson
import Yesod.Core.Json (parseCheckJsonBody)
import Yesod.Core.Handler (invalidArgs)
import Data.Aeson.Types (Result(..), Parser, parseEither, withObject)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (readFile)
import System.Directory (doesFileExist)

import Interpreter.CodeGen (runCodeGen, ResultCodeGen(..))

data App = App

mkYesod "App" [parseRoutes|
/codegen CodegenR POST
/lib LibR POST
|]

instance Yesod App

-- Request types
newtype CodegenRequest = CodegenRequest { code :: Text } deriving Show
newtype LibraryRequest = LibraryRequest { filename :: Text } deriving Show

-- Request parsers
codegenRequestParser :: Value -> Parser CodegenRequest
codegenRequestParser = withObject "CodegenRequest" (\obj -> do
                          code <- obj .: "code"
                          return $ CodegenRequest code)

libraryRequestParser :: Value -> Parser LibraryRequest
libraryRequestParser = withObject "LibraryRequest" (\obj -> do
                          filename <- obj .: "filename"
                          return $ LibraryRequest filename)

-- Codegen endpoint handler
postCodegenR :: Handler Value
postCodegenR = do
  (result :: Result Value) <- parseCheckJsonBody
  case result of
    Error error ->
      invalidArgs [pack error]
    Success value -> do
      case parseEither codegenRequestParser value of
            Left error ->
              invalidArgs [pack error]
            Right (CodegenRequest code) -> do
              currentTime <- liftIO getCurrentTime
              let timeStr = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime
              let codegenResult = runCodeGen code
              let responseId = 1 -- Simple ID for now, could be improved with proper ID generation
              case codegenResult of
                ResultCodeGen (Just output) err status -> 
                  return $ object [ "id" .= responseId
                                  , "result" .= output
                                  , "status" .= status
                                  , "error" .= err
                                  , "createdAt" .= timeStr
                                  ]
                ResultCodeGen Nothing err status ->
                  return $ object [ "id" .= responseId
                                  , "result" .= ("" :: Text)
                                  , "status" .= status
                                  , "error" .= err
                                  , "createdAt" .= timeStr
                                  ]

-- Library import endpoint handler
postLibR :: Handler Value
postLibR = do
  (result :: Result Value) <- parseCheckJsonBody
  case result of
    Error error ->
      invalidArgs [pack error]
    Success value -> do
      case parseEither libraryRequestParser value of
            Left error ->
              invalidArgs [pack error]
            Right (LibraryRequest filename) -> do
              let filepath = "/app/qiqe/library/" ++ unpack filename
              fileExists <- liftIO $ doesFileExist filepath
              if fileExists
                then do
                  content <- liftIO $ System.IO.readFile filepath
                  return $ object [ "target_code" .= pack content
                                  , "status" .= True
                                  , "error" .= ("" :: Text)
                                  ]
                else
                  return $ object [ "target_code" .= ("" :: Text)
                                  , "status" .= False
                                  , "error" .= ("File not found: " <> filename)
                                  ]

debug, info :: MonadIO m => String -> m ()
info = liftIO . infoM "Codegen"
debug = liftIO . debugM "Codegen"

codegenStreamHandler :: IO (GenericHandler Handle)
codegenStreamHandler = let 
  formatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"
  in do 
    handler <- streamHandler stdout DEBUG 
    return $ setFormatter handler formatter

main = do 
    handler <- codegenStreamHandler
    updateGlobalLogger "Codegen" (addHandler handler)
    updateGlobalLogger "Codegen" (setLevel DEBUG)

    info "Running Codegen in 0.0.0.0:8000"
    warp 8000 App