
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Text.Parsec.Error

import Language.While as WHILE
import Language.TinyARM
import Language.TinyARM.CFG

import Language.LatticeViz

-- import TinyARM.Parser
-- import Language.While.Parser

import Snap.Core
import Snap.Http.Server
import GHC.Generics
import Data.Aeson as JSON
import Control.Monad.IO.Class
import Data.ByteString.Lazy.UTF8 as BLU
import Snap.Util.FileServe
import Data.HashMap.Strict (insert)


data ExecutionRequest = ExecutionRequest
  { programCode :: String
  , inputFile :: String
  } deriving (Show, Generic, ToJSON, FromJSON)


data ExecutionResponse = ExecutionResponse
  { message :: String
  , stepsTaken :: Int
  , success :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)


data AnalysisRequest = AnalysisRequest
  { programCode :: String
  , analysisName :: String
  , stepCount :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)


data AnalysisResponse = AnalysisResponse
  { dot :: String,
    worklist :: String,
    stepsTaken :: Int
  , success :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)
analysisResponse :: (Show l, Show p) => GenericAnalysisResult l p -> AnalysisResponse
analysisResponse g = AnalysisResponse 
      { dot=gaDotGraph g
      , worklist=show $ gaWorkList g
      , stepsTaken=gaSteps g
      , success=True 
      }


data ErrResponse = ErrResponse
  { message :: String 
  , success :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)
errResponse :: String -> ErrResponse
errResponse msg = ErrResponse { message=msg, success=False }

maxBodyLen = 1000000





data APIResponse = APIErrorReponse String
                 | APISuccessResponse 
                 deriving (Show, Generic, ToJSON, FromJSON)




main :: IO ()
main = quickHttpServe $ route 
  [ ("/",             serveFile "static/index.html")
  , ("/static",       serveDirectoryWith (defaultDirectoryConfig { mimeTypes = mimetypes }) "static")
  , ("/api",               requestHandler)
  , ("/api/while/execute", rhWhileExecute)
  , ("/api/lattice",       reqHandlerLattice)
  ]
  where 
    mimetypes = insert ".wasm" "application/wasm" defaultMimeTypes

rhWhileExecute :: Snap ()
rhWhileExecute = method POST $ do
  body <- readRequestBody maxBodyLen
  res <- liftIO $ case JSON.eitherDecode body :: Either String ExecutionRequest  of
      Left err -> return $ JSON.encode $ errResponse ("Invalid interpreter request: " ++ err)
      Right req -> case executeWhile (programCode (req :: ExecutionRequest)) (inputFile req) (-1) of
        Left err -> return $ JSON.encode $ errResponse err
        Right (msg, steps) -> return $ JSON.encode $ ExecutionResponse { message = msg, stepsTaken = steps, success = True }
  writeLBS res

reqHandlerLattice :: Snap ()
reqHandlerLattice = method POST $ do 
    body <- readRequestBody maxBodyLen
    res <- liftIO $ case latticeGraph $ BLU.toString body of
        Left err -> return err
        Right graphString -> graphString
    writeLBS $ BLU.fromString res

requestHandler :: Snap ()
requestHandler = method POST $ do 
    body <- readRequestBody maxBodyLen
    case requestHandler' body of
      Left  err -> writeLBS $ JSON.encode err
      Right res -> writeLBS $ JSON.encode res
  where

    requestHandler' :: ByteString -> Either ErrResponse AnalysisResponse
    requestHandler' body = 
      case (JSON.eitherDecode body) :: Either String AnalysisRequest of
        Left err -> Left $ errResponse $ "Invalid JSON object: " ++ err
        Right req -> 
          case analysisName req of
            "While-dummy"                 -> requestHandler_WhileDummyAnalysis req
            "While-taint"                 -> requestHandler_WhileTaintAnalysis req
            "TinyARM-reachingDefinitions" -> requestHandler_TinyARMReachingDefinitions req
            "TinyARM-liveness"            -> requestHandler_TinyARMLiveness req
            _                             -> Left $ errResponse "Invalid analysis name"


    requestHandler_WhileDummyAnalysis :: AnalysisRequest -> Either ErrResponse AnalysisResponse
    requestHandler_WhileDummyAnalysis req =
      case WHILE.parseWhile $ programCode (req :: AnalysisRequest) of
        Left  err  -> Left $ errResponse $ "Parse error: " ++ (show err)
        Right prog -> Right $ analysisResponse $ WHILE.dummyAnalysis prog (stepCount req)

    requestHandler_WhileTaintAnalysis :: AnalysisRequest -> Either ErrResponse AnalysisResponse
    requestHandler_WhileTaintAnalysis req =
      case WHILE.parseWhile $ programCode (req :: AnalysisRequest) of
        Left  err  -> Left $ errResponse $ "Parse error: " ++ (show err)
        Right prog -> Right $ analysisResponse $ WHILE.taintAnalysis prog (stepCount req)

    requestHandler_TinyARMReachingDefinitions :: AnalysisRequest -> Either ErrResponse AnalysisResponse
    requestHandler_TinyARMReachingDefinitions req =
      case parseTinyARM $ programCode (req :: AnalysisRequest) of
        Left  err  -> Left $ errResponse $ "Parse error: " ++ (show err)
        Right prog -> Right $ analysisResponse $ reachingDefinitions prog (stepCount req)
    
    requestHandler_TinyARMLiveness :: AnalysisRequest -> Either ErrResponse AnalysisResponse
    requestHandler_TinyARMLiveness req =
      case parseTinyARM $ programCode (req :: AnalysisRequest) of
        Left  err  -> Left $ errResponse $ "Parse error: " ++ (show err)
        Right prog -> Right $ analysisResponse $ liveness prog (stepCount req)

