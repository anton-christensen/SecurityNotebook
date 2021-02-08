
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import LatticeViz.Types
import LatticeViz.DotGenerator
import LatticeViz.Parser

import Text.Parsec.Error

import Language.TinyARM
import Language.TinyARM.CFG

-- import TinyARM.Parser
-- import Language.While.Parser

import Snap.Core
import Snap.Http.Server
import GHC.Generics
import Data.Aeson as JSON
import Control.Monad.IO.Class
import Data.ByteString.Lazy.UTF8 as BLU
import Snap.Util.FileServe

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
analysisResponse :: GenericAnalysisResult -> AnalysisResponse
analysisResponse g = AnalysisResponse 
      { dot=gaDotGraph g
      , worklist=show $ gaWorkList g
      , stepsTaken=gaSteps g
      , success=True 
      }


data AnalysisErrResponse = AnalysisErrResponse
  { message :: String 
  , success :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)
errResponse :: String -> AnalysisErrResponse
errResponse msg = AnalysisErrResponse { message=msg, success=False }

maxBodyLen = 1000000

main :: IO ()
main = quickHttpServe $ route 
  [ ("/",       serveFile "static/index.html")
  , ("/static", serveDirectory "static")
  , ("/api",    requestHandler)
  ]

requestHandler :: Snap ()
requestHandler = method POST $ do 
    body <- readRequestBody maxBodyLen
    case requestHandler' body of
      Left  err -> writeLBS $ JSON.encode err
      Right res -> writeLBS $ JSON.encode res
  where

    requestHandler' :: ByteString -> Either AnalysisErrResponse AnalysisResponse
    requestHandler' body = 
      case (JSON.decode body) :: Maybe AnalysisRequest of
        Nothing -> Left $ errResponse "Invalid JSON object"
        Just req -> 
          case analysisName req of
            "TinyARM-reachingDefinitions" -> requestHandler_TinyARMReachingDefinitions req
            "TinyARM-liveness"            -> requestHandler_TinyARMLiveness req
            _                             -> Left $ errResponse "Invalid analysis name"


    requestHandler_TinyARMReachingDefinitions :: AnalysisRequest -> Either AnalysisErrResponse AnalysisResponse
    requestHandler_TinyARMReachingDefinitions req =
      case parseTinyARM $ programCode req of
        Left  err  -> Left $ errResponse $ "Parse error: " ++ (show err)
        Right prog -> Right $ analysisResponse $ reachingDefinitions prog (stepCount req)
    
    requestHandler_TinyARMLiveness :: AnalysisRequest -> Either AnalysisErrResponse AnalysisResponse
    requestHandler_TinyARMLiveness req =
      case parseTinyARM $ programCode req of
        Left  err  -> Left $ errResponse $ "Parse error: " ++ (show err)
        Right prog -> Right $ analysisResponse $ liveness prog (stepCount req)

