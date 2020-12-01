
{-# LANGUAGE OverloadedStrings #-}
module Main where

import LatticeViz.Types
import LatticeViz.DotGenerator
import LatticeViz.Parser

-- main :: IO ()
-- main = do
--     let
--         set = ["x", "y", "z"]
--         poset = LatPoset [Pair "⊥" "-", Pair "-" "⊤"]
--         powerset = LatPowerset set
--         product = LatProduct powerset poset
--         map = LatMap set poset
--     graph <- latticeToGraph map
--     putStrLn $ printGraph graph


-- main :: IO ()
-- main = do
--     file <- getContents
--     case parseLatticeNotation file of
--         Left err -> putStrLn err
--         Right lattice -> do
--             graph <- latticeToGraph lattice
--             putStrLn $ printGraph graph


import Snap.Core
import Snap.Http.Server
import Control.Monad.IO.Class
import Data.ByteString.Lazy.UTF8 as BLU
import Snap.Util.FileServe

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
    res <- liftIO $ case parseLatticeNotation $ BLU.toString body of
        Left err -> return err
        Right lattice -> do
            graph <- latticeToGraph lattice
            return $ printGraph graph
    writeLBS $ BLU.fromString res
     
