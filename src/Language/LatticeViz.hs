module Language.LatticeViz where

import Language.LatticeViz.Parser
import Language.LatticeViz.Types
import Language.LatticeViz.DotGenerator

import Control.Monad.IO.Class

latticeGraph :: String -> Either String (IO String)
latticeGraph code = 
    case parseLatticeLanguage code of
        Left error -> Left error
        Right lattice -> Right $ do
            graph <- latticeToGraph lattice
            return $ printGraph graph 
