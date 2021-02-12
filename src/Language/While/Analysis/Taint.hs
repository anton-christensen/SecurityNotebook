module Language.While.Analysis.Taint where

import Analysis
import Language.While.Language
import Language.While.CFG
import Language.While.Analysis
import CFG

import Data.Lattice
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

data Taint = Tainted | Clean deriving (Eq, Show)
instance Lattice Taint where
    lbot = Clean
    ltop = Tainted
    ljoin Clean Clean = Clean
    ljoin a b = Tainted
    lmeet Tainted Tainted = Tainted
    lmeet a b = Clean
    
type TaintLatticeElem = M.Map Var Taint

prettyPrintState :: TaintLatticeElem -> String

prettyPrintState state = "[" ++ (intercalate ", " $ map showMapElm (M.toList state)) ++ "]"
  where 
    showMapElm :: (Var, Taint) -> String
    showMapElm (v, Tainted) = (show v) ++ " -> ⚡"
    showMapElm (v, Clean) = (show v) ++ " -> ⊥"


taintAnalysis :: Int -> WhileCFG -> WhileAnalysisResult TaintLatticeElem
taintAnalysis n cfg = 
  worklistAlgorithmSimpleForward n cfg initialState join transfer
  where
    initialState :: M.Map WhileCFGLabel TaintLatticeElem
    initialState = M.map (\_ -> bottom) cfg

    bottom :: TaintLatticeElem
    bottom = M.empty

    join :: [TaintLatticeElem] -> TaintLatticeElem
    join = ljoins

    transfer :: WhileCFGLabel -> WhileCFGNode -> TaintLatticeElem -> TaintLatticeElem
    transfer l (NDefault (INPUT v _)) inElm = M.insert v Tainted inElm
    transfer l (NDefault (ASGN v e)) inElm = M.insert v (
            ljoins $ map (\k -> M.findWithDefault Clean k inElm) (S.toList (extractVarsFromExpr e))
        ) inElm
    transfer l _ inElm = inElm


    


