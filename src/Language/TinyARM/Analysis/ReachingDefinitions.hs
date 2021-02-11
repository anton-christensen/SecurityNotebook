module Language.TinyARM.Analysis.ReachingDefinitions where

import Analysis
import Language.TinyARM.Language
import Language.TinyARM.CFG
import Language.TinyARM.Analysis
import CFG

import Data.Lattice
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S


type RDLatticeElem = M.Map Register (S.Set TinyArmCFGLabel)

prettyPrintState :: RDLatticeElem -> String
prettyPrintState state = "[" ++ (intercalate ", " $ map showMapElm (M.toList state)) ++ "]"
  where 
    showMapElm :: (Register, S.Set TinyArmCFGLabel) -> String
    showMapElm (r, s) = "" ++ (show r) ++ "->" ++ (showSet s) ++  ""
    showSet :: S.Set TinyArmCFGLabel -> String
    showSet s = "{" ++ (intercalate ", " $ map show $ S.toList s) ++ "}"

reachingDefinitionsAnalysis :: Int -> TinyArmCFG -> TinyArmAnalysisResult RDLatticeElem
reachingDefinitionsAnalysis n cfg = 
  worklistAlgorithmSimpleForward n cfg initialState join transfer
  where
    initialState :: M.Map TinyArmCFGLabel RDLatticeElem
    initialState = M.map (\_ -> bottom) cfg

    bottom :: RDLatticeElem
    bottom = M.empty

    join :: [RDLatticeElem] -> RDLatticeElem
    join [] = bottom
    join (x:xs) = ljoin x $ join xs

    transfer :: TinyArmCFGLabel -> TinyArmCFGNode -> RDLatticeElem -> RDLatticeElem
    transfer l (NDefault (IMOV _ _ r _)) inElm = transferAssign l r inElm
    transfer l (NDefault (IBINOP _ _ _ r _ _)) inElm = transferAssign l r inElm
    transfer l (NDefault (ILDR _ r _)) inElm = transferAssign l r inElm
    transfer l (NDefault (IPOP _ rs)) inElm = foldr (\r elm -> transferAssign l r elm) inElm rs
    
    transfer _ (NDefault (ICMP  _ _ _)) inElm = inElm
    transfer _ (NDefault (ISTR  _ _ _)) inElm = inElm
    transfer _ (NDefault (IB    _ _))   inElm = inElm
    transfer _ (NDefault (IPUSH _ _))   inElm = inElm
    transfer _ (NDefault (IOUT  _ _))   inElm = inElm
    transfer _ (NBranch _) inElm = inElm
    transfer _ (NAssertFalse _) inElm = inElm
    transfer _ (NAssertTrue _) inElm = inElm
    transfer _ NStart inElm = inElm
    transfer _ NEnd   inElm = inElm
    transfer _ n _ = error ("Undefined CFG node type for reaching definitions [" ++ (show n) ++ "]") 

    transferAssign :: TinyArmCFGLabel -> Register -> RDLatticeElem -> RDLatticeElem
    transferAssign l r input = M.insert r (S.singleton l) input


