module TinyARM.Analysis.ConditionLiveness where

import TinyARM.Analysis
import TinyARM.Language
import TinyARM.CFG

import Data.Lattice
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

type LiveLatticeElem = S.Set Condition


prettyPrintState :: LiveLatticeElem -> String
prettyPrintState s = "{" ++ (intercalate ", " $ map show $ S.toList s) ++ "}"

conditionLivenessAnalysis :: CFG -> (M.Map CFGKey LiveLatticeElem)
conditionLivenessAnalysis cfg = 
  let (outmap, inmap) = worklistAlgorithm (reverseCFG cfg) initialState initialState join transfer
  in inmap  
  where
    initialState :: M.Map CFGKey LiveLatticeElem
    initialState = M.map (\_ -> bottom) cfg
    transfer :: CFGKey -> CFGNode -> LiveLatticeElem -> LiveLatticeElem
    -- (IN U GenSet(instr)) / KillSet(instr)
    transfer l n inState = S.union (genSet n) $ if shouldEmpty n then S.empty else inState

    shouldEmpty :: CFGNode -> Bool
    shouldEmpty (NDefault (IMOV s _ _ _)) = True
    shouldEmpty (NDefault (IBINOP s _ _ _ _ _)) = True
    shouldEmpty (NDefault (ICMP _ _ _)) = True
    shouldEmpty _ = False

    genSet :: CFGNode -> S.Set Condition -- set of registers required for evaluation
    genSet (NBranch c) = S.singleton c
    -- genSet (NAssertTrue c) = S.singleton c
    -- genSet (NAssertFalse c) = S.singleton c
    genSet _ = S.empty

    bottom :: LiveLatticeElem
    bottom = S.empty

    join :: [LiveLatticeElem] -> LiveLatticeElem
    join [] = bottom
    join (x:xs) = ljoin x $ join xs
    