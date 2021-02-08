module TinyARM.Analysis.NonInterference where

import TinyARM.Analysis
import TinyARM.Language
import TinyARM.CFG
import TinyARM.Common

import Data.Lattice
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

                -- High tainted regs  -- High sec reg
type NILatticeElem = (S.Set Register, S.Set Register)

prettyPrintState :: NILatticeElem -> String
prettyPrintState state = "foobar"-- "[" ++ (intercalate ", " $ map showMapElm (M.toList state)) ++ "]"
  where 
    showMapElm :: (Register, S.Set CFGKey) -> String
    showMapElm (r, s) = "" ++ (show r) ++ "->" ++ (showSet s) ++  ""
    showSet :: S.Set CFGKey -> String
    showSet s = "{" ++ (intercalate ", " $ map show $ S.toList s) ++ "}"

nonInterferenceAnalysis :: CFG -> (M.Map CFGKey NILatticeElem)
nonInterferenceAnalysis cfg = 
  let

    (outmap, inmap) = worklistAlgorithm cfg initialState initialState join transfer
  in outmap
  where
    initialState :: M.Map CFGKey NILatticeElem
    initialState = M.map (\_ -> bottom) cfg

    join :: [NILatticeElem] -> NILatticeElem
    join [] = bottom
    join (x:xs) = ljoin x $ join xs

    bottom :: NILatticeElem
    bottom = (S.empty, S.empty)

    transfer :: CFGKey -> CFGNode -> NILatticeElem -> NILatticeElem
    transfer _ (NDefault (IOUT    _ (REGVAL r))) (low, high) = (S.union low $ S.singleton r, high)
    transfer _ _ inElm = inElm


type DominatorLatticeElem = S.Set CFGKey

prettyPrintDomState :: DominatorLatticeElem -> String
prettyPrintDomState state =  printInlineSet show state

-- | Returns the graph decorated with the immidiate post-dominator of every node
dominatorAnalysis :: CFG -> (M.Map CFGKey DominatorLatticeElem)
dominatorAnalysis cfg = 
  let 
    (outmap, inmap) = worklistAlgorithm (reverseCFG cfg) initialState initialState join transfer
    doms = immediateDominators outmap
  in doms
  -- in mapmap (\(k, v) -> case k of 
  --     (LBranch _) -> (k,S.empty)
  --     _           -> (k,S.empty)
  --     ) doms
  where
    initialState :: M.Map CFGKey DominatorLatticeElem
    initialState = M.map (\_ -> bottom) cfg

    join :: [DominatorLatticeElem] -> DominatorLatticeElem
    join [] = bottom
    join (x:[]) = x
    join (x:xs) = S.intersection x $ join xs
    
    bottom :: DominatorLatticeElem
    bottom = S.empty

    transfer :: CFGKey -> CFGNode -> DominatorLatticeElem -> DominatorLatticeElem
    transfer l _ pred = S.union pred $ S.singleton l

    immediateDominators :: (M.Map CFGKey DominatorLatticeElem) -> (M.Map CFGKey DominatorLatticeElem)
    immediateDominators dominators = mapmap (findImmediateDominatorOfNode dominators) dominators

    findImmediateDominatorOfNode :: (M.Map CFGKey DominatorLatticeElem) -> (CFGKey, DominatorLatticeElem) -> (CFGKey, DominatorLatticeElem)
    findImmediateDominatorOfNode dominators n@(l, ldoms) = (l, S.delete l $ S.filter (isImmediateDominatorOfNode dominators l) ldoms)

    isImmediateDominatorOfNode :: (M.Map CFGKey DominatorLatticeElem) -> CFGKey -> CFGKey -> Bool
    isImmediateDominatorOfNode dominators l potentialImmDom = (dominators M.! l) == (S.insert l (dominators M.! potentialImmDom))  

