module Language.TinyARM.Analysis.Liveness where

import Analysis
import Language.TinyARM.Language
import Language.TinyARM.Analysis
import Language.TinyARM.CFG
import CFG

import Data.Lattice
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

type LiveLatticeElem = S.Set Register

prettyPrintState :: LiveLatticeElem -> String
prettyPrintState s = "{" ++ (intercalate ", " $ map show $ S.toList s) ++ "}"

livenessAnalysis :: Int -> TinyArmCFG -> TinyArmAnalysisResult LiveLatticeElem
livenessAnalysis n cfg = 
  worklistAlgorithmSimpleBackwards n cfg initialState join transfer
  where
    initialState :: M.Map TinyArmCFGLabel LiveLatticeElem
    initialState = M.map (\_ -> bottom) cfg

    bottom :: LiveLatticeElem
    bottom = S.empty

    join :: [LiveLatticeElem] -> LiveLatticeElem
    join [] = bottom
    join (x:xs) = ljoin x $ join xs

    transfer :: TinyArmCFGLabel -> TinyArmCFGNode -> LiveLatticeElem -> LiveLatticeElem
    -- (IN U GenSet(instr)) / KillSet(instr)
    transfer l (NDefault i)     inState = S.union (genSet i) $ S.filter (flip S.notMember $ killSet i) inState
    transfer _ (NBranch _)      inElm = inElm
    transfer _ (NAssertFalse _) inElm = inElm
    transfer _ (NAssertTrue _)  inElm = inElm
    transfer _ NStart           inElm = inElm
    transfer _ NEnd             inElm = inElm
    transfer _ n _ = error ("Undefined CFG node type for liveness [" ++ (show n) ++ "]") 


    killSet :: Instr -> S.Set Register -- set of registers assigned to
    killSet (IMOV _ _ r _) = S.fromList [r]
    killSet (IBINOP _ _ _ r _ _) = S.fromList [r]
    killSet (ILDR _ r _) = S.fromList [r]
    killSet (IPOP _ rs) = S.fromList rs
    
    killSet (ICMP _ _ _) = S.empty
    killSet (ISTR _ _ _) = S.empty
    killSet (IB _ _)     = S.empty
    killSet (IPUSH _ _)  = S.empty
    killSet (IOUT _ _)   = S.empty
    killSet i = error ("Undefined liveness killSet for instruction [" ++ (show i) ++ "]") 

    genSet :: Instr -> S.Set Register -- set of registers required for evaluation
    genSet (IMOV _ _ _ (REGVAL r)) = S.fromList [r]
    genSet (IMOV _ _ _ _) = S.empty
    
    genSet (IBINOP _ _ _ _ r1 (REGVAL r2)) = S.fromList [r1,r2]
    genSet (IBINOP _ _ _ _ r1 _) = S.fromList [r1]
    
    genSet (ICMP _ r1 (REGVAL r2)) = S.fromList [r1,r2]
    genSet (ICMP _ r1 _) = S.fromList [r1]
    
    genSet (ILDR _ _ (REGADDR r1)) = S.fromList [r1]
    genSet (ILDR _ _ (REGADDRIMMOFFSET r1 _ _)) = S.fromList [r1]
    genSet (ILDR _ _ (REGADDRREGOFFSET r1 _ r2)) = S.fromList [r1,r2]
    genSet (ILDR _ _ _) = S.empty
    
    genSet (ISTR _ r1 (REGADDR r2)) = S.fromList [r1,r2]
    genSet (ISTR _ r1 (REGADDRIMMOFFSET r2 _ _)) = S.fromList [r1,r2]
    genSet (ISTR _ r1 (REGADDRREGOFFSET r2 _ r3)) = S.fromList [r1,r2,r3]
    genSet (ISTR _ r1 _) = S.fromList [r1]
    
    genSet (IOUT _ (REGVAL r)) = S.fromList [r]
    
    genSet (IPUSH _ rs) = S.fromList rs


    genSet (IB _ _) = S.empty
    genSet (IPOP _ _) = S.empty
    genSet (IOUT _ _) = S.empty
    genSet i = error ("Undefined liveness genSet for instruction [" ++ (show i) ++ "]") 
    -- genSet _ = S.empty
    