module Language.TinyARM (parseTinyARM, reachingDefinitions, liveness,
    GenericAnalysisResult, gaSteps, gaWorkList, gaDotGraph) where

import Language.TinyARM.Parser
import Language.TinyARM.Language
import Language.TinyARM.CFG
import Language.TinyARM.Analysis
import CFG
import Analysis

import qualified Data.Map as M

import qualified Language.TinyARM.Analysis.ReachingDefinitions as RD
import qualified Language.TinyARM.Analysis.Liveness as L


_analysis :: (Show a) => (Int -> TinyArmCFG -> TinyArmAnalysisResult a) -> (a -> String) -> Program -> Int -> TinyArmGenericAnalysisResult
_analysis analysis statePrinter program nSteps = 
    let result = analysis nSteps $ makeCFG program 
    in analysisToGeneric result show prettyPrintInstr statePrinter

reachingDefinitions :: Program -> Int -> TinyArmGenericAnalysisResult
reachingDefinitions = _analysis RD.reachingDefinitionsAnalysis RD.prettyPrintState 

liveness :: Program -> Int -> TinyArmGenericAnalysisResult
liveness = _analysis L.livenessAnalysis L.prettyPrintState