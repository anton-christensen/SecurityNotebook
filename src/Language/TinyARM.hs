module Language.TinyARM (parseTinyARM, reachingDefinitions, liveness,
    GenericAnalysisResult, gaSteps, gaWorkList, gaDotGraph) where

import Language.TinyARM.Parser
import Language.TinyARM.Language
import Language.TinyARM.CFG
import Language.TinyARM.Analysis

import qualified Data.Map as M

import qualified Language.TinyARM.Analysis.ReachingDefinitions as RD
import qualified Language.TinyARM.Analysis.Liveness as L

_analysis :: (Show a) => (Int -> CFG -> AnalysisResult a) -> (a -> String) -> Program -> Int -> GenericAnalysisResult
_analysis analysis statePrinter program nSteps = 
    let result = analysis nSteps $ makeCFG program 
    in analysisToGeneric result statePrinter

reachingDefinitions :: Program -> Int -> GenericAnalysisResult
reachingDefinitions = _analysis RD.reachingDefinitionsAnalysis RD.prettyPrintState 

liveness :: Program -> Int -> GenericAnalysisResult
liveness = _analysis L.livenessAnalysis L.prettyPrintState