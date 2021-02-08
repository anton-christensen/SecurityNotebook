-- necescary to make forall work, which makes the `a` type variables the same in aux function type
{-# LANGUAGE ExplicitForAll,ScopedTypeVariables #-}

module Language.TinyARM.Analysis where

import Language.TinyARM.Analysis.Types
import Language.TinyARM.Common
import Language.TinyARM.Language
import Language.TinyARM.CFG

import Prelude hiding (EQ,LT,GT)
import Debug.Trace
import Data.Lattice
import Data.Word
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S

data GenericAnalysisResult = GenericAnalysisResult
    { gaDotGraph :: String
    , gaSteps :: Int
    , gaWorkList :: [CFGKey]
    , gaGraph :: CFG
    }
    deriving (Show)

data AnalysisResult a = AnalysisResult
    { aLabels :: M.Map CFGKey a
    , aSteps :: Int
    , aWorkList :: [CFGKey]
    , aGraph :: CFG
    }
    deriving (Show)

data AnalysisResult_ a = AnalysisResult_
    { aOutLabels_ :: M.Map CFGKey a
    , aInLabels_  :: M.Map CFGKey a
    , aSteps_ :: Int
    , aWorkList_ :: [CFGKey]
    , aGraph_ :: CFG
    }
    deriving (Show)



analysisToGeneric :: (Show a) 
                  => AnalysisResult a -- Analysis result
                  -> (a -> String)    -- State printer
                  -> GenericAnalysisResult
analysisToGeneric result statePrinter =
  GenericAnalysisResult 
    { gaDotGraph= analysisToDotGraph (aGraph result) (aLabels result) (listHead $ aWorkList result) statePrinter
    , gaSteps= aSteps result, gaWorkList = aWorkList result, gaGraph = aGraph result }
  where
    listHead [] = []
    listHead l = (head l):[]



printAnalysisDot :: (Show a) => CFG -> (CFG -> (M.Map CFGKey a)) -> (a -> String) -> String
printAnalysisDot cfg analysis showState = analysisToDotGraph cfg (analysis cfg) [] showState

printAnalysisTxt :: (Show a) => CFG -> (CFG -> (M.Map CFGKey a)) -> (a -> String) -> String
printAnalysisTxt cfg analysis showState = analysisToTxt cfg (analysis cfg) showState



_forwardAnalysis :: AnalysisResult_ a -> AnalysisResult a
_forwardAnalysis res = AnalysisResult { aLabels= aOutLabels_ res, aSteps= aSteps_ res, aWorkList= aWorkList_ res, aGraph= aGraph_ res }

_backwardAnalysis :: AnalysisResult_ a -> AnalysisResult a
_backwardAnalysis res = AnalysisResult { aLabels= aInLabels_ res, aSteps= aSteps_ res, aWorkList= aWorkList_ res, aGraph= aGraph_ res }

-- | Simplified interface for basic forwards anlysees like reaching definitions
worklistAlgorithmSimpleForward :: forall a . (Eq a, Show a)
  => Int -- number of steps to perform (negative number means keep going till done)
  -> CFG -- ^ the program graph
  -> (M.Map CFGKey a) -- ^ Initial state
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (CFGKey -> CFGNode -> a -> a)  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult a -- ^ Resulting map from node labels to
worklistAlgorithmSimpleForward n cfg initialState join transfer = 
  _forwardAnalysis $ worklistAlgorithm n cfg initialState initialState join transfer

-- | Simplified interface for basic backwards analysees like liveness 
worklistAlgorithmSimpleBackwards :: forall a . (Eq a, Show a)
  => Int -- number of steps to perform (negative number means keep going till done)
  -> CFG -- ^ the program graph
  -> (M.Map CFGKey a) -- ^ Initial state
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (CFGKey -> CFGNode -> a -> a)  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult a -- ^ Resulting map from node labels to
worklistAlgorithmSimpleBackwards n cfg initialState join transfer = 
  _backwardAnalysis $ worklistAlgorithm n (reverseCFG cfg) initialState initialState join transfer



-- | General worklist algorithm but does not use any global state
worklistAlgorithm :: forall a . (Eq a, Show a)
                  => Int -- number of steps to perform (negative number means keep going till done)
                  -> CFG -- ^ the program graph
                  -> (M.Map CFGKey a) -- ^ Initial out states
                  -> (M.Map CFGKey a) -- ^ Initial in states
                  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
                  -> (CFGKey -> CFGNode -> a -> a)  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
                  -> AnalysisResult_ a
worklistAlgorithm n cfg outmap inmap join transfer =
  _worklistAlgorithm n cfg Nothing outmap inmap join (\g k n l -> (g, transfer k n l)) 





worklistAlgorithmWithGlobalState :: forall a g . (Eq a, Show a)
  => CFG -- ^ the program graph
  -> g -- ^ initial global state
  -> (M.Map CFGKey a) -- ^ Initial out states
  -> (M.Map CFGKey a) -- ^ Initial in states
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (g -> CFGKey -> CFGNode -> a -> (g, a))  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult_ a
worklistAlgorithmWithGlobalState = _worklistAlgorithm (-1)





-- | The most general version of the worklist algorithm
_worklistAlgorithm :: forall a g . (Eq a, Show a)
  => Int -- number of steps to perform (negative number means keep going till done)
  -> CFG -- ^ the program graph
  -> g -- ^ initial global state
  -> (M.Map CFGKey a) -- ^ Initial out states
  -> (M.Map CFGKey a) -- ^ Initial in states
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (g -> CFGKey -> CFGNode -> a -> (g, a))  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult_ a
_worklistAlgorithm n cfg globalInit outmap inmap join transfer = 
  (worklistAlgorithm' n 0 (M.keys cfg) globalInit outmap inmap) {aGraph_ = cfg}
  where 
    worklistAlgorithm' :: Int -> Int -> [CFGKey] -> g -> M.Map CFGKey a -> M.Map CFGKey a -> AnalysisResult_ a
    worklistAlgorithm' _ counter [] _ outmap inmap = AnalysisResult_ { aOutLabels_=outmap, aInLabels_=inmap, aSteps_=counter, aWorkList_=[], aGraph_=M.empty}
    worklistAlgorithm' 0 counter wl  _ outmap inmap = AnalysisResult_ { aOutLabels_=outmap, aInLabels_=inmap, aSteps_=counter, aWorkList_=wl, aGraph_=M.empty}
    worklistAlgorithm' n counter (key:rest) globalState outmap inmap = worklistAlgorithm' (n-1) (counter+1) (rest++newWorklistElements) globalState' updatedOut updatedIn
    -- worklistAlgorithm' (key:rest) out = (trace debugString $ worklistAlgorithm' (rest++newWorklistElements) updatedResult)
      where 
        v = cfg M.! key
        node = type_ v
        predecessors = predecessors_ v
        successors = successors_ v
        inlist = (map (outmap M.!) predecessors)
        newInElm = join inlist
        (globalState', newOutElm) = transfer globalState key node newInElm
        updatedOut = M.insert key newOutElm outmap
        updatedIn = M.insert key newInElm inmap
        newWorklistElements = if newOutElm == (outmap M.! key) then [] else successors
        debugString = ""
          -- "Worklist: " ++ (tshow (key:rest)) ++ "\n" 
          --          ++ "Analysing "++(tshow key) ++": " ++ (tshow v) ++ "\n" 
          --          ++ "   in = " ++ (tshow (join inlist)) ++ "\n"
          --          ++ "oldout= " ++ (tshow (out M.! key)) ++ "\n"
          --          ++ "newout= " ++ (tshow newOutElm)
          --          ++ "\n\n" 
