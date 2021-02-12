-- necescary to make forall work, which makes the `a` type variables the same in aux function type
{-# LANGUAGE ExplicitForAll,ScopedTypeVariables #-}

module Analysis where

import CFG
import Common
-- import Language.TinyARM.Analysis.Types
-- import Language.TinyARM.Language

import Prelude hiding (EQ,LT,GT)
import Debug.Trace
import Data.Lattice
import Data.Word
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S

data GenericAnalysisResult l p = GenericAnalysisResult
    { gaDotGraph :: String
    , gaSteps :: Int
    , gaWorkList :: [CFGKey l]
    , gaGraph :: CFG l p
    }
    deriving (Show)

data AnalysisResult l p a = AnalysisResult
    { aLabels :: M.Map (CFGKey l) a
    , aSteps :: Int
    , aWorkList :: [CFGKey l]
    , aGraph :: CFG l p
    }
    deriving (Show)

data AnalysisResult_ l p a = AnalysisResult_
    { aOutLabels_ :: M.Map (CFGKey l) a
    , aInLabels_  :: M.Map (CFGKey l) a
    , aSteps_ :: Int
    , aWorkList_ :: [CFGKey l]
    , aGraph_ :: CFG l p
    }
    deriving (Show)



analysisToGeneric :: (Ord l) 
                  => AnalysisResult l p a -- Analysis result
                  -> (l -> String)    -- Label printer
                  -> (p -> String)    -- Program Command printer
                  -> (a -> String)    -- State printer
                  -> GenericAnalysisResult l p
analysisToGeneric result printLabel printCode printState =
  GenericAnalysisResult 
    { gaDotGraph= analysisToDotGraph (aGraph result) (aLabels result) (listHead $ aWorkList result) printLabel printCode printState
    , gaSteps= aSteps result, gaWorkList = aWorkList result, gaGraph = aGraph result }
  where
    listHead [] = []
    listHead l = (head l):[]



printAnalysisDot :: (Ord l) => CFG l p -> (CFG l p -> (M.Map (CFGKey l) a)) -> (l -> String) -> (p -> String) -> (a -> String) -> String
printAnalysisDot cfg analysis printLabel printCode printState = analysisToDotGraph cfg (analysis cfg) [] printLabel printCode printState

printAnalysisTxt :: (Ord l) => CFG l p -> (CFG l p -> (M.Map (CFGKey l) a)) -> (l -> String) -> (p -> String) -> (a -> String) -> String
printAnalysisTxt cfg analysis printLabel printCode printState = analysisToTxt cfg (analysis cfg) printLabel printCode printState



_forwardAnalysis :: AnalysisResult_ l p a -> AnalysisResult l p a
_forwardAnalysis res = AnalysisResult { aLabels= aOutLabels_ res, aSteps= aSteps_ res, aWorkList= aWorkList_ res, aGraph= aGraph_ res }

_backwardAnalysis :: AnalysisResult_ l p a -> AnalysisResult l p a
_backwardAnalysis res = AnalysisResult { aLabels= aInLabels_ res, aSteps= aSteps_ res, aWorkList= aWorkList_ res, aGraph= aGraph_ res }

-- | Simplified interface for basic forwards anlysees like reaching definitions
worklistAlgorithmSimpleForward :: forall l p a . (Ord l, Eq a, Show a)
  => Int -- number of steps to perform (negative number means keep going till done)
  -> CFG l p -- ^ the program graph
  -> (M.Map (CFGKey l) a) -- ^ Initial state
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (CFGKey l -> CFGNode p -> a -> a)  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult l p a -- ^ Resulting map from node labels to
worklistAlgorithmSimpleForward n cfg initialState join transfer = 
  _forwardAnalysis $ worklistAlgorithm n cfg initialState initialState join transfer

-- | Simplified interface for basic backwards analysees like liveness 
worklistAlgorithmSimpleBackwards :: forall l p a . (Ord l, Eq a, Show a)
  => Int -- number of steps to perform (negative number means keep going till done)
  -> CFG l p -- ^ the program graph
  -> (M.Map (CFGKey l) a) -- ^ Initial state
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (CFGKey l -> CFGNode p -> a -> a)  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult l p a -- ^ Resulting map from node labels to
worklistAlgorithmSimpleBackwards n cfg initialState join transfer = 
  _backwardAnalysis $ worklistAlgorithm n (reverseCFG cfg) initialState initialState join transfer



-- | General worklist algorithm but does not use any global state
worklistAlgorithm :: forall l p a . (Ord l, Eq a, Show a)
                  => Int -- number of steps to perform (negative number means keep going till done)
                  -> CFG l p-- ^ the program graph
                  -> (M.Map (CFGKey l) a) -- ^ Initial out states
                  -> (M.Map (CFGKey l) a) -- ^ Initial in states
                  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
                  -> (CFGKey l -> CFGNode p -> a -> a)  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
                  -> AnalysisResult_ l p a
worklistAlgorithm n cfg outmap inmap join transfer =
  _worklistAlgorithm n cfg Nothing outmap inmap join (\g k n l -> (g, transfer k n l)) 





worklistAlgorithmWithGlobalState :: forall l p a g . (Ord l, Eq a, Show a)
  => CFG l p -- ^ the program graph
  -> g -- ^ initial global state
  -> (M.Map (CFGKey l) a) -- ^ Initial out states
  -> (M.Map (CFGKey l) a) -- ^ Initial in states
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (g -> CFGKey l -> CFGNode p -> a -> (g, a))  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult_ l p a
worklistAlgorithmWithGlobalState = _worklistAlgorithm (-1)





-- | The most general version of the worklist algorithm
_worklistAlgorithm :: forall l p a g . (Ord l, Eq a, Show a)
  => Int -- number of steps to perform (negative number means keep going till done)
  -> CFG l p -- ^ the program graph
  -> g -- ^ initial global state
  -> (M.Map (CFGKey l) a) -- ^ Initial out states
  -> (M.Map (CFGKey l) a) -- ^ Initial in states
  -> ([a] -> a)       -- ^ The join function that combines the out states of the list of predecesors
  -> (g -> CFGKey l -> CFGNode p -> a -> (g, a))  -- ^ The transfer function that generates the nodes new out state from the the node details and the join of its predecesors
  -> AnalysisResult_ l p a
_worklistAlgorithm n cfg globalInit outmap inmap join transfer = 
  (worklistAlgorithm' n 0 (M.keys cfg) globalInit outmap inmap) {aGraph_ = cfg}
  where 
    worklistAlgorithm' :: Int -> Int -> [CFGKey l] -> g -> M.Map (CFGKey l) a -> M.Map (CFGKey l) a -> AnalysisResult_ l p a
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
