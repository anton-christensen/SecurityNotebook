{-# LANGUAGE NamedFieldPuns,ExplicitForAll,ScopedTypeVariables #-}

module CFG where

import Common

import Language.TinyARM.Language

import Debug.Trace
import Data.List
import qualified Data.Map as M

data CFGLabel a
  = LStart
  | LBranch a
  | LAssertFalse a
  | LAssertTrue a
  | LDefault a
  | LEnd
  deriving (Eq)

data CFGNode a
  = NStart
  | NBranch a
  | NAssertFalse a
  | NAssertTrue a
  | NDefault a
  | NEnd
  deriving (Show)

instance (Ord a) => Ord (CFGLabel a) where
  -- start is least element
  compare LStart LStart = Prelude.EQ
  compare LEnd LEnd = Prelude.EQ
  compare LStart _ = Prelude.LT
  compare _ LStart = Prelude.GT
  -- end is last element
  compare LEnd _ = Prelude.GT
  compare _ LEnd = Prelude.LT
  
  -- compare based on address first, then based on type
  -- Branch < AssertFalse < AssertTrue < Default 
  compare (LBranch a1) (LAssertFalse a2) = if a1 == a2 then Prelude.LT else compare a1 a2
  compare (LBranch a1) (LAssertTrue a2)  = if a1 == a2 then Prelude.LT else compare a1 a2
  compare (LBranch a1) (LDefault a2)     = if a1 == a2 then Prelude.LT else compare a1 a2
  compare (LAssertFalse a1) (LBranch a2) = if a1 == a2 then Prelude.GT else compare a1 a2
  compare (LAssertTrue a1) (LBranch a2)  = if a1 == a2 then Prelude.GT else compare a1 a2
  compare (LDefault a1) (LBranch a2)     = if a1 == a2 then Prelude.GT else compare a1 a2
  
  compare (LAssertFalse a1) (LAssertTrue a2) = if a1 == a2 then Prelude.LT else compare a1 a2
  compare (LAssertFalse a1) (LDefault a2)    = if a1 == a2 then Prelude.LT else compare a1 a2
  compare (LAssertTrue a1) (LAssertFalse a2) = if a1 == a2 then Prelude.GT else compare a1 a2
  compare (LDefault a1) (LAssertFalse a2)    = if a1 == a2 then Prelude.GT else compare a1 a2
  
  compare (LAssertTrue a1) (LDefault a2)     = if a1 == a2 then Prelude.LT else compare a1 a2
  compare (LDefault a1) (LAssertTrue a2)     = if a1 == a2 then Prelude.GT else compare a1 a2
  
  -- compare addresses if same type
  compare a b = compare (extractAddress a) (extractAddress b)
    where
      extractAddress :: CFGLabel a -> a
      extractAddress (LBranch a) = a
      extractAddress (LAssertFalse a) = a
      extractAddress (LAssertTrue a) = a
      extractAddress (LDefault a) = a

instance (Show a) => Show (CFGLabel a) where
  show (LDefault a) = show a
  show (LBranch a) = "IF" ++ (show a)
  show (LAssertTrue a) = "T"  ++ (show a)
  show (LAssertFalse a) = "F"  ++ (show a)
  show LStart = "Start"
  show LEnd = "End"

type CFGKey = CFGLabel
data CFGValue l p = CFGValue
    { type_ :: CFGNode p
    , successors_ :: [CFGKey l]
    , predecessors_ :: [CFGKey l]
    }
    deriving (Show)

type CFG l p = M.Map (CFGKey l) (CFGValue l p)
type CFGListElement l p = (CFGKey l, CFGValue l p)


analysisToTxt :: forall l p a . (Show p, Show l, Ord l) => (CFG l p) -> M.Map (CFGKey l) a -> (a -> String) -> String
analysisToTxt cfg stateMap printState = intercalate "\n" $ map showState $ M.toList stateMap
  where
    showState :: (CFGKey l, a) -> String
    showState (key, state) = (showCFGNode key $ type_ $ cfg M.! key) ++ "\n" ++ (indent 2 $ printState state)
  

toDotGraph :: (Show p, Show l, Ord l) => CFG l p -> String
toDotGraph cfg = _toDotGraph cfg (M.map (\_ -> "") cfg) []

analysisToDotGraph :: (Show p, Show l, Ord l) => CFG l p -> M.Map (CFGKey l) a -> [CFGKey l] -> (a -> String) -> String
analysisToDotGraph cfg analysisResult highlightedStates printState = _toDotGraph cfg (M.map (\x -> escapeQuote $ printState x) analysisResult) highlightedStates

_toDotGraph :: forall l p a . (Show p, Show l, Ord l) => CFG l p -> M.Map (CFGKey l) String -> [CFGKey l] -> String
_toDotGraph graph analysisResult highlightedStates = graphPrefix ++ graphLabels graph highlightedStates ++ graphEdges graph ++ graphSuffix
  where
    graphPrefix :: String
    graphPrefix = "digraph G {\n  node [shape=box, fontsize=10];\n\n"
    
    graphLabels :: CFG l p -> [CFGKey l] -> String
    graphLabels cfg highlightedStates = intercalate "\n" (map (showNodeLabel highlightedStates) (M.toList cfg)) ++ "\n\n"

    graphEdges :: CFG l p -> String
    graphEdges cfg = intercalate "\n" (map showNodeEdges (M.toList cfg)) ++ "\n"
    
    showNodeLabel :: [CFGKey l] -> CFGListElement l p -> String
    showNodeLabel highlightedStates (lbl, CFGValue{type_=(NBranch c)}) = 
      if elem lbl highlightedStates then 
        "  " ++ (show lbl) ++ " [shape=diamond, style=\"rounded, filled\", fillcolor=grey, height=0.8 label=\"" ++ (showCFGNode lbl (NBranch c)) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
      else
        "  " ++ (show lbl) ++ " [shape=diamond, style=\"rounded\", height=0.8 label=\"" ++ (showCFGNode lbl (NBranch c)) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
    showNodeLabel highlightedStates (lbl, CFGValue{type_=(NAssertTrue c)}) = 
      if elem lbl highlightedStates then 
        "  " ++ (show lbl) ++ " [fontcolor=\"#008800FF\", color=\"#008800FF\", style=filled, fillcolor=\"#00880040\", label=\"" ++ (showCFGNode lbl (NAssertTrue c)) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
      else
        "  " ++ (show lbl) ++ " [fontcolor=\"#008800FF\", color=\"#008800FF\", label=\"" ++ (showCFGNode lbl (NAssertTrue c)) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
    showNodeLabel highlightedStates (lbl, CFGValue{type_=(NAssertFalse c)}) = 
      if elem lbl highlightedStates then 
        "  " ++ (show lbl) ++ " [fontcolor=\"#BB0000FF\", color=\"#BB0000FF\", style=filled, fillcolor=\"#BB000080\", label=\"" ++ (showCFGNode lbl (NAssertFalse c)) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
      else
        "  " ++ (show lbl) ++ " [fontcolor=\"#BB0000FF\", color=\"#BB0000FF\", label=\"" ++ (showCFGNode lbl (NAssertFalse c)) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
    showNodeLabel highlightedStates (lbl, CFGValue{type_=(NStart)}) = 
      if elem lbl highlightedStates then 
        "  " ++ (show lbl) ++ " [shape=circle, fillcolor=\"#00000080\", style=filled, width=0.2, label=\"\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
      else
        "  " ++ (show lbl) ++ " [shape=circle, fillcolor=\"#000000FF\", style=filled, width=0.2, label=\"\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
    showNodeLabel highlightedStates (lbl, CFGValue{type_=(NEnd)}) = 
      if elem lbl highlightedStates then 
        "  " ++ (show lbl) ++ " [shape=doublecircle, fillcolor=\"#00000080\", style=filled, width=0.2, label=\"\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
      else
        "  " ++ (show lbl) ++ " [shape=doublecircle, fillcolor=\"#000000FF\", style=filled, width=0.2, label=\"\", xlabel=\""++(analysisResult M.! lbl)++"\"];"
    showNodeLabel highlightedStates (lbl, CFGValue{type_=(node)}) = 
      if elem lbl highlightedStates then 
        "  " ++ (show lbl) ++ " [label=\"" ++ (showCFGNode lbl node) ++ "\", style=filled, fillcolor=grey,  xlabel=\""++(analysisResult M.! lbl)++"\"];"
      else
        "  " ++ (show lbl) ++ " [label=\"" ++ (showCFGNode lbl node) ++ "\", xlabel=\""++(analysisResult M.! lbl)++"\"];"

    showNodeEdges :: CFGListElement l p -> String
    showNodeEdges (lbl, CFGValue{successors_=edges}) = concat ["  ", (show lbl), " -> {", (intercalate "," $ map show edges), "};"]

    -- showCFGLabel :: CFGLabel -> String
    -- showCFGLabel (LDefault addr) = "pc" ++ show addr
    -- showCFGLabel (LBranch addr) = "pc" ++ (show addr) ++ "_if"
    -- showCFGLabel (LAssertTrue addr) = "pc" ++ (show addr) ++ "_t"
    -- showCFGLabel (LAssertFalse addr) = "pc" ++ (show addr) ++ "_f"
    -- showCFGLabel (LStart) = "start"
    -- showCFGLabel (LEnd) = "end"

    graphSuffix :: String
    graphSuffix = "}\n"
    
showCFGNode :: (Show l, Show p) => CFGLabel l -> CFGNode p -> String
showCFGNode lbl node  = (show lbl) ++ ": " ++ (showCFGNode' node)
  where
    showCFGNode' :: (Show p) => CFGNode p -> String
    showCFGNode' (NStart) = ""
    showCFGNode' (NEnd) = ""
    showCFGNode' (NDefault i) = escapeQuote $ show i
    showCFGNode' (NBranch cond) = "if " ++ (show cond)
    showCFGNode' (NAssertTrue  cond) = "Assert " ++ (show cond)
    showCFGNode' (NAssertFalse cond) = "Refute " ++ (show cond)


reverseCFG :: CFG l p -> CFG l p
reverseCFG cfg = M.map (\val@CFGValue {successors_ = s, predecessors_ = p} -> val {successors_ = p, predecessors_ = s}) cfg