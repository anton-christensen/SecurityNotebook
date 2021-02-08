{-# LANGUAGE NamedFieldPuns,ExplicitForAll,ScopedTypeVariables #-}

module Language.TinyARM.CFG where

import Language.TinyARM.Common
import Language.TinyARM.Language

import Debug.Trace
import Data.List
import qualified Data.Map as M

data CFGLabel 
  = LStart
  | LBranch Address
  | LAssertFalse Address
  | LAssertTrue Address
  | LDefault Address
  | LEnd
  deriving (Eq)

data CFGNode 
  = NStart
  | NBranch Condition
  | NAssertFalse Condition
  | NAssertTrue Condition
  | NDefault (Instruction SpecialInstructions)
  | NEnd
  deriving (Show)

instance Ord CFGLabel where
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
      extractAddress :: CFGLabel -> Address
      extractAddress (LBranch a) = a
      extractAddress (LAssertFalse a) = a
      extractAddress (LAssertTrue a) = a
      extractAddress (LDefault a) = a

instance Show CFGLabel where
  show (LDefault a) = show a
  show (LBranch a) = "IF" ++ (show a)
  show (LAssertTrue a) = "T"  ++ (show a)
  show (LAssertFalse a) = "F"  ++ (show a)
  show LStart = "Start"
  show LEnd = "End"

type CFGKey = CFGLabel
data CFGValue = CFGValue
    { type_ :: CFGNode
    , successors_ :: [CFGKey]
    , predecessors_ :: [CFGKey]
    }
    deriving (Show)

type CFG = M.Map CFGKey CFGValue
type CFGListElement = (CFGKey, CFGValue)


analysisToTxt :: forall a . CFG -> M.Map CFGKey a -> (a -> String) -> String
analysisToTxt cfg stateMap printState = intercalate "\n" $ map showState $ M.toList stateMap
  where
    showState :: (CFGKey, a) -> String
    showState (key, state) = (showCFGNode key $ type_ $ cfg M.! key) ++ "\n" ++ (indent 2 $ printState state)
  

toDotGraph :: CFG -> String
toDotGraph cfg = _toDotGraph cfg (M.map (\_ -> "") cfg) []

analysisToDotGraph :: (Show a) => CFG -> M.Map CFGKey a -> [CFGKey] -> (a -> String) -> String
analysisToDotGraph cfg analysisResult highlightedStates printState = _toDotGraph cfg (M.map (\x -> escapeQuote $ printState x) analysisResult) highlightedStates

_toDotGraph :: CFG -> M.Map CFGKey String -> [CFGKey] -> String
_toDotGraph graph analysisResult highlightedStates = graphPrefix ++ graphLabels graph highlightedStates ++ graphEdges graph ++ graphSuffix
  where
    graphPrefix :: String
    graphPrefix = "digraph G {\n  node [shape=box, fontsize=10];\n\n"
    
    graphLabels :: CFG -> [CFGKey] -> String
    graphLabels cfg highlightedStates = intercalate "\n" (map (showNodeLabel highlightedStates) (M.toList cfg)) ++ "\n\n"

    graphEdges :: CFG -> String
    graphEdges cfg = intercalate "\n" (map showNodeEdges (M.toList cfg)) ++ "\n"
    
    showNodeLabel :: [CFGKey] -> CFGListElement -> String
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

    showNodeEdges :: CFGListElement -> String
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
    
showCFGNode :: CFGLabel -> CFGNode -> String
showCFGNode lbl node  = (show lbl) ++ ": " ++ (showCFGNode' node)
  where
    showCFGNode' :: CFGNode -> String
    showCFGNode' (NStart) = ""
    showCFGNode' (NEnd) = ""
    showCFGNode' (NDefault i) = escapeQuote $ prettyPrintInstr i
    showCFGNode' (NBranch cond) = "if " ++ (show cond)
    showCFGNode' (NAssertTrue  cond) = "Assert " ++ (show cond)
    showCFGNode' (NAssertFalse cond) = "Refute " ++ (show cond)


reverseCFG :: CFG -> CFG
reverseCFG cfg = M.map (\val@CFGValue {successors_ = s, predecessors_ = p} -> val {successors_ = p, predecessors_ = s}) cfg

makeCFG :: Program -> CFG
makeCFG prog = cfg
  where 
    g0 = mapmap initialGraph (code_ prog)                 -- Create default nodes
    g1 = mapmapReplace insertBranches g0                  -- Insert conditional structures
    g2 = M.mapWithKey insertEdges g1                      -- Insert all successor edges
    g3 = addStartEnd g2                                   -- Add start and end nodes
    g4 = M.map (redirectEdgesToConditionals g3) g3        -- Redirect all edges pointing to conditional instructions
    g5 = M.map (removeBranchOfRepeatedCondition g4) g4    -- Map edges of repeated branches
    g6 = fillPredecessors g5                              -- Create a reverse CFG
    g7 = removeOrphanBranches g6                          -- Remove branches that no longer have any predecessors
    cfg = g7

    newEmptyCFGValue :: CFGNode -> CFGValue
    newEmptyCFGValue t = CFGValue {type_= t, successors_ = [],predecessors_ = []}


    -- emptyCFGValue :: CFGValue
    -- emptyCFGValue = CFGValue {successors_ = [],predecessors_ = []}

    initialGraph :: (Address, Instr) -> CFGListElement
    initialGraph (addr, instr) = (LDefault addr, newEmptyCFGValue (NDefault instr))

    insertBranches :: CFGListElement -> [CFGListElement]
    insertBranches lstElement@(lbl, CFGValue {type_ = NDefault instr}) = 
      insertBranches' lstElement (extractCondition instr)
      where
        insertBranches' :: CFGListElement -> Condition -> [CFGListElement]
        insertBranches' origElement AL = 
          [origElement]
        insertBranches' origElement@(LDefault addr, _) c =
          [ origElement
          , (LBranch addr,      newEmptyCFGValue (NBranch c))
          , (LAssertTrue addr,  newEmptyCFGValue (NAssertTrue c))
          , (LAssertFalse addr, newEmptyCFGValue (NAssertFalse c))
          ]
    -- The following shouldn't be neccessary..
    -- insertBranches x = [x]

    insertEdges :: CFGLabel -> CFGValue -> CFGValue
    insertEdges (LDefault _) val@CFGValue {type_ = NDefault (IB _ (DIRECT addr))} =       -- branch direct, edge to target 
      val {successors_ = [LDefault addr]}
    insertEdges (LDefault addr) val@CFGValue {type_ = NDefault (IB _ (RELATIVE PLUS i))} =   -- branch relative, edge to target + i
      val {successors_ = [LDefault $ addr + i]}
    insertEdges (LDefault addr) val@CFGValue {type_ = NDefault (IB _ (RELATIVE MINUS i))} =  -- branch relative, edge to target - i
      val {successors_ = [LDefault $ addr - i]}
    insertEdges (LDefault addr) val@CFGValue {type_ = NDefault (IB _ (LABEL l))} =           -- branch label, edge to label's address
      val {successors_ = [LDefault $ (labels_ prog) M.! l]}

    insertEdges (LBranch addr)      val@CFGValue {type_ = NBranch _} =                       -- branching node, edges to True and False
      val {successors_ = [LAssertTrue addr, LAssertFalse addr]}
    insertEdges (LAssertTrue addr)  val@CFGValue {type_ = NAssertTrue _} =                   -- true node, edge to original node
      val {successors_ = [LDefault addr]}
    insertEdges (LAssertFalse addr) val@CFGValue {type_ = NAssertFalse _} =                  -- false node, edge to next default node           
      val {successors_ = [LDefault $ addr + 1]}

    insertEdges LEnd    val@CFGValue {type_ = NEnd} =                                        -- end node
      val {successors_ = []}
    insertEdges LStart  val@CFGValue {type_ = NStart} =                                      -- start node
      val {successors_ = [LDefault 0]}
      
    insertEdges (LDefault addr) val@CFGValue {type_ = NDefault i} =                          -- all other nodes
      val {successors_ = [LDefault $ addr + 1]}

    -- The following shouldn't be neccessary..
    -- insertEdges _ x = trace (show x) x

    addStartEnd :: CFG -> CFG
    addStartEnd cfg = addEnd $ replaceEdgesToHalt $ addStart cfg
      where
        addStart :: CFG -> CFG
        addStart cfg = M.insert LStart (newEmptyCFGValue NStart) {successors_ = [LDefault 0]} cfg

        replaceEdgesToHalt :: CFG -> CFG
        replaceEdgesToHalt cfg = M.map replaceEdgesToHalt' cfg
        
        replaceEdgesToHalt' :: CFGValue -> CFGValue
        replaceEdgesToHalt' val@CFGValue {successors_ = edges} = val {successors_ = map replaceEdgesToHalt'' edges}

        replaceEdgesToHalt'' :: CFGLabel -> CFGLabel
        replaceEdgesToHalt'' edge = case cfg M.! edge of
          CFGValue {type_ = NDefault (META (HALT _) _)} -> LEnd
          _                                             -> edge

        addEnd :: CFG -> CFG
        addEnd cfg = mapmap haltToEnd cfg
        
        haltToEnd :: CFGListElement -> CFGListElement
        haltToEnd (_, val@CFGValue {type_ = NDefault (META (HALT _) _)}) = (LEnd, newEmptyCFGValue NEnd)
        haltToEnd elem = elem

    redirectEdgesToConditionals :: CFG -> CFGValue -> CFGValue
    redirectEdgesToConditionals cfg val@CFGValue {type_ = NAssertTrue _} = val -- don't touch NAssertTrue (creates unintentional loop)
    redirectEdgesToConditionals cfg val@CFGValue {successors_ = edges} = 
      val {successors_ = map redirectEdgeToConditionals edges}
      where        
        redirectEdgeToConditionals :: CFGLabel -> CFGLabel
        redirectEdgeToConditionals (LDefault addr) = 
          let NDefault instr = type_ $ cfg M.! (LDefault addr) in
            if extractCondition instr == AL 
              then (LDefault addr) 
              else LBranch addr
        redirectEdgeToConditionals lbl = lbl
    
    removeBranchOfRepeatedCondition :: CFG -> CFGValue -> CFGValue
    removeBranchOfRepeatedCondition cfg val@CFGValue {type_ = NAssertFalse c, successors_ = (s:_)} = 
      val{successors_ = [findRefuteTail c s]}
      where 
        findRefuteTail :: Condition -> CFGKey -> CFGKey
        findRefuteTail c key@(LBranch a) = 
          case type_ $ cfg M.! key of
            NBranch c' -> if c' /= c then key else findRefuteTail c (LAssertFalse a)
        findRefuteTail c key@(LAssertFalse a) = 
          let (s:_) = successors_ $ cfg M.! key
          in
            case type_ $ cfg M.! key of
              NAssertFalse c' -> if c' /= c then key else findRefuteTail c s
        findRefuteTail c key = key
    removeBranchOfRepeatedCondition cfg val@CFGValue {type_ = NDefault instr, successors_ = (s:_)} = 
      let c = extractCondition instr
      in
        if c == AL 
          then val
          else val{successors_ = [findAssertTail c s]}
      where
        findAssertTail :: Condition -> CFGKey -> CFGKey
        findAssertTail c key@(LBranch a) = 
          let NBranch c' = type_ $ cfg M.! key
          in
            if c == c'
              then succ (LAssertTrue a)
              else key
        findAssertTail c key = key
        succ :: CFGKey -> CFGKey
        succ key = 
          let (s:_) = successors_ $ cfg M.! key 
          in s
    removeBranchOfRepeatedCondition cfg val =
      val

    
    fillPredecessors :: CFG -> CFG
    fillPredecessors cfg = 
      let emptyPredecessorCFG = M.map (\val -> val {predecessors_ = []}) cfg in
        M.foldlWithKey' f emptyPredecessorCFG cfg
          where          
            f :: CFG -> CFGLabel -> CFGValue -> CFG
            f reverseCFG lbl CFGValue {successors_ = succs} = 
              M.unionWith concatPredecessors reverseCFG $ M.fromList [ (succ, (cfg M.! succ) {predecessors_ = [lbl]}) | succ <- succs ]
            
            concatPredecessors :: CFGValue -> CFGValue -> CFGValue
            concatPredecessors val@CFGValue {predecessors_ = p1} CFGValue {predecessors_ = p2} = val {predecessors_ = p1 ++ p2}
    
    removeOrphanBranches :: CFG -> CFG
    removeOrphanBranches cfg =
      let 
        g1 = mapmapReplace removeBranch cfg
        g2 = mapmapReplace (removeAssert g1) g1
        g3 = M.map (removeOrphanPredecessorEdges g2) g2
      in
        g3
      where
        removeBranch :: (CFGKey, CFGValue) -> [(CFGKey, CFGValue)]
        removeBranch (key, val@CFGValue{type_ = NBranch c, predecessors_ = []}) = []
        removeBranch pair = [pair]

        removeAssert :: CFG -> (CFGKey, CFGValue) -> [(CFGKey, CFGValue)]
        removeAssert cfg pair@(_, CFGValue{type_ = NAssertTrue _, predecessors_ = (p:_)}) = 
          case cfg M.!? p of
            Nothing -> []
            Just x -> [pair]
        removeAssert cfg pair@(_, CFGValue{type_ = NAssertFalse _, predecessors_ = (p:_)}) = 
          case cfg M.!? p of
            Nothing -> []
            Just x -> [pair]
        removeAssert cfg pair = [pair]
        removeOrphanPredecessorEdges :: CFG -> CFGValue -> CFGValue
        removeOrphanPredecessorEdges cfg val@CFGValue{predecessors_ = p} = val{predecessors_ = prunedEdges p}
          where
            prunedEdges :: [CFGKey] -> [CFGKey]
            prunedEdges [] = []
            prunedEdges (key:rest) = 
              case cfg M.!? key of
                Nothing -> prunedEdges rest
                Just _ -> key:(prunedEdges rest)