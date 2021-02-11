{-# LANGUAGE NamedFieldPuns,ExplicitForAll,ScopedTypeVariables #-}

module Language.TinyARM.CFG where

import Language.TinyARM.Language
import Common
import CFG
import Debug.Trace
import Data.List
import qualified Data.Map as M


import Debug.Trace
import Data.List
import qualified Data.Map as M

type TinyArmCFG = CFG Address Instr
type TinyArmCFGLabel = CFGLabel Address
type TinyArmCFGNode = CFGNode Instr
type TinyArmCFGValue = CFGValue Address Instr
type TinyArmCFGListElement = CFGListElement Address Instr


makeCFG :: Program -> TinyArmCFG
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

    newEmptyCFGValue :: TinyArmCFGNode -> TinyArmCFGValue
    newEmptyCFGValue t = CFGValue {type_= t, successors_ = [],predecessors_ = []}


    -- emptyCFGValue :: CFGValue
    -- emptyCFGValue = CFGValue {successors_ = [],predecessors_ = []}

    initialGraph :: (Address, Instr) -> TinyArmCFGListElement
    initialGraph (addr, instr) = (LDefault addr, newEmptyCFGValue (NDefault instr))

    insertBranches :: TinyArmCFGListElement -> [TinyArmCFGListElement]
    insertBranches lstElement@(lbl, CFGValue {type_ = NDefault instr}) = 
      insertBranches' lstElement (extractCondition instr)
      where
        insertBranches' :: TinyArmCFGListElement -> Condition -> [TinyArmCFGListElement]
        insertBranches' origElement AL = 
          [origElement]
        insertBranches' origElement@(LDefault addr, _) c =
          [ origElement
          , (LBranch addr,      newEmptyCFGValue (NBranch (META (COND c) Nothing)))
          , (LAssertTrue addr,  newEmptyCFGValue (NAssertTrue (META (COND c) Nothing)))
          , (LAssertFalse addr, newEmptyCFGValue (NAssertFalse (META (COND c) Nothing)))
          ]
    -- The following shouldn't be neccessary..
    -- insertBranches x = [x]

    insertEdges :: TinyArmCFGLabel -> TinyArmCFGValue -> TinyArmCFGValue
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

    addStartEnd :: TinyArmCFG -> TinyArmCFG
    addStartEnd cfg = addEnd $ replaceEdgesToHalt $ addStart cfg
      where
        addStart :: TinyArmCFG -> TinyArmCFG
        addStart cfg = M.insert LStart (newEmptyCFGValue NStart) {successors_ = [LDefault 0]} cfg

        replaceEdgesToHalt :: TinyArmCFG -> TinyArmCFG
        replaceEdgesToHalt cfg = M.map replaceEdgesToHalt' cfg
        
        replaceEdgesToHalt' :: TinyArmCFGValue -> TinyArmCFGValue
        replaceEdgesToHalt' val@CFGValue {successors_ = edges} = val {successors_ = map replaceEdgesToHalt'' edges}

        replaceEdgesToHalt'' :: TinyArmCFGLabel -> TinyArmCFGLabel
        replaceEdgesToHalt'' edge = case cfg M.! edge of
          CFGValue {type_ = NDefault (META (HALT _) _)} -> LEnd
          _                                             -> edge

        addEnd :: TinyArmCFG -> TinyArmCFG
        addEnd cfg = mapmap haltToEnd cfg
        
        haltToEnd :: TinyArmCFGListElement -> TinyArmCFGListElement
        haltToEnd (_, val@CFGValue {type_ = NDefault (META (HALT _) _)}) = (LEnd, newEmptyCFGValue NEnd)
        haltToEnd elem = elem

    redirectEdgesToConditionals :: TinyArmCFG -> TinyArmCFGValue -> TinyArmCFGValue
    redirectEdgesToConditionals cfg val@CFGValue {type_ = NAssertTrue _} = val -- don't touch NAssertTrue (creates unintentional loop)
    redirectEdgesToConditionals cfg val@CFGValue {successors_ = edges} = 
      val {successors_ = map redirectEdgeToConditionals edges}
      where        
        redirectEdgeToConditionals :: TinyArmCFGLabel -> TinyArmCFGLabel
        redirectEdgeToConditionals (LDefault addr) = 
          let NDefault instr = type_ $ cfg M.! (LDefault addr) in
            if extractCondition instr == AL 
              then (LDefault addr) 
              else LBranch addr
        redirectEdgeToConditionals lbl = lbl
    
    removeBranchOfRepeatedCondition :: TinyArmCFG -> TinyArmCFGValue -> TinyArmCFGValue
    removeBranchOfRepeatedCondition cfg val@CFGValue {type_ = NAssertFalse (META (COND c) _), successors_ = (s:_)} = 
      val{successors_ = [findRefuteTail c s]}
      where 
        findRefuteTail :: Condition -> TinyArmCFGLabel -> TinyArmCFGLabel
        findRefuteTail c key@(LBranch a) = 
          case type_ $ cfg M.! key of
            NBranch (META (COND c') _) -> if c' /= c then key else findRefuteTail c (LAssertFalse a)
        findRefuteTail c key@(LAssertFalse a) = 
          let (s:_) = successors_ $ cfg M.! key
          in
            case type_ $ cfg M.! key of
              NAssertFalse (META (COND c') _) -> if c' /= c then key else findRefuteTail c s
        findRefuteTail c key = key
    removeBranchOfRepeatedCondition cfg val@CFGValue {type_ = NDefault instr, successors_ = (s:_)} = 
      let c = extractCondition instr
      in
        if c == AL 
          then val
          else val{successors_ = [findAssertTail c s]}
      where
        findAssertTail :: Condition -> TinyArmCFGLabel -> TinyArmCFGLabel
        findAssertTail c key@(LBranch a) = 
          let NBranch (META (COND c') _) = type_ $ cfg M.! key
          in
            if c == c'
              then succ (LAssertTrue a)
              else key
        findAssertTail c key = key
        succ :: TinyArmCFGLabel -> TinyArmCFGLabel
        succ key = 
          let (s:_) = successors_ $ cfg M.! key 
          in s
    removeBranchOfRepeatedCondition cfg val =
      val

    
    fillPredecessors :: TinyArmCFG -> TinyArmCFG
    fillPredecessors cfg = 
      let emptyPredecessorCFG = M.map (\val -> val {predecessors_ = []}) cfg in
        M.foldlWithKey' f emptyPredecessorCFG cfg
          where          
            f :: TinyArmCFG -> TinyArmCFGLabel -> TinyArmCFGValue -> TinyArmCFG
            f reverseCFG lbl CFGValue {successors_ = succs} = 
              M.unionWith concatPredecessors reverseCFG $ M.fromList [ (succ, (cfg M.! succ) {predecessors_ = [lbl]}) | succ <- succs ]
            
            concatPredecessors :: TinyArmCFGValue -> TinyArmCFGValue -> TinyArmCFGValue
            concatPredecessors val@CFGValue {predecessors_ = p1} CFGValue {predecessors_ = p2} = val {predecessors_ = p1 ++ p2}
    
    removeOrphanBranches :: TinyArmCFG -> TinyArmCFG
    removeOrphanBranches cfg =
      let 
        g1 = mapmapReplace removeBranch cfg
        g2 = mapmapReplace (removeAssert g1) g1
        g3 = M.map (removeOrphanPredecessorEdges g2) g2
      in
        g3
      where
        removeBranch :: (TinyArmCFGLabel, TinyArmCFGValue) -> [(TinyArmCFGLabel, TinyArmCFGValue)]
        removeBranch (key, val@CFGValue{type_ = NBranch c, predecessors_ = []}) = []
        removeBranch pair = [pair]

        removeAssert :: TinyArmCFG -> (TinyArmCFGLabel, TinyArmCFGValue) -> [(TinyArmCFGLabel, TinyArmCFGValue)]
        removeAssert cfg pair@(_, CFGValue{type_ = NAssertTrue _, predecessors_ = (p:_)}) = 
          case cfg M.!? p of
            Nothing -> []
            Just x -> [pair]
        removeAssert cfg pair@(_, CFGValue{type_ = NAssertFalse _, predecessors_ = (p:_)}) = 
          case cfg M.!? p of
            Nothing -> []
            Just x -> [pair]
        removeAssert cfg pair = [pair]
        removeOrphanPredecessorEdges :: TinyArmCFG -> TinyArmCFGValue -> TinyArmCFGValue
        removeOrphanPredecessorEdges cfg val@CFGValue{predecessors_ = p} = val{predecessors_ = prunedEdges p}
          where
            prunedEdges :: [TinyArmCFGLabel] -> [TinyArmCFGLabel]
            prunedEdges [] = []
            prunedEdges (key:rest) = 
              case cfg M.!? key of
                Nothing -> prunedEdges rest
                Just _ -> key:(prunedEdges rest)