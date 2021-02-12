{-# LANGUAGE NamedFieldPuns,ExplicitForAll,ScopedTypeVariables #-}

module Language.While.CFG where

import CFG
import Language.While.Language

import Debug.Trace
import Data.List
import qualified Data.Map as M
import Text.Parsec



type WhileCFG = CFG SourcePos Cmd
type WhileCFGLabel = CFGLabel SourcePos
type WhileCFGNode = CFGNode Cmd
type WhileCFGValue = CFGValue SourcePos Cmd
type WhileCFGListElement = CFGListElement SourcePos Cmd

makeCFG :: CmdWithPos -> WhileCFG
makeCFG cmd = g1
  where 
    g0 = initialGraph cmd                 -- Create default nodes
--     g1 = mapmapReplace insertBranches g0                  -- Insert conditional structures
--     g2 = M.mapWithKey insertEdges g1                      -- Insert all successor edges
--     g3 = addStartEnd g2                                   -- Add start and end nodes
--     g4 = M.map (redirectEdgesToConditionals g3) g3        -- Redirect all edges pointing to conditional instructions
--     g5 = M.map (removeBranchOfRepeatedCondition g4) g4    -- Map edges of repeated branches
    g1 = fillPredecessors $ M.fromList g0                              -- Create a reverse CFG
--     g7 = removeOrphanBranches g6                          -- Remove branches that no longer have any predecessors
--     cfg = g7

    newEmptyCFGValue :: WhileCFGNode -> WhileCFGValue
    newEmptyCFGValue t = CFGValue {type_= t, successors_ = [], predecessors_ = []}

    newCFGValue :: WhileCFGNode -> [WhileCFGLabel] -> WhileCFGValue
    newCFGValue t succs = CFGValue {type_= t, successors_ = succs, predecessors_ = []}


--     -- emptyCFGValue :: CFGValue
--     -- emptyCFGValue = CFGValue {successors_ = [],predecessors_ = []}

    initialGraph :: CmdWithPos -> [WhileCFGListElement]
    initialGraph cmd = 
        start 
        : 
        middle
        ++
        [end]

        where
            start = (LStart, newCFGValue NStart [LDefault (getPos cmd)])
            middle = (initialGraph' cmd Nothing)
            end = (LEnd, newCFGValue NEnd [])

            initialGraph' :: CmdWithPos -> Maybe SourcePos -> [WhileCFGListElement]
            -- sequences
            initialGraph' (CANNO _ (SEQ (cmd:[]))) nextPos 
                = initialGraph' cmd nextPos
            initialGraph' (CANNO pos (SEQ (cmd1:cmd2:cmds))) nextPos 
                = (initialGraph' cmd1 (Just $ getPos cmd2)) ++ (initialGraph' (CANNO pos (SEQ (cmd2:cmds))) nextPos)
            -- if statements
            initialGraph' (CANNO pos cmd@(IF expr tblk Nothing)) nextPos 
                = makeNode cmd pos [Just $ getPos tblk, nextPos] : initialGraph' tblk nextPos
            initialGraph' (CANNO pos cmd@(IF expr tblk (Just fblk))) nextPos 
                = makeNode cmd pos [Just $ getPos tblk, Just $ getPos fblk] : ((initialGraph' tblk nextPos) ++ (initialGraph' fblk nextPos))
            -- while loops
            initialGraph' (CANNO pos cmd@(WHILE expr loopblk)) nextPos 
                = makeNode cmd pos [Just $ getPos loopblk, nextPos] : initialGraph' loopblk (Just pos)

            -- single lines            
            initialGraph' (CANNO pos cmd) nextPos = (makeNode cmd pos [nextPos]):[]
            initialGraph' _ _ = undefined
            
            getPos :: CmdWithPos -> SourcePos
            getPos (CANNO p _) = p
            getPos _ = undefined

            makeNode :: ACmd a -> SourcePos -> [Maybe SourcePos] -> WhileCFGListElement
            makeNode cmd thisPos succ = (LDefault thisPos, newCFGValue (NDefault $ forgetCmdA cmd) (map mSrcPosToLabel succ))

            mSrcPosToLabel :: Maybe SourcePos -> WhileCFGLabel
            mSrcPosToLabel Nothing = LEnd
            mSrcPosToLabel (Just p) = LDefault p

--     insertBranches :: CFGListElement -> [CFGListElement]
--     insertBranches lstElement@(lbl, CFGValue {type_ = NDefault instr}) = 
--       insertBranches' lstElement (extractCondition instr)
--       where
--         insertBranches' :: CFGListElement -> Condition -> [CFGListElement]
--         insertBranches' origElement AL = 
--           [origElement]
--         insertBranches' origElement@(LDefault addr, _) c =
--           [ origElement
--           , (LBranch addr,      newEmptyCFGValue (NBranch c))
--           , (LAssertTrue addr,  newEmptyCFGValue (NAssertTrue c))
--           , (LAssertFalse addr, newEmptyCFGValue (NAssertFalse c))
--           ]
--     -- The following shouldn't be neccessary..
--     -- insertBranches x = [x]

--     insertEdges :: CFGLabel -> CFGValue -> CFGValue
--     insertEdges (LDefault _) val@CFGValue {type_ = NDefault (IB _ (DIRECT addr))} =       -- branch direct, edge to target 
--       val {successors_ = [LDefault addr]}
--     insertEdges (LDefault addr) val@CFGValue {type_ = NDefault (IB _ (RELATIVE PLUS i))} =   -- branch relative, edge to target + i
--       val {successors_ = [LDefault $ addr + i]}
--     insertEdges (LDefault addr) val@CFGValue {type_ = NDefault (IB _ (RELATIVE MINUS i))} =  -- branch relative, edge to target - i
--       val {successors_ = [LDefault $ addr - i]}
--     insertEdges (LDefault addr) val@CFGValue {type_ = NDefault (IB _ (LABEL l))} =           -- branch label, edge to label's address
--       val {successors_ = [LDefault $ (labels_ prog) M.! l]}

--     insertEdges (LBranch addr)      val@CFGValue {type_ = NBranch _} =                       -- branching node, edges to True and False
--       val {successors_ = [LAssertTrue addr, LAssertFalse addr]}
--     insertEdges (LAssertTrue addr)  val@CFGValue {type_ = NAssertTrue _} =                   -- true node, edge to original node
--       val {successors_ = [LDefault addr]}
--     insertEdges (LAssertFalse addr) val@CFGValue {type_ = NAssertFalse _} =                  -- false node, edge to next default node           
--       val {successors_ = [LDefault $ addr + 1]}

--     insertEdges LEnd    val@CFGValue {type_ = NEnd} =                                        -- end node
--       val {successors_ = []}
--     insertEdges LStart  val@CFGValue {type_ = NStart} =                                      -- start node
--       val {successors_ = [LDefault 0]}
      
--     insertEdges (LDefault addr) val@CFGValue {type_ = NDefault i} =                          -- all other nodes
--       val {successors_ = [LDefault $ addr + 1]}

--     -- The following shouldn't be neccessary..
--     -- insertEdges _ x = trace (show x) x

--     addStartEnd :: CFG -> CFG
--     addStartEnd cfg = addEnd $ replaceEdgesToHalt $ addStart cfg
--       where
--         addStart :: CFG -> CFG
--         addStart cfg = M.insert LStart (newEmptyCFGValue NStart) {successors_ = [LDefault 0]} cfg

--         replaceEdgesToHalt :: CFG -> CFG
--         replaceEdgesToHalt cfg = M.map replaceEdgesToHalt' cfg
        
--         replaceEdgesToHalt' :: CFGValue -> CFGValue
--         replaceEdgesToHalt' val@CFGValue {successors_ = edges} = val {successors_ = map replaceEdgesToHalt'' edges}

--         replaceEdgesToHalt'' :: CFGLabel -> CFGLabel
--         replaceEdgesToHalt'' edge = case cfg M.! edge of
--           CFGValue {type_ = NDefault (META (HALT _) _)} -> LEnd
--           _                                             -> edge

--         addEnd :: CFG -> CFG
--         addEnd cfg = mapmap haltToEnd cfg
        
--         haltToEnd :: CFGListElement -> CFGListElement
--         haltToEnd (_, val@CFGValue {type_ = NDefault (META (HALT _) _)}) = (LEnd, newEmptyCFGValue NEnd)
--         haltToEnd elem = elem

--     redirectEdgesToConditionals :: CFG -> CFGValue -> CFGValue
--     redirectEdgesToConditionals cfg val@CFGValue {type_ = NAssertTrue _} = val -- don't touch NAssertTrue (creates unintentional loop)
--     redirectEdgesToConditionals cfg val@CFGValue {successors_ = edges} = 
--       val {successors_ = map redirectEdgeToConditionals edges}
--       where        
--         redirectEdgeToConditionals :: CFGLabel -> CFGLabel
--         redirectEdgeToConditionals (LDefault addr) = 
--           let NDefault instr = type_ $ cfg M.! (LDefault addr) in
--             if extractCondition instr == AL 
--               then (LDefault addr) 
--               else LBranch addr
--         redirectEdgeToConditionals lbl = lbl
    
--     removeBranchOfRepeatedCondition :: CFG -> CFGValue -> CFGValue
--     removeBranchOfRepeatedCondition cfg val@CFGValue {type_ = NAssertFalse c, successors_ = (s:_)} = 
--       val{successors_ = [findRefuteTail c s]}
--       where 
--         findRefuteTail :: Condition -> CFGKey -> CFGKey
--         findRefuteTail c key@(LBranch a) = 
--           case type_ $ cfg M.! key of
--             NBranch c' -> if c' /= c then key else findRefuteTail c (LAssertFalse a)
--         findRefuteTail c key@(LAssertFalse a) = 
--           let (s:_) = successors_ $ cfg M.! key
--           in
--             case type_ $ cfg M.! key of
--               NAssertFalse c' -> if c' /= c then key else findRefuteTail c s
--         findRefuteTail c key = key
--     removeBranchOfRepeatedCondition cfg val@CFGValue {type_ = NDefault instr, successors_ = (s:_)} = 
--       let c = extractCondition instr
--       in
--         if c == AL 
--           then val
--           else val{successors_ = [findAssertTail c s]}
--       where
--         findAssertTail :: Condition -> CFGKey -> CFGKey
--         findAssertTail c key@(LBranch a) = 
--           let NBranch c' = type_ $ cfg M.! key
--           in
--             if c == c'
--               then succ (LAssertTrue a)
--               else key
--         findAssertTail c key = key
--         succ :: CFGKey -> CFGKey
--         succ key = 
--           let (s:_) = successors_ $ cfg M.! key 
--           in s
--     removeBranchOfRepeatedCondition cfg val =
--       val

    
    fillPredecessors :: WhileCFG -> WhileCFG
    fillPredecessors cfg = 
      let emptyPredecessorCFG = M.map (\val -> val {predecessors_ = []}) cfg in
        M.foldlWithKey' f emptyPredecessorCFG cfg
          where          
            f :: WhileCFG -> WhileCFGLabel -> WhileCFGValue -> WhileCFG
            f reverseCFG lbl CFGValue {successors_ = succs} = 
              M.unionWith concatPredecessors reverseCFG $ M.fromList [ (succ, (cfg M.! succ) {predecessors_ = [lbl]}) | succ <- succs ]
            
            concatPredecessors :: WhileCFGValue -> WhileCFGValue -> WhileCFGValue
            concatPredecessors val@CFGValue {predecessors_ = p1} CFGValue {predecessors_ = p2} = val {predecessors_ = p1 ++ p2}
    
--     removeOrphanBranches :: CFG -> CFG
--     removeOrphanBranches cfg =
--       let 
--         g1 = mapmapReplace removeBranch cfg
--         g2 = mapmapReplace (removeAssert g1) g1
--         g3 = M.map (removeOrphanPredecessorEdges g2) g2
--       in
--         g3
--       where
--         removeBranch :: (CFGKey, CFGValue) -> [(CFGKey, CFGValue)]
--         removeBranch (key, val@CFGValue{type_ = NBranch c, predecessors_ = []}) = []
--         removeBranch pair = [pair]

--         removeAssert :: CFG -> (CFGKey, CFGValue) -> [(CFGKey, CFGValue)]
--         removeAssert cfg pair@(_, CFGValue{type_ = NAssertTrue _, predecessors_ = (p:_)}) = 
--           case cfg M.!? p of
--             Nothing -> []
--             Just x -> [pair]
--         removeAssert cfg pair@(_, CFGValue{type_ = NAssertFalse _, predecessors_ = (p:_)}) = 
--           case cfg M.!? p of
--             Nothing -> []
--             Just x -> [pair]
--         removeAssert cfg pair = [pair]
--         removeOrphanPredecessorEdges :: CFG -> CFGValue -> CFGValue
--         removeOrphanPredecessorEdges cfg val@CFGValue{predecessors_ = p} = val{predecessors_ = prunedEdges p}
--           where
--             prunedEdges :: [CFGKey] -> [CFGKey]
--             prunedEdges [] = []
--             prunedEdges (key:rest) = 
--               case cfg M.!? key of
--                 Nothing -> prunedEdges rest
--                 Just _ -> key:(prunedEdges rest)