module TinyARM.Analysis.Interval where

import TinyARM.Analysis
import TinyARM.Analysis.Types
import TinyARM.Analysis.ConditionLiveness
import TinyARM.Language
import TinyARM.CFG

import Data.Lattice
import Data.List

import TinyARM.Common

import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S



type IntLatticeElem = S.Set (FlagsLattice, IntEssentialLatticeElem)
type IntEssentialLatticeElem = M.Map Register IntervalLattice

prettyPrintState :: IntLatticeElem -> String
-- prettyPrintState s = "{ " ++ (intercalate "\n, " (map prettyStateElem $ S.toList s)) ++ "\n}"
prettyPrintState s = "{ " ++ (intercalate "\n, " (map prettyStateElem $ sort $ map toIntOrder $ S.toList s)) ++ "\n}"
  where
    toIntOrder :: (FlagsLattice, IntEssentialLatticeElem) -> (IntEssentialLatticeElem, FlagsLattice)
    toIntOrder (a,b) = (b,a)

    prettyStateElem :: (IntEssentialLatticeElem, FlagsLattice) -> String
    -- prettyStateElem (flags, intervals) = "("++(show flags)++", "++ (prettyIntervals intervals) ++ ")"
    prettyStateElem (intervals, flags) = "("++(show flags)++", "++ (prettyIntervals intervals) ++ ")"

    prettyIntervals :: IntEssentialLatticeElem -> String
    prettyIntervals intervals = "[" ++ (intercalate "," $ map prettyInterval (M.toList intervals)) ++ "]"

    prettyInterval :: (Register, IntervalLattice) -> String
    prettyInterval (r, l) = " " ++ (show r) ++ " -> " ++ (show l) ++  " "


intervalAnalysis :: CFG -> (M.Map CFGKey IntLatticeElem)
-- intervalAnalysis cfg = worklistAlgorithm cfg initialState join transfer
intervalAnalysis cfg = 
  let 
    conditionLiveness = conditionLivenessAnalysis cfg
    (outmap, inmap) = worklistAlgorithm cfg initialState initialState join (\a b c -> mergeLatticeElementsBasedOnConditions (S.toList $ conditionLiveness M.! a) $ transfer a b c)
  in outmap   
  where
    initialState :: M.Map CFGKey IntLatticeElem
    initialState = M.map (\_ -> bottom) cfg    

    bottom :: IntLatticeElem
    bottom = S.fromList [(FlagsLattice BitBottom BitBottom BitBottom BitBottom, M.empty)]

    join :: [IntLatticeElem] -> IntLatticeElem
    join [] = bottom
    join [x] = x
    join (x:xs) = S.fromList $ M.toList $ M.unionWith ljoin (M.fromList $ S.toList x) (M.fromList $ S.toList (join xs))
    
    transfer :: CFGKey -> CFGNode -> IntLatticeElem -> IntLatticeElem
    transfer _ (NDefault (IMOV _ _ r (IMMVAL v))) inElm = transferAssignImm r v inElm
    transfer _ (NDefault (IMOV _ _ r (REGVAL r'))) inElm = transferAssignReg r r' inElm

    transfer _ (NDefault (IBINOP _ _ op dstReg srcReg vr)) inElm = transferAdd dstReg srcReg op vr  inElm
    transfer k (NDefault (ICMP _ r rv)) inElm = transferCMP r rv inElm
    
    transfer _ (NDefault (ILDR _ r _)) inElm = transferAssignTop r inElm

    transfer _ (NAssertTrue c) inElm = transferAssert c inElm
    transfer _ (NAssertFalse c) inElm = transferRefute c inElm

    
    transfer _ (NDefault (ISTR  _ _ _)) inElm = inElm
    transfer _ (NDefault (IB    _ _))   inElm = inElm
    transfer _ (NDefault (IPUSH _ _))   inElm = inElm
    transfer _ (NDefault (IPOP  _ rs))  inElm = foldr (\r elm -> transferAssignTop r elm) inElm rs
    transfer _ (NDefault (IOUT  _ _))   inElm = inElm
    
    transfer _ (NBranch _)      inElm = inElm
    transfer _ NStart           inElm = inElm
    transfer _ NEnd             inElm = inElm
    transfer _ n _ = error ("Undefined CFG node type for interval analysis [" ++ (show n) ++ "]") 
    -- transfer _ _ inElm = inElm

    transferAssert c inElm = setmapReplace (transferAssert' c) inElm
      where
        transferAssert' c v@(flags, _) = if evalCond c flags then [v] else []

    transferRefute c inElm = setmapReplace (transferRefute' c) inElm
      where
        transferRefute' c v@(flags, _) = if evalCond (negCond c) flags then [v] else []

    transferCMP :: Register -> ValOrReg -> IntLatticeElem -> IntLatticeElem
    transferCMP leftReg (IMMVAL v) state = setmapReplace transferCMPEach state
      where
        transferCMPEach :: (FlagsLattice, IntEssentialLatticeElem) -> [(FlagsLattice, IntEssentialLatticeElem)]
        transferCMPEach (_, intervals) = 
          let
            toResult = (
              \(flags,(leftInterval, rightInterval)) -> 
                (flags, M.insert leftReg (IntLat leftInterval) intervals))

            rightInterval = Interval v v
            
            leftInterval = case intervals M.!? leftReg of 
              Just (IntLat x) -> x
              _ -> error $ "Can't compare with uninitialized register: "++(show leftReg)
          in
            map toResult $ getIntervalsCmp leftInterval rightInterval

    transferCMP leftReg (REGVAL rightReg) state = setmapReplace transferCMPEach state
      where
        transferCMPEach :: (FlagsLattice, IntEssentialLatticeElem) -> [(FlagsLattice, IntEssentialLatticeElem)]
        transferCMPEach (_, intervals) = 
          let
            toResult = (
              \(flags,(leftResultInterval, rightResultInterval)) -> 
                (flags, M.insert leftReg (IntLat leftResultInterval) $ M.insert rightReg (IntLat rightResultInterval) intervals))

            rightInterval = case intervals M.!? rightReg of
              Just (IntLat x) -> x
              _ -> error "Can't compare with uninitialized register"
            
            leftInterval = case intervals M.!? leftReg of 
              Just (IntLat x) -> x
              _ -> error "Can't compare with uninitialized register"
            
          in
            map toResult $ getIntervalsCmp leftInterval rightInterval
                  
                

    transferAssignImm :: Register -> Value -> IntLatticeElem -> IntLatticeElem
    transferAssignImm r v state = S.map 
      (
        \(flags, intervals) -> (flags, M.insert r (IntLat $ Interval v v) intervals)
      ) state

    transferAssignReg :: Register -> Register -> IntLatticeElem -> IntLatticeElem
    transferAssignReg dstReg srcReg state = S.map 
      (
        \(flags, intervals) -> 
          (flags
          , M.insert dstReg (case intervals M.!? srcReg of 
            Nothing -> lbot
            Just x -> x
          ) intervals)
      ) state
    
    transferAssignTop :: Register -> IntLatticeElem -> IntLatticeElem
    transferAssignTop r state = S.map 
      (
        \(flags, intervals) -> (flags, M.insert r ltop intervals)
      ) state
    
    transferAdd :: Register -> Register -> BinOp -> ValOrReg -> IntLatticeElem -> IntLatticeElem
    transferAdd dstReg srcReg op (IMMVAL v) state = 
      S.map (
        \(flags, intervals) -> 
          let 
            srcIntLat = case intervals M.!? srcReg of
              Nothing -> ltop
              Just i  -> i
            f = case op of
              ADD -> wrapIntLatValBinFunc intervalAddConst
              SUB -> wrapIntLatValBinFunc intervalSubConst
          in
            (flags, M.insert dstReg (f srcIntLat v) intervals)
      ) state
    transferAdd dstReg srcReg1 op (REGVAL srcReg2) state = 
      S.map (
        \(flags, intervals) -> 
          let 
            srcIntLat1 = case intervals M.!? srcReg1 of
              Nothing -> ltop
              Just i  -> i
            srcIntLat2 = case intervals M.!? srcReg2 of
              Nothing -> ltop
              Just i  -> i
            f = case op of
              ADD -> wrapIntLatBinFunc intervalAdd
              SUB -> wrapIntLatBinFunc intervalSub
          in
            (flags, M.insert dstReg (f srcIntLat1 srcIntLat2) intervals)
      ) state



mergeLatticeElementsBasedOnConditions :: [Condition] -> IntLatticeElem -> IntLatticeElem
mergeLatticeElementsBasedOnConditions conditions s = S.fromList $ combineLatticeElements compareFlagsFunction $ S.toList s
  where
    compareFlagsFunction :: FlagsLattice -> FlagsLattice -> Bool
    compareFlagsFunction = if length conditions == 0 || True then (==) else (\f1 f2 -> toBucket f1 == toBucket f2)
    
    toBucket :: FlagsLattice -> [Condition]
    toBucket flags = 
      case find (matchesAllConditions flags) (bucketize conditions) of
        Nothing -> []
        Just bucket -> bucket

    matchesAllConditions :: FlagsLattice -> [Condition] -> Bool
    matchesAllConditions flags conds = foldl (\b cond -> b && evalCond cond flags) True conds
    
    -- [EQ] -> [[EQ],[NE]], [EQ,PL] -> [[EQ,PL],[EQ,MI],[NE,PL],[NE,MI]]
    bucketize :: [Condition] -> [[Condition]]
    bucketize [] = []
    bucketize [c] = [[c],[negCond c]]
    bucketize (c:cs) = 
      let recursiveResult = bucketize cs
      in (map ((:) c) recursiveResult) ++ (map ((:) (negCond c)) recursiveResult)

-- if flags are the same, and all intervals are mergable, merge
combineLatticeElements :: (FlagsLattice -> FlagsLattice -> Bool) -> [(FlagsLattice, IntEssentialLatticeElem)] -> [(FlagsLattice, IntEssentialLatticeElem)]
combineLatticeElements _ [] = []
combineLatticeElements flagEquality (v:rest) = newV : (combineLatticeElements flagEquality newRest)
  where 
    -- merge the first element with the rest, return the wider newV and (rest, but with merged elements removed)
    (newV, newRest) = combineSingle v rest
    combineSingle :: (FlagsLattice, IntEssentialLatticeElem) -> [(FlagsLattice, IntEssentialLatticeElem)] -> ((FlagsLattice, IntEssentialLatticeElem), [(FlagsLattice, IntEssentialLatticeElem)])
    combineSingle (flags, intervals) rest = (widenedValue, remainingRest++notSameFlagsRest)
      where
        f = (\(flgs,_) -> flagEquality flags flgs)
        sameFlagsRest = filter f rest
        notSameFlagsRest = filter (not . f) rest
        (widenedValue, remainingRest) = combineSingle' (flags, intervals) sameFlagsRest
      
    -- merge only with those that have same flags
    combineSingle' :: (FlagsLattice, IntEssentialLatticeElem) -> [(FlagsLattice, IntEssentialLatticeElem)] -> ((FlagsLattice, IntEssentialLatticeElem), [(FlagsLattice, IntEssentialLatticeElem)])
    combineSingle' val [] = (val, [])
    combineSingle' (flags, intervals) (x@(f,i):xs) = 
      case mergeIntervalMaps intervals i of
        Just widerIntervals -> combineSingle' (ljoin flags f, widerIntervals) xs
        Nothing             -> (widestVal, x:remainingRest)
      where
        (widestVal, remainingRest) = combineSingle' (flags, intervals) xs

    mergeIntervalMaps :: IntEssentialLatticeElem -> IntEssentialLatticeElem -> Maybe IntEssentialLatticeElem
    mergeIntervalMaps intervals1 intervals2 = 
      if keys1 == keys2
        then let maybeIntervals = M.mapWithKey (\reg int2 -> mergeIntervals (intervals1 M.! reg) int2) intervals2 in
          if M.foldl allJust True maybeIntervals 
            then Just $ unwrapMap maybeIntervals
            else Nothing
        else Nothing
      where
        keys1 = M.keys intervals1
        keys2 = M.keys intervals2 
    
        mergeIntervals :: IntervalLattice -> IntervalLattice -> Maybe IntervalLattice
        mergeIntervals (IntLat (Interval min1 max1)) (IntLat (Interval min2 max2)) = 
          if (toInteger l2) <= ((toInteger r1) + 1)
            then if r2 <= r1 
              then Just $ IntLat $ Interval l1 r1 -- [l2..r2] is inside [l1..r1]
              else Just $ IntLat $ Interval l1 r2 -- [l2..r2] extends [l1..r1]
            else Nothing
          where
            (l1, r1, l2, r2) = if min1 <= min2 then (min1, max1, min2, max2) else (min2, max2, min1, max1) 

        allJust :: Bool -> Maybe IntervalLattice -> Bool
        allJust b Nothing = False
        allJust b (Just _) = b 
    
        unwrapMap :: M.Map Register (Maybe IntervalLattice) -> IntEssentialLatticeElem
        unwrapMap m = M.map (\maybeInterval -> case maybeInterval of Just i -> i) m
