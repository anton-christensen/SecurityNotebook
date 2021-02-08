module TinyARM.Analysis.Bitflip where

import TinyARM.Analysis
import TinyARM.Analysis.Types
import TinyARM.Analysis.ConditionLiveness
import TinyARM.Language
import TinyARM.CFG

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Lattice
import Data.List

import TinyARM.Common

import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S


data Bitflips = Bitflips 
  { flipPos_ :: [Value] -- bit indexes that can have been fliped from 0 to 1
  , flipNeg_ :: [Value] -- bit indexes that can have been fliped from 1 to 0
  } deriving (Eq, Ord)

instance Show Bitflips where
  show bf = "+" ++ (show $ flipPos_ bf) ++ " -" ++ (show $ flipNeg_ bf)

instance Lattice Bitflips where
  lbot = noFlips
  -- finiteBitSize :: Value)
  ltop = Bitflips{flipPos_ = [0..(bitWidth-1)], flipNeg_ = [0..(bitWidth-1)]}
  ljoin a b = Bitflips{flipPos_ = (makeUnique ((flipPos_ a) ++ (flipPos_ b))), flipNeg_ = (makeUnique ((flipNeg_ a) ++ (flipNeg_ b)))}
  lmeet a b = Bitflips{flipPos_ = (filter ((flip elem) $ flipPos_ a) (flipPos_ b)), flipNeg_ = (filter ((flip elem) $ flipNeg_ a) (flipNeg_ b))}

noFlips :: Bitflips
noFlips = Bitflips{flipPos_ = [], flipNeg_ = []} 

type BitflipLatticeElem = M.Map BitflipKey FlagToRegisterMap
type BitflipKey = Maybe (CFGKey, Register)
type FlagToRegisterMap = S.Set (FlagsLattice, (RegisterToIntervalMap, Bitflips))
type RegisterToIntervalMap = M.Map Register IntervalLattice

prettyPrintState :: BitflipLatticeElem -> String
-- prettyPrintState lat = "[\n" ++ (intercalate "\n" (M.map prettyStateElem $ sort $ map toIntOrder $ M.toList lat)) ++ "\n]"
prettyPrintState lat = printL0Map lat


printL0Map :: BitflipLatticeElem -> String
printL0Map = printMultilineMap printL0Tuple
-- printL0Map l0Map = "[\n" ++ (indent tabWidth $ (intercalate ",\n" (M.map printL0Tuple $ M.toList l0Map))) ++ "\n]"

printL0Tuple :: (BitflipKey, FlagToRegisterMap) -> String
printL0Tuple (bitflipInfo, l1Map) = (printBitflipKey bitflipInfo) ++ " ->\n" ++ (indent tabWidth $ printL1Map l1Map)

printBitflipKey :: BitflipKey -> String
-- Maybe (CFGKey, Register, Bitflips) -> String
printBitflipKey Nothing = "No flips"
printBitflipKey (Just (key, reg)) = "lbl: "++(show key)++", reg: "++(show reg)

printL1Map :: FlagToRegisterMap -> String
printL1Map m = "{\n" ++ (indent tabWidth $ (intercalate ",\n" (map printL1Tuple $ sort $ map toIntOrder $ S.toList m))) ++ "\n}"
-- printL1Map = printMultilineSet printL1Tuple

printL1Tuple :: ((RegisterToIntervalMap, Bitflips), FlagsLattice) -> String
printL1Tuple ((l2Map, flips), flags) = (show flags) ++ " -> " ++ (printL2Map l2Map) ++ " " ++ (show flips)

printL2Map :: RegisterToIntervalMap -> String
printL2Map = printInlineMap printL2Tuple

printL2Tuple :: (Register, IntervalLattice) -> String
printL2Tuple (r, i) = (show r) ++ " -> " ++ (show i)
    

toIntOrder :: (a, b) -> (b, a)
toIntOrder (a,b) = (b,a)

bitflipAnalysis :: CFG -> (M.Map CFGKey BitflipLatticeElem)
bitflipAnalysis cfg = outmap
  where
    conditionLiveness = conditionLivenessAnalysis cfg
    (outmap, inmap) = worklistAlgorithm cfg initialState initialState join (\key node state -> mergeLatticeElementsBasedOnConditions (S.toList $ conditionLiveness M.! key) $ transfer key node state)

    initialState :: M.Map CFGKey BitflipLatticeElem
    initialState = M.map (\_ -> bottom) cfg
    
    bottom :: BitflipLatticeElem
    bottom = M.fromList [(Nothing, S.empty)]

    join :: [BitflipLatticeElem] -> BitflipLatticeElem
    join [] = bottom
    join [x] = x
    join (x:xs) = ljoin x $ join xs

    transfer :: CFGKey -> CFGNode -> BitflipLatticeElem -> BitflipLatticeElem
    transfer _ (NDefault (IMOV _ _ r (IMMVAL v))) inElm = transferAssignImm r v inElm
    transfer _ (NDefault (IMOV _ _ r (REGVAL r'))) inElm = transferAssignReg r r' inElm

    transfer k (NDefault (IBINOP s _ op dstReg srcReg vr)) inElm = transferAdd k s dstReg srcReg op vr inElm
    transfer k (NDefault (ICMP _ r rv))                    inElm = transferCMP k r rv inElm
    transfer k (NDefault (ISTR _ r ra))                    inElm = transferSTR k r ra inElm
    transfer k (NDefault (IOUT _ rv))                      inElm = transferOUT k rv   inElm
    
    transfer _ (NDefault (ILDR _ r _)) inElm = transferAssignTop r inElm

    transfer _ (NAssertTrue c) inElm = transferAssert c inElm
    transfer _ (NAssertFalse c) inElm = transferRefute c inElm
    
    transfer _ _ inElm = inElm

    transferAssert :: Condition -> BitflipLatticeElem -> BitflipLatticeElem
    transferAssert c inElm = M.map (setmapReplace (transferAssert' c)) inElm
      where
        transferAssert' c v@(flags, _) = if evalCond c flags then [v] else []

    transferRefute :: Condition -> BitflipLatticeElem -> BitflipLatticeElem
    transferRefute c inElm = M.map (setmapReplace (transferRefute' c)) inElm
      where
        transferRefute' c v@(flags, _) = if evalCond (negCond c) flags then [v] else []

    -- calculates more specific bitflips based on the result of the transfer functions
    flipRegisters :: (BitflipLatticeElem -> BitflipLatticeElem) -> (BitflipLatticeElem -> BitflipLatticeElem) -> CFGKey -> [Register] -> BitflipLatticeElem -> BitflipLatticeElem
    flipRegisters transfPre transfPost key regs state =
      let 
        newKeys = [Just (key, r) | r <- regs]
        expandedWorlds = expandWorldsOnRegisters key regs state
        updatedState = transfPre expandedWorlds
        bitflipedStateElems = M.fromList [(Just (key, r), calculateBitflips r (state M.! Nothing) (updatedState M.! (Just (key, r)))) | r <- regs]
        updatedWithFlips =  M.union bitflipedStateElems updatedState
      in
        transfPost updatedWithFlips
    
    -- flipRegisters :: (BitflipLatticeElem -> BitflipLatticeElem) -> CFGKey -> [Register] -> BitflipLatticeElem -> BitflipLatticeElem
    -- flipRegisters transf key regs state =
    --   let 
    --     newKeys = [Just (key, r) | r <- regs]
    --     expandedWorlds = expandWorldsOnRegisters key regs state
    --   in
    --     transf expandedWorlds


    -- do operation, then calculate flips, high precision
    transferCMP :: CFGKey -> Register -> ValOrReg -> BitflipLatticeElem -> BitflipLatticeElem
    transferCMP key leftReg rv@(IMMVAL _)        state = flipRegisters (transferCMP' getIntervalsCmp leftReg rv) id key [leftReg] state 
    transferCMP key leftReg rv@(REGVAL rightReg) state = flipRegisters (transferCMP' getIntervalsCmp leftReg rv) id key [leftReg, rightReg] state 

    transferCMP' :: (Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]) -> Register -> ValOrReg -> BitflipLatticeElem -> BitflipLatticeElem
    transferCMP' cmpFunction leftReg (IMMVAL v) state = M.map (setmapReplace transferCMPEach) state
      where
        transferCMPEach :: (FlagsLattice, (RegisterToIntervalMap, Bitflips)) -> [(FlagsLattice, (RegisterToIntervalMap, Bitflips))]
        transferCMPEach (_, (intervals, flips)) = 
          let
            toResult = (
              \(flags,(leftInterval, rightInterval)) -> 
                (flags, (M.insert leftReg (IntLat leftInterval) intervals, flips)))

            rightInterval = Interval v v
            
            leftInterval = case intervals M.!? leftReg of 
              Just (IntLat x) -> x
              _ -> error "Can't compare with uninitialized register"
          in
            map toResult $ cmpFunction leftInterval rightInterval

    transferCMP' cmpFunction leftReg (REGVAL rightReg) state = M.map (setmapReplace transferCMPEach) state
      where
        transferCMPEach :: (FlagsLattice, (RegisterToIntervalMap, Bitflips)) -> [(FlagsLattice, (RegisterToIntervalMap, Bitflips))]
        transferCMPEach (_, (intervals, flips)) = 
          let
            toResult = (
              \(flags,(leftResultInterval, rightResultInterval)) -> 
                ( flags
                , (M.insert leftReg (IntLat leftResultInterval) $ 
                    M.insert rightReg (IntLat rightResultInterval) 
                      intervals
                , flips)
                )
              )

            rightInterval = case intervals M.!? rightReg of
              Just (IntLat x) -> x
              _ -> error "Can't compare with uninitialized register"
            
            leftInterval = case intervals M.!? leftReg of 
              Just (IntLat x) -> x
              _ -> error "Can't compare with uninitialized register"
            
          in
            map toResult $ cmpFunction leftInterval rightInterval



    transferAssignImm :: Register -> Value -> BitflipLatticeElem -> BitflipLatticeElem
    transferAssignImm r v state = lvl2MapWithBottomIfEmpty transferAssignImmEach state
      where
        transferAssignImmEach :: (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)
        transferAssignImmEach (intervals, flips) = (M.insert r (IntLat $ Interval v v) intervals, flips)

    transferAssignReg :: Register -> Register -> BitflipLatticeElem -> BitflipLatticeElem
    transferAssignReg dstReg srcReg state = lvl2MapWithBottomIfEmpty transferAssignRegEach state
      where
        transferAssignRegEach :: (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)
        transferAssignRegEach (intervals, flips) = (M.insert dstReg lookup intervals, flips)
          where
            lookup = case intervals M.!? srcReg of 
              Nothing -> lbot
              Just el -> el
    
    transferAssignTop :: Register -> BitflipLatticeElem -> BitflipLatticeElem
    transferAssignTop r state = lvl2MapWithBottomIfEmpty transferAssignTopEach state
      where
        transferAssignTopEach :: (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)
        transferAssignTopEach (intervals, flips) = (M.insert r ltop intervals, flips)
    

    vorrToReg :: ValOrReg -> [Register]
    vorrToReg (REGVAL r) = [r]
    vorrToReg _ = []
    aorrToReg :: AddrOrReg -> [Register]
    aorrToReg (REGADDR r) = [r]
    aorrToReg (REGADDRIMMOFFSET r _ _) = [r]
    aorrToReg (REGADDRREGOFFSET r1 _ r2) = [r1,r2]
    aorrToReg _ = []
    
    transferSTR :: CFGKey -> Register -> AddrOrReg -> BitflipLatticeElem -> BitflipLatticeElem
    transferSTR key r ra state = flipRegisters id id key (r:(aorrToReg ra)) state
    transferOUT :: CFGKey -> ValOrReg -> BitflipLatticeElem -> BitflipLatticeElem
    transferOUT key rv state = flipRegisters id id key (vorrToReg rv) state

    transferAdd  :: CFGKey -> SetFlags -> Register -> Register -> BinOp -> ValOrReg -> BitflipLatticeElem -> BitflipLatticeElem
    transferAdd key setsFlags dstReg srcReg op rv state = 
      let 
        preFlipTransf = case setsFlags of 
          NoSet -> id 
          DoSet -> (transferCMP' (getIntervalsCmpFunction op) srcReg rv)
        postFlipTransf = transferAdd' dstReg srcReg op rv
      in 
        flipRegisters preFlipTransf postFlipTransf key (srcReg : (vorrToReg rv)) state

    transferAdd' :: Register -> Register -> BinOp -> ValOrReg -> BitflipLatticeElem -> BitflipLatticeElem
    transferAdd' dstReg srcReg op rv state = lvl2MapWithBottomIfEmpty transferAdd'' state
      where 
        f = case op of
          ADD -> wrapIntLatBinFunc intervalAdd
          SUB -> wrapIntLatBinFunc intervalSub

        transferAdd'' :: (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)
        transferAdd'' (intervals, flips) = 
          let 
            srcInt1 = case intervals M.!? srcReg of
              Nothing -> ltop
              Just i  -> i
            srcInt2 = case rv of
              (IMMVAL v) -> IntLat (Interval v v)
              (REGVAL r) -> case intervals M.!? r of
                Nothing -> ltop
                Just i  -> i
          in 
            (M.insert dstReg (f srcInt1 srcInt2) intervals, flips)

    lvl2MapWithBottomIfEmpty :: ((RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)) -> BitflipLatticeElem -> BitflipLatticeElem
    lvl2MapWithBottomIfEmpty f state = 
      if state == M.empty
        then M.map (ifEmptyHandler f) $ M.fromList [(Nothing, S.empty)]
        else M.map (ifEmptyHandler f) state
      where
        ifEmptyHandler :: ((RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)) -> FlagToRegisterMap -> FlagToRegisterMap
        ifEmptyHandler f intervals = 
          if intervals == S.empty
            then S.map (\(a,b) -> (a, f b)) bottomFlagToRegisterMap
            else S.map (\(a,b) -> (a, f b)) intervals
        
        bottomFlagToRegisterMap :: FlagToRegisterMap
        bottomFlagToRegisterMap = S.fromList [(FlagsLattice BitBottom BitBottom BitBottom BitBottom, (M.empty, noFlips))]




calculateBitflips :: Register -> FlagToRegisterMap -> FlagToRegisterMap -> FlagToRegisterMap
calculateBitflips reg stateBeforeSplit stateAfterSplit = 
  -- foreach register reg interval in the afterSplit
    -- foreach register reg interval in the beforeSplit
      -- calculate bitflips
    -- combine
  S.map (\(a,b) -> (a, calculateBitflipsEach b)) stateAfterSplit
  where
    calculateBitflipsEach :: (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)
    calculateBitflipsEach (intervals, _) = 
      case intervals M.!? reg of
        Nothing -> (intervals, noFlips)
        Just Bottom -> (intervals, noFlips)
        Just (IntLat intervalAfter) -> 
          (intervals, ljoins $ map (calculateBitflipsEach' intervalAfter) $ S.toList stateBeforeSplit)
      where
        calculateBitflipsEach' :: Interval -> (FlagsLattice, (RegisterToIntervalMap, Bitflips)) -> Bitflips
        calculateBitflipsEach' intervalTo (_, (intervalsFrom, _)) = 
          case intervalsFrom M.!? reg of
            Nothing -> noFlips
            Just Bottom -> noFlips
            Just (IntLat intervalFrom) -> 
              let (flipPos, flipNeg) = getPossibleFlips intervalFrom intervalTo
              in Bitflips{flipPos_=flipPos,flipNeg_=flipNeg}
  

-- given a line (key) and what registers are used, add an element to the state that indicates that bitflips can happen at that line, on those registers
-- also widen the intervals of the fliped register to any value that it could potentially be
expandWorldsOnRegisters :: CFGKey -> [Register] -> BitflipLatticeElem -> BitflipLatticeElem
expandWorldsOnRegisters key registers state = mapmapReplace expand' state
  where 
    expand' :: (BitflipKey, FlagToRegisterMap) -> [(BitflipKey, FlagToRegisterMap)]
    expand' x@(Just _, _) = [x]
    expand' x@(Nothing, flgToRegMap) = x:(map (expand'' flgToRegMap) registers)

    expand'' :: FlagToRegisterMap -> Register -> (BitflipKey, FlagToRegisterMap)
    expand'' flgToRegMap reg = (Just (key,reg), newmap)
      where newmap = S.map (\(a,b) -> (a, expandSpecificRegister reg b)) flgToRegMap

    expandSpecificRegister :: Register -> (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips)
    expandSpecificRegister reg (regMap, _) = 
      case regMap M.!? reg of
        Nothing       -> 
          let
            (expandedInterval, flips) = wrapExpandInterval ltop
            expandedRegMap = M.insert reg expandedInterval regMap
          in (expandedRegMap, flips)
        Just interval -> 
          let  
            (expandedInterval, flips) = wrapExpandInterval interval
            expandedRegMap = M.insert reg expandedInterval regMap 
          in (expandedRegMap, flips)
           

    
    wrapExpandInterval :: IntervalLattice -> (IntervalLattice, Bitflips)
    wrapExpandInterval Bottom = undefined -- (Bottom, noFlips) ???
    wrapExpandInterval (IntLat interval) = 
      let 
        expandedInterval = expandInterval interval
        (flipPos, flipNeg) = getPossibleFlips interval expandedInterval
      in (IntLat expandedInterval, Bitflips{flipPos_=flipPos,flipNeg_=flipNeg})

mergeLatticeElementsBasedOnConditions :: [Condition] -> BitflipLatticeElem -> BitflipLatticeElem
mergeLatticeElementsBasedOnConditions conditions s = 
  let 
    prunedResult = M.mapWithKey (\key -> S.filter (\(_, (_, flips)) -> flips /= noFlips || isNothing key)) s
    mergedResult = M.map ( S.fromList . (combineLatticeElements compareFlagsFunction) . S.toList) prunedResult

  in mergedResult
  where
    compareFlagsFunction :: FlagsLattice -> FlagsLattice -> Bool
    compareFlagsFunction = if length conditions == 0 ||  True {-- overrides and set to flags-are-exactly-equal function --} then (==) else (\f1 f2 -> toBucket f1 == toBucket f2)
    
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
combineLatticeElements :: (FlagsLattice -> FlagsLattice -> Bool) -> [(FlagsLattice, (RegisterToIntervalMap, Bitflips))] -> [(FlagsLattice, (RegisterToIntervalMap, Bitflips))]
combineLatticeElements _ [] = []
combineLatticeElements flagEquality (v:rest) = newV : (combineLatticeElements flagEquality newRest)
  where 
    -- merge the first element with the rest, return the wider newV and (rest, but with merged elements removed)
    (newV, newRest) = combineSingle v rest
    combineSingle :: (FlagsLattice, (RegisterToIntervalMap, Bitflips)) -> [(FlagsLattice, (RegisterToIntervalMap, Bitflips))] -> ((FlagsLattice, (RegisterToIntervalMap, Bitflips)), [(FlagsLattice, (RegisterToIntervalMap, Bitflips))])
    combineSingle (flags, intervals) rest = (widenedValue, remainingRest++notSameFlagsRest)
      where
        f = (\(flgs,_) -> flagEquality flags flgs)
        sameFlagsRest = filter f rest
        notSameFlagsRest = filter (not . f) rest
        (widenedValue, remainingRest) = combineSingle' (flags, intervals) sameFlagsRest
      
    -- merge only with those that have same flags
    combineSingle' :: (FlagsLattice, (RegisterToIntervalMap, Bitflips)) -> [(FlagsLattice, (RegisterToIntervalMap, Bitflips))] -> ((FlagsLattice, (RegisterToIntervalMap, Bitflips)), [(FlagsLattice, (RegisterToIntervalMap, Bitflips))])
    combineSingle' val [] = (val, [])
    combineSingle' (flags, intervals) (x@(f,i):xs) = 
      case mergeIntervalMaps intervals i of
        Just widerIntervals -> combineSingle' (ljoin flags f, widerIntervals) xs
        Nothing             -> (widestVal, x:remainingRest)
      where
        (widestVal, remainingRest) = combineSingle' (flags, intervals) xs

    mergeIntervalMaps :: (RegisterToIntervalMap, Bitflips) -> (RegisterToIntervalMap, Bitflips) -> Maybe (RegisterToIntervalMap, Bitflips)
    mergeIntervalMaps (intervals1, flips1) (intervals2, flips2) = 
      if flips1 == flips2
        then if keys1 == keys2
          then let maybeIntervals = M.mapWithKey (\reg int2 -> mergeIntervals (intervals1 M.! reg) int2) intervals2 in
            if M.foldl maybeAnd True maybeIntervals 
              then Just $ (mapUnwrapMaybe maybeIntervals, flips1)
              else Nothing
          else Nothing
        else Nothing
      where
        keys1 = M.keys intervals1
        keys2 = M.keys intervals2 
    
        mergeIntervals :: IntervalLattice -> IntervalLattice -> Maybe IntervalLattice
        mergeIntervals (IntLat (Interval min1 max1)) (IntLat (Interval min2 max2)) = 
          if (toInteger l2) <= ((toInteger r1) + 1)
            then if r2 <= r1 
              then Just $ (IntLat $ Interval l1 r1) -- [l2..r2] is inside [l1..r1]
              else Just $ (IntLat $ Interval l1 r2) -- [l2..r2] extends [l1..r1]
            else Nothing
          where
            (l1, r1, l2, r2) = if min1 <= min2 then (min1, max1, min2, max2) else (min2, max2, min1, max1) 

        maybeAnd :: Bool -> Maybe a -> Bool
        maybeAnd b Nothing = False
        maybeAnd b (Just _) = b 
    
        mapUnwrapMaybe :: M.Map a (Maybe b) -> M.Map a b
        mapUnwrapMaybe m = M.map (\maybe -> case maybe of Just i -> i) m

