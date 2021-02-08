module Language.TinyARM.Analysis.Types where

import Data.Lattice
import Language.TinyARM.Language
import Language.TinyARM.Common
import Data.Strings

import Debug.Trace
import Data.Word
import Data.Bits
import qualified Data.List as L 
import qualified Data.Set as S 

import Prelude hiding (EQ,LT,GT)

--- Bits ---
data BitLattice 
  = BitTop
  | BitTrue
  | BitFalse
  | BitBottom
  deriving (Eq, Ord)

instance Show BitLattice where
  show BitTop = "⊤"
  show BitBottom = "⊥"
  show BitTrue = "1"
  show BitFalse = "0"

instance Lattice BitLattice where
  lbot = BitBottom
  ltop = BitTop
  ljoin BitTop _ = BitTop
  ljoin _ BitTop = BitTop
  ljoin BitBottom x = x
  ljoin x BitBottom = x
  ljoin a b = if a == b then a else ltop

  lmeet BitTop x = x
  lmeet x BitTop = x
  lmeet BitBottom _ = BitBottom
  lmeet _ BitBottom = BitBottom
  lmeet a b = if a == b then a else lbot
------------


--- Flags ---

--                     N            Z          C          V
data FlagsLattice
    = FlagsLattice BitLattice BitLattice BitLattice BitLattice
    deriving (Eq, Ord)

instance Lattice FlagsLattice where
  lbot = FlagsLattice BitBottom BitBottom BitBottom BitBottom
  ltop = FlagsLattice BitTop BitTop BitTop BitTop
  ljoin (FlagsLattice n1 z1 c1 v1) (FlagsLattice n2 z2 c2 v2) = FlagsLattice (ljoin n1 n2) (ljoin z1 z2) (ljoin c1 c2) (ljoin v1 v2)
  lmeet (FlagsLattice n1 z1 c1 v1) (FlagsLattice n2 z2 c2 v2) = FlagsLattice (lmeet n1 n2) (lmeet z1 z2) (lmeet c1 c2) (lmeet v1 v2)

  
instance Show FlagsLattice where
  show (FlagsLattice n z c v) = show (n,z,c,v)
  -- show x = show (trueConditions x)

trueConditions :: FlagsLattice -> [Condition]
trueConditions flags = filter (\cond -> evalCond cond flags) [EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE]



evalCond :: Condition -> FlagsLattice -> Bool
evalCond c f = _evalCond set clr c f
  where
    set :: BitLattice -> Bool
    set BitTrue = True
    set BitTop  = True
    set _ = False
    clr :: BitLattice -> Bool
    clr BitFalse = True
    clr BitTop   = True
    clr _ = False

evalCondStrict :: Condition -> FlagsLattice -> Bool
evalCondStrict c f = _evalCond set clr c f
  where
    set :: BitLattice -> Bool
    set BitTrue = True
    set _ = False
    clr :: BitLattice -> Bool
    clr BitFalse = True
    clr _ = False

--           N  Z  C  V
_evalCond :: (BitLattice -> Bool) -> (BitLattice -> Bool) -> Condition -> FlagsLattice -> Bool
_evalCond set clr EQ (FlagsLattice n z c v) = set z
_evalCond set clr NE (FlagsLattice n z c v) = clr z
_evalCond set clr CS (FlagsLattice n z c v) = set c
_evalCond set clr CC (FlagsLattice n z c v) = clr c
_evalCond set clr MI (FlagsLattice n z c v) = set n
_evalCond set clr PL (FlagsLattice n z c v) = clr n
_evalCond set clr VS (FlagsLattice n z c v) = set v
_evalCond set clr VC (FlagsLattice n z c v) = clr v
_evalCond set clr HI (FlagsLattice n z c v) = clr z && set c --
_evalCond set clr LS (FlagsLattice n z c v) = clr c || set z
_evalCond set clr GE (FlagsLattice n z c v) = (set n && set v) || (clr n && clr v)
_evalCond set clr LT (FlagsLattice n z c v) = (set n && clr v) || (clr n && set v)
_evalCond set clr GT (FlagsLattice n z c v) = (set n && clr z && set v) || (clr n && clr z && clr v)
_evalCond set clr LE (FlagsLattice n z c v) = set z || (set n && clr v) || (clr n && set v)
_evalCond set clr AL _ = True
_evalCond set clr NV _ = False





-- matchesCondition  :: FlagsLattice -> Condition -> Bool
-- matchesCondition (FlagsLattice n z c v) cond = 

-- matchesConditions :: FlagsLattice -> [Condition] -> Bool

negCond :: Condition -> Condition
negCond EQ = NE
negCond NE = EQ

negCond CS = CC
negCond CC = CS

negCond MI = PL
negCond PL = MI

negCond VS = VC
negCond VC = VS

negCond HI = LS
negCond LS = HI

negCond GE = LT
negCond LT = GE

negCond GT = LE
negCond LE = GT

negCond AL = NV
negCond NV = AL

-------------

--- intervals ---
data Interval 
  = Interval Value Value 
  deriving (Eq,Ord)

instance Show Interval where
  show (Interval a b) = "["++(strPadLeft ' ' maxLen $ show a)++".."++(strPadLeft ' ' maxLen $ show b)++"]"
    where maxLen = length $ show (maxBound::Value)

data IntervalLattice 
  = Bottom
  | IntLat Interval
  deriving (Eq,Ord)



instance Lattice Interval where
  lbot = error "No bottom exists in intervals"
  ltop = Interval 0 (maxBound::Value)
  ljoin (Interval min1 max1) (Interval min2 max2) = Interval (min min1 min2) (max max1 max2)
  lmeet (Interval min1 max1) (Interval min2 max2) = 
    if max1 >= min2 || max2 >= min1 
      then Interval (max min1 min2) (min max1 max2)
      else lbot
  
instance Show IntervalLattice where
  show (IntLat i) = show i
  show Bottom = "⊥"

instance Lattice IntervalLattice where
  lbot = Bottom
  ltop = IntLat ltop
  ljoin Bottom a = a
  ljoin a Bottom = a
  ljoin (IntLat i1) (IntLat i2) = IntLat $ ljoin i1 i2
  lmeet Bottom a = Bottom
  lmeet a Bottom = Bottom
  lmeet (IntLat i1@(Interval min1 max1)) (IntLat i2@(Interval min2 max2)) = 
    if max1 >= min2 || max2 >= min1 
      then IntLat $ lmeet i1 i2
      else Bottom

wrapIntLatBinFunc :: (Interval -> Interval -> Interval) -> IntervalLattice -> IntervalLattice -> IntervalLattice
wrapIntLatBinFunc f (IntLat a) (IntLat b) = IntLat $ f a b
wrapIntLatBinFunc _ _ _ = Bottom

wrapIntLatValBinFunc :: (Interval -> Value -> Interval) -> IntervalLattice -> Value -> IntervalLattice
wrapIntLatValBinFunc f (IntLat a) v = IntLat $ f a v
wrapIntLatValBinFunc _ _ _ = Bottom

intervalIntersection :: Interval -> Interval -> Maybe Interval
intervalIntersection (Interval l1 r1) (Interval l2 r2) = 
  checkInterval (Interval (max l1 l2) (min r1 r2))

intervalAddConst :: Interval -> Value -> Interval
intervalAddConst i v = intervalAdd i (Interval v v)

intervalSubConst :: Interval -> Value -> Interval
intervalSubConst i v = intervalSub i (Interval v v)

intervalSubConstBounded :: Interval -> Value -> Interval
intervalSubConstBounded (Interval l r) v = 
  (Interval (if l < v then 0 else l-v) (if r < v then 0 else r-v))
intervalAddConstBounded :: Interval -> Value -> Interval
intervalAddConstBounded (Interval l r) v = 
  (Interval (if maxU-l < v then maxU else l+v) (if maxU-r < v then maxU else r+v))


intervalAdd :: Interval -> Interval -> Interval
intervalAdd (Interval l1 r1) (Interval l2 r2) = 
  let 
    il1 = toInteger l1
    il2 = toInteger l2
    ir1 = toInteger r1
    ir2 = toInteger r2
    upperbound = toInteger (maxBound::Value)
    lowerbound = toInteger (minBound::Value)
  in
    if il1+il2 <= upperbound && ir1+ir2 > upperbound
      then Interval (minBound::Value) (maxBound::Value)
      else Interval (l1+l2) (r1+r2)


intervalSub :: Interval -> Interval -> Interval
intervalSub (Interval l1 r1) (Interval l2 r2) = 
  let 
    il1 = toInteger l1
    il2 = toInteger l2
    ir1 = toInteger r1
    ir2 = toInteger r2
    upperbound = toInteger (maxBound::Value)
    lowerbound = toInteger (minBound::Value)
  in
    if il1-ir2 < lowerbound && ir1-il2 >= lowerbound
      then Interval (minBound::Value) (maxBound::Value)
      else Interval (l1-r2) (r1-l2)

inInterval :: Value -> Interval -> Bool
inInterval v (Interval l r) = v >= l && v <= r

      

-----------------




-------- Interval functions ----------


checkInterval :: Interval -> Maybe Interval
checkInterval i@(Interval l r) = if l <= r then Just i else Nothing

evalFlagAdds :: Value -> Value -> FlagsLattice
evalFlagAdds val2 val1 = FlagsLattice flagN flagZ flagC flagV
  where 
    wordSize = finiteBitSize val1
    resultWord = val1 + val2
    resultInt = (toInteger val1) + (toInteger val2)
    msb1 = testBit val1 (wordSize - 1)
    msb2 = testBit val2 (wordSize - 1)
    msbR = testBit resultWord (wordSize - 1)
    flagN = if msbR                         then BitTrue else BitFalse
    flagZ = if resultWord == 0              then BitTrue else BitFalse
    flagC = if testBit resultInt wordSize   then BitTrue else BitFalse
    flagV = if msb1 == msb2 && msb1 /= msbR then BitTrue else BitFalse


evalFlagSubs :: Value -> Value -> FlagsLattice
evalFlagSubs val1 val2 = FlagsLattice flagN flagZ flagC flagV
  where
    wordSize = finiteBitSize val1
    resultWord = val1 + ((complement val2) + 1)
    resultInt = (toInteger val1) - (toInteger val2)
    msb1 = testBit val1 (wordSize - 1)
    msb2 = testBit val2 (wordSize - 1)
    msbR = testBit resultWord (wordSize - 1) 
    flagN = if msbR                         then BitTrue else BitFalse
    flagZ = if resultWord == 0              then BitTrue else BitFalse
    flagC = if resultInt < 0                then BitFalse else BitTrue
    flagV = if msb1 /= msb2 && msb1 /= msbR then BitTrue else BitFalse      

getIntervalsAddsBruteForce :: Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]
getIntervalsAddsBruteForce (Interval l1 r1) (Interval l2 r2) = 
  combineValueFlagMap $ map (\(lVal, rVal) -> (evalFlagAdds lVal rVal, (lVal, rVal))) [ (lVal, rVal) | lVal <- [l1..r1], rVal <- [l2..r2] ]

getIntervalsAdds :: Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]
getIntervalsAdds (Interval l1 r1) (Interval l2 r2) = 
  let

    al1 = max (maxS+2) (maxS-r2+1) -- no bounds check is by design
    ar1 = r1
    al2 = max (maxS+2) (maxS-r1+1) -- no bounds check is by design
    ar2 = r2 

    -- l1 >= maxS-r2+1      -- r1 <= maxS
    -- l2 >= maxS-r1+1      -- r2 <= maxS
    bl1 = if r2 > maxS then 1 else maxS-r2+1
    br1 = maxS
    bl2 = if r1 > maxS then 1 else maxS-r1+1
    br2 = maxS

    cl1 = if r2 > maxS then l1 else maxS+1
    cr1 = maxU-l2
    cl2 = if r1 > maxS then l2 else maxS+1
    cr2 = maxU-l1

    dl1 = maxS+1
    dr1 = maxS+1
    dl2 = maxS+1
    dr2 = maxS+1

    el1 = if r2 == 0    then 1    else maxU-r2+1
    er1 = if l2 == 0    then maxU else maxU-l2+1
    el2 = if r1 == 0    then 1    else maxU-r1+1
    er2 = if l1 == 0    then maxU else maxU-l1+1

    fl1 = 0
    fr1 = 0
    fl2 = 0
    fr2 = 0
    

    -- is not allowed to be the singular case i1 = [128..128] i2 = [128..128]
    -- l1 >= maxS+1       -- r1 <= maxS-l2
    -- l2 >= maxS+1       -- r2 <= maxS-l1
    -- ((0,0,1,1),([128..255],[128..255]))

    gl1 = maxS+1
    gr1 = if l2 >= maxS+1 then maxS-l2 else maxU
    gl2 = maxS+1
    gr2 = if l1 >= maxS+1 then maxS-l1 else maxU



    -- l1 >= maxU-r2+2        -- r1 <= 
    -- l2 >= maxU-r1+2        -- r2 <= 
    hl1'  = max l1 $ if r2 < 2 then maxU else maxU-r2+2 -- the general pattern
    hl1'' = if hl1' >= maxS+1 && hl1' <= maxS+2 then maxS+3 else hl1' -- move cases around [128..129]
    hr1' = min r1 $ if r1   >= maxS+1 && r1   <= maxS+2 then maxS   else r1   -- move cases around [128..129]
    hl2'  = max l2 $ if r1 < 2 then maxU else maxU-r1+2 -- the general pattern
    hl2'' = if hl2' >= maxS+1 && hl2' <= maxS+2 then maxS+3 else hl2'
    hr2' = min r2 $ if r2   >= maxS+1 && r2   <= maxS+2 then maxS else r2
    -- case i2 is entirely > maxS  -- i1 should be entirely <= maxS
    hr1 = if hl2'' >  maxS then min hr1' maxS else hr1'
    -- case i2 is entirely <= maxS -- i1 should be entirely > maxS
    hl1 = if hr2'  <= maxS then max hl1'' (maxS+1) else hl1''
    -- case i1 is entirely > maxS  -- i2 should be entirely <= maxS
    hr2 = if hl1'' >  maxS then min hr2' maxS else hr2'
    -- case i1 is entirely <= maxS -- i2 should be entirely > maxS
    hl2 = if hr1'  <= maxS then max hl2'' (maxS+1) else hl2''
    


    il1 = if l1 == 0 && l2 == 0 && r2 == 0 then 1 else l1
    ir1 = min maxS $ maxS-l2
    il2 = if l2 == 0 && l1 == 0 && r1 == 0 then 1 else l2
    ir2 = min maxS $ maxS-l1
    -- il1 = if l1 == l2 && l2 == r2 then l1+1 else l1
    -- ir1 = if r1 == r2 && l2 == r2 then min (r1-1) (maxS-l2) else maxS-l2
    -- il2 = if l2 == l1 && l1 == r1 then l2+1 else l2
    -- ir2 = if r2 == r1 && l1 == r1 then min (r2-1) (maxS-l1) else maxS-l1



    -- l1 >= maxS+2 >= (maxS-r2)+1 -- r1 == r1
    -- l2 >= maxS+2 >= (maxS-r1)+1 -- r2 == r2
    -- ((1,0,1,0),([129..255],[129..255])),
    a =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 al1) (min r1 ar1)), checkInterval (Interval (max l2 al2) (min r2 ar2) )))
    
    
    -- l1 >= maxS-r2+1      -- r1 <= maxS
    -- l2 >= maxS-r1+1      -- r2 <= maxS
    -- ((1,0,0,1),([  1..127],[  1..127]))
    b =    ((FlagsLattice BitTrue  BitFalse BitFalse BitTrue ), (checkInterval (Interval (max l1 bl1) (min r1 br1)), checkInterval (Interval (max l2 bl2) (min r2 br2) )))
    
    -- if r1 > maxS && r2 > maxS
    --  l1 >=                 -- r1 <= maxU-l2
    --  l2 >=                 -- r2 <= maxU-l1
    -- if r1 <= maxS
      -- l2 >= maxS+1
    -- if r2 <= maxS
      -- l1 >= maxS+1
    -- ((1,0,0,0),([  0..255],[  0..255]))
    c =    ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval (max l1 cl1) (min r1 cr1)), checkInterval (Interval (max l2 cl2) (min r2 cr2) )))
    
    -- l1 >= maxS+1        -- r1 <= maxS+1
    -- l2 >= maxS+1        -- r2 <= maxS+1
    -- ((0,1,1,1),([128..128],[128..128]))
    d =    ((FlagsLattice BitFalse BitTrue  BitTrue  BitTrue ), (checkInterval (Interval (max l1 dl1) (min r1 dr1)), checkInterval (Interval (max l2 dl2) (min r2 dr2) )))
    
    -- is not allowed to be the singular case i1 = [128..128] i2 = [128..128]
    -- l1 >= maxU+1-r2    -- r1 <= maxU+1-l2
    -- l2 >= maxU+1-r1    -- r2 <= maxU+1-l1
    -- ((0,1,1,0),([  1..255],[  1..255]))
    e'=    ((FlagsLattice BitFalse BitTrue  BitTrue  BitFalse), (checkInterval (Interval (max l1 el1) (min r1 er1)), checkInterval (Interval (max l2 el2) (min r2 er2) )))
    e = case e' of
      orig@(f, ( Just (Interval l1 r1), Just (Interval l2 r2) ) ) -> 
        if r1 == (maxS+1) && l2 == (maxS+1) -- if [l1..128] [128..r2]
          then if l1 == (maxS+1) && r2 == (maxS+1) -- if [128..128] [128..128]
            then (f, (Nothing, Nothing)) -- Is taken by ((0,1,1,1),([128..128],[128..128]))
            else (f, (checkInterval (Interval l1 (r1-1)), checkInterval (Interval (l2+1) r2))) -- try removing the offending case
          else if l1 == (maxS+1) && r2 == (maxS+1) -- if [128..r1] [l2..128]
            then (f, (checkInterval (Interval (l1+1) r1), checkInterval (Interval l2 (r2-1)))) -- try removing the offending case 
            else orig
      orig -> orig


    -- l1 >= 0  -- r1 <= 0
    -- l2 >= 0  -- r2 <= 0
    -- ((0,1,0,0),([  0..  0],[  0..  0]))
    f =    ((FlagsLattice BitFalse BitTrue  BitFalse BitFalse), (checkInterval (Interval (max l1 fl1) (min r1 fr1)), checkInterval (Interval (max l2 fl2) (min r2 fr2) )))
    
    -- is not allowed to be the singular case i1 = [128..128] i2 = [128..128]
    -- l1 >= maxS+1       -- r1 <= maxS-l2
    -- l2 >= maxS+1       -- r2 <= maxS-l1
    -- ((0,0,1,1),([128..255],[128..255]))
    g'=    ((FlagsLattice BitFalse BitFalse BitTrue  BitTrue ), (checkInterval (Interval (max l1 gl1) (min r1 gr1)), checkInterval (Interval (max l2 gl2) (min r2 gr2) )))
    g = case g' of
      orig@(f, ( Just (Interval l1 r1), Just (Interval l2 r2) ) ) -> 
        if r1 == (maxS+1) && l2 == (maxS+1) -- if [l1..128] [128..r2]
          then if l1 == (maxS+1) && r2 == (maxS+1) -- if [128..128] [128..128]
            then (f, (Nothing, Nothing)) -- Is taken by ((0,1,1,1),([128..128],[128..128]))
            else (f, (checkInterval (Interval l1 r1), checkInterval (Interval (l2+1) r2))) -- try removing the offending case
          else if l1 == (maxS+1) && r2 == (maxS+1) -- if [128..r1] [l2..128]
            then (f, (checkInterval (Interval (l1+1) r1), checkInterval (Interval l2 r2))) -- try removing the offending case 
            else orig
      orig -> orig


    -- skip cases where edges are 128 or 129
    -- l1 >= maxU-r2+2        -- r1 <= 
    -- l2 >= maxU-r1+2        -- r2 <= 
    -- ((0,0,1,0),([  2..255],[  2..255]))
    h =    ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 hl1) (min r1 hr1)), checkInterval (Interval (max l2 hl2) (min r2 hr2) )))

    -- if i1 or i2 is a singleton interval [0..0] exclude it from the other
    -- l1 >=                  -- r1 <= maxS-l2
    -- l2 >=                  -- r2 <= maxS-l1
    -- ((0,0,0,0),([  0..127],[  0..127]))
    i =    ((FlagsLattice BitFalse BitFalse BitFalse BitFalse), (checkInterval (Interval (max l1 il1) (min r1 ir1)), checkInterval (Interval (max l2 il2) (min r2 ir2) )))



  in
    [(f,(extractJustContent l, extractJustContent r)) | (f,(l,r)) <- [a,b,c,d,e,f,g,h,i] , l /= Nothing, r /= Nothing]


getIntervalsSubsBruteForce :: Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]
getIntervalsSubsBruteForce i1@(Interval l1 r1) i2@(Interval l2 r2) = 
  combineValueFlagMap $ map (\(lVal, rVal) -> (evalFlagSubs lVal rVal, (lVal, rVal))) [ (lVal, rVal) | lVal <- [l1..r1], rVal <- [l2..r2] ]

getIntervalsSubs :: Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]
getIntervalsSubs i1@(Interval l1 r1) i2@(Interval l2 r2) = 
  let
    -- handle overflow of calculations
    al1 = if l2 >= maxS  then maxU else l2+maxS+1
    ar1 = r1
    al2 = l2
    ar2 = if r1 <= maxS  then 0    else r1-maxS-1
    bl1 = if l2 <= maxS  then 0    else l2-maxS-1
    br1 = maxS
    bl2 = maxS+1
    br2 = if r1 >= maxS  then maxU else r1+maxS+1
    cr1 = if r2 >= 1     then r2-1 else r2
    cl2 = if l1 == maxU  then maxU else l1+1
    er1 = if r2 >  maxS  then maxU-1 else r2+maxS 
    el2 = if l1 <= maxS  then 1    else l1-maxS
    fl1 = if l2 == maxU  then maxU else l2+1
    fr2 = if r1 >= 1     then r1-1 else r1
    gl1 = l1
    gr1 = if r2 < maxS+2 then 0    else r2-maxS-2
    gl2 = if l1 >= maxS  then maxU else l1+maxS+2 
    gr2 = r2



    -- l1 >= l2+maxS+1    r1 = r1
    -- l2 = l2            r2 <= r1-(maxS+1) 
    -- 1010 - NE,CS,MI,VC,HI,LT,LE
    a =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 al1) (min r1 ar1)), checkInterval (Interval (max l2 al2) (min r2 ar2) )))
    
    -- l1 >= l2-(maxS+1)       r1 <= maxS
    -- l2 >= maxS+1  r2 <= r1+128
    -- 1001 - NE,CC,MI,VS,LS,GE,GT
    b =    ((FlagsLattice BitTrue  BitFalse BitFalse BitTrue ), (checkInterval (Interval (max l1 bl1) (min r1 br1)), checkInterval (Interval (max l2 bl2) (min r2 br2) )))

    -- if r1 <= maxS || r2 <= maxS+1
      -- l1 = l1      r1 <= maxS-1
      -- l2 >= l1+1   r2 <= maxS
    -- if l2 > maxS
      -- l1 >= maxS+1      r1 <= r2-1
      -- l2 >= calc(l1)+1  r2 = r2
    -- else
      -- l1 = l1      r1 <= r2-1
      -- l2 >= l1+1   r2 = r2
    -- 1000 - NE,CC,MI,VC,LS,LT,LE
    c = if r1 <= maxS || r2 <= maxS+1
      then ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval l1                   (min r1 (min cr1 (maxS-1)))       ), checkInterval (Interval (max l2 cl2)         (min r2 maxS)          )))
      else if l2 > maxS || l1 >= maxS
      then ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval (max l1 (maxS+1))    (min r1 cr1)                      ), checkInterval (Interval (max l2 (max cl2 (maxS+2)))  r2                     )))
      else ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval l1                   (min r1 cr1)                      ), checkInterval (Interval (max l2 cl2)         r2                     )))
    
    -- l1 >= l2   r1 <= r2
    -- l2 >= l1   r2 <= r1
    -- 0110 - EQ,CS,PL,VC,LS,GE,LE
    d =    ((FlagsLattice BitFalse BitTrue  BitTrue  BitFalse), (checkInterval (Interval (max l1 l2)          (min r1 r2)                       ), checkInterval (Interval (max l1 l2)          (min r1 r2)            )))
    
    -- l1 >= maxS+1   r1 <= r2+maxS
    -- l2 >= l1-maxS  r2 <= maxS
    -- 0011 - NE,CS,PL,VS,HI,LT,LE
    e =    ((FlagsLattice BitFalse BitFalse BitTrue  BitTrue ), (checkInterval (Interval (max l1 (maxS+1))    (min r1 er1)                      ), checkInterval (Interval (max l2 el2)           (min r2 maxS)          )))
     

    -- if r1 <= maxS+1 || r2 <= maxS
      -- l1 >= l2+1   r1 <= maxS
      -- l2 == l2     r2 <= maxS-1
    -- if l1 > maxS || l2 >= maxS
      -- l1 >= calc(l2)+1  r1 = r1
      -- l2 >= maxS+1      r2 <= r1-1
    -- else
      -- l1 >= l2+1   r1 == r1
      -- l2 == l2     r2 <= r1-1
    -- 0010 - NE,CS,PL,VC,HI,GE,GT
    f = if r2 <= maxS || r1 <= maxS+1
      then ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 fl1)         (min r1 maxS)                     ), checkInterval (Interval l2                   (min r2 (min fr2 (maxS-1))) )))
      else if l1 > maxS || l2 >= maxS
      then ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 (max fl1 (maxS+2))) r1                         ), checkInterval (Interval (max l2 (maxS+1))    (min r2 fr2)           )))
      else ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 fl1)         r1                                ), checkInterval (Interval l2                   (min r2 fr2)           )))
    
    -- 0000 - NE,CC,PL,VC,LS,GE,GT
    -- l1 == l1         r1 <= r2-(maxS+2)
    -- l2 >= l1+maxS+2  r2 == r2
    g =    ((FlagsLattice BitFalse BitFalse BitFalse BitFalse), (checkInterval (Interval (max l1 gl1) (min r1 gr1) ), checkInterval (Interval (max l2 gl2) (min r2 gr2))))
    
  in
    [(flgs,(extractJustContent l, extractJustContent r)) | (flgs,(l,r)) <- [a,b,c,d,e,f,g] , l /= Nothing, r /= Nothing]
{-- 

getIntervalsSubs :: Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]
getIntervalsSubs i1@(Interval l1 r1) i2@(Interval l2 r2) = 
  let
    al1 = if l2 >= maxS  then maxU else l2+maxS+1
    ar1 = r1
    al2 = l2
    ar2 = if r1 <= maxS  then 0    else r1-maxS-1

    bl1 = if l2 <= maxS  then 0    else l2-maxS-1
    br1 = maxS
    bl2 = maxS+1
    br2 = if r1 >= maxS  then maxU else r1+maxS+1
    
    cl1 = l1
    cr1 = if r2 >= 1     then r2-1 else r2
    cl2 = if l1 == maxU  then maxU else l1+1
    cr2 = r2

    dl1 = l2
    dr1 = r2
    dl2 = l1
    dr2 = r1

    el1 = maxS+1
    er1 = if r2 >  maxS  then maxU-1 else r2+maxS 
    el2 = if l1 <= maxS  then 1    else l1-maxS
    er2 = maxS

    fl1 = if l2 == maxU  then maxU else l2+1
    fr1 = r1
    fl2 = l2
    fr2 = if r1 >= 1     then r1-1 else r1
    
    gl1 = l1
    gr1 = if r2 < maxS+2 then 0    else r2-maxS-2
    gl2 = if l1 >= maxS  then maxU else l1+maxS+2 
    gr2 = r2

    a =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 al1) (min r1 ar1)), checkInterval (Interval (max l2 al2) (min r2 ar2) )))
    b =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 bl1) (min r1 br1)), checkInterval (Interval (max l2 bl2) (min r2 br2) )))
    c = if r1 <= maxS || r2 <= maxS+1
      then ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval l1                   (min r1 (min cr1 (maxS-1)))       ), checkInterval (Interval (max l2 cl2)         (min r2 maxS)          )))
      else if l2 > maxS || l1 >= maxS
      then ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval (max l1 (maxS+1))    (min r1 cr1)                      ), checkInterval (Interval (max l2 (max cl2 (maxS+2)))  r2                     )))
      else ((FlagsLattice BitTrue  BitFalse BitFalse BitFalse), (checkInterval (Interval l1                   (min r1 cr1)                      ), checkInterval (Interval (max l2 cl2)         r2                     )))
    d =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 dl1) (min r1 dr1)), checkInterval (Interval (max l2 dl2) (min r2 dr2) )))
    e =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 el1) (min r1 er1)), checkInterval (Interval (max l2 el2) (min r2 er2) )))
    f = if r2 <= maxS || r1 <= maxS+1
      then ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 fl1)         (min r1 maxS)                     ), checkInterval (Interval l2                   (min r2 (min fr2 (maxS-1))) )))
      else if l1 > maxS || l2 >= maxS
      then ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 (max fl1 (maxS+2))) r1                         ), checkInterval (Interval (max l2 (maxS+1))    (min r2 fr2)           )))
      else ((FlagsLattice BitFalse BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 fl1)         r1                                ), checkInterval (Interval l2                   (min r2 fr2)           )))
    g =    ((FlagsLattice BitTrue  BitFalse BitTrue  BitFalse), (checkInterval (Interval (max l1 gl1) (min r1 gr1)), checkInterval (Interval (max l2 gl2) (min r2 gr2) )))
    
  in
    [(flgs,(extractJustContent l, extractJustContent r)) | (flgs,(l,r)) <- [a,b,c,d,e,f,g] , l /= Nothing, r /= Nothing]

--}
getIntervalsCmp :: Interval -> Interval -> [(FlagsLattice, (Interval, Interval))]
getIntervalsCmp = getIntervalsSubs --BruteForce

getIntervalsCmpFunction :: BinOp -> (Interval -> Interval -> [(FlagsLattice, (Interval, Interval))])
getIntervalsCmpFunction ADD = getIntervalsAdds
getIntervalsCmpFunction SUB = getIntervalsSubs






combineValueFlagMap :: [(FlagsLattice, (Value, Value))] -> [(FlagsLattice,(Interval,Interval))]
combineValueFlagMap lst = combine' (L.sort lst) []
  where
    combine' :: [(FlagsLattice, (Value, Value))] -> [(FlagsLattice,(Interval,Interval))] -> [(FlagsLattice,(Interval,Interval))]
    combine' [] lst = lst
    combine' ((flags, (lval, rval)):rest) [] = combine' rest [(flags, (Interval lval lval, Interval rval rval))]
    combine' ((flags, (lval, rval)):rest) (prev@(prevFlags,(prevLInterval, prevRInterval)):restResult) = combine' rest $
      if flags == prevFlags
        then (flags, (includeValInInterval lval prevLInterval, includeValInInterval rval prevRInterval)) : restResult
        else (flags, (Interval lval lval, Interval rval rval)) : (prev : restResult)
        
    includeValInInterval :: Value -> Interval -> Interval
    includeValInInterval newVal (Interval oldMin oldMax) = 
      if newVal <= oldMin 
        then Interval newVal oldMax
        else if newVal >= oldMax 
          then Interval oldMin newVal
          else Interval oldMin oldMax


getIntervalsAddsWithFlips :: Interval -> Interval -> [(FlagsLattice, ((Interval, [Value], [Value]), (Interval, [Value], [Value])))]
getIntervalsAddsWithFlips l@(Interval l1 r1) r@(Interval l2 r2) = 
  combineValueFlagMapWithFlips $ map (\(lVal, rVal) -> (evalFlagAdds lVal rVal, ((lVal, positiveFlips l lVal, negativeFlips l lVal), (rVal, positiveFlips r rVal, negativeFlips r rVal)))) [ (lVal, rVal) | lVal <- [l1..r1], rVal <- [l2..r2] ]

getIntervalsSubsWithFlips :: Interval -> Interval -> [(FlagsLattice, ((Interval, [Value], [Value]), (Interval, [Value], [Value])))]
getIntervalsSubsWithFlips l@(Interval l1 r1) r@(Interval l2 r2) = 
  combineValueFlagMapWithFlips $ 
    map (
      \(lVal, rVal) -> 
        (evalFlagSubs lVal rVal, 
        (
          (
            lVal, 
            positiveFlips l lVal, 
            negativeFlips l lVal
          ), 
          (
            rVal,
            positiveFlips r rVal, 
            negativeFlips r rVal
          )
        )
        )
      ) 
      [ (lVal, rVal) | lVal <- [l1..r1], rVal <- [l2..r2] ]

getIntervalsCmpWithFlips :: Interval -> Interval -> [(FlagsLattice, ((Interval, [Value], [Value]), (Interval, [Value], [Value])))]
getIntervalsCmpWithFlips = getIntervalsSubsWithFlips


combineValueFlagMapWithFlips :: 
  [(FlagsLattice, ((Value,    [Value], [Value]), (Value,    [Value], [Value])))] -> 
  [(FlagsLattice, ((Interval, [Value], [Value]), (Interval, [Value], [Value])))]
combineValueFlagMapWithFlips lst = combine' (L.sort lst) []
  where
    combine' :: 
      [(FlagsLattice, ((Value,    [Value], [Value]), (Value,    [Value], [Value])))] -> 
      [(FlagsLattice, ((Interval, [Value], [Value]), (Interval, [Value], [Value])))] -> 
      [(FlagsLattice, ((Interval, [Value], [Value]), (Interval, [Value], [Value])))]
    combine' [] lst = lst
    combine' ((flags, ((lval, lfp, lfn), (rval, rfp, rfn))):rest) [] = 
      combine' rest [(flags, ((Interval lval lval, lfp, lfn), (Interval rval rval, rfp, rfn)))]
    combine' ((flags, ((lval, lfp, lfn), (rval, rfp, rfn))):rest) (prev@(prevFlags,((prevLInterval, prevlfp, prevlfn), (prevRInterval, prevrfp, prevrfn))):restResult) = 
      combine' rest $
        if flags == prevFlags
          then (flags, ((includeValInInterval lval prevLInterval, bitflipUnion lfp prevlfp, bitflipUnion lfn prevlfn), (includeValInInterval rval prevRInterval, bitflipUnion rfp prevrfp, bitflipUnion rfn prevrfn))) : restResult
          else (flags, ((Interval lval lval, lfp, lfn), (Interval rval rval, rfp, rfn))) : (prev : restResult)
    
    bitflipUnion :: [Value] -> [Value] -> [Value]
    bitflipUnion a b = S.toList $ S.union (S.fromList a) (S.fromList b)
        
    includeValInInterval :: Value -> Interval -> Interval
    includeValInInterval newVal (Interval oldMin oldMax) = 
      if newVal <= oldMin 
        then Interval newVal oldMax
        else if newVal >= oldMax 
          then Interval oldMin newVal
          else Interval oldMin oldMax
          


expandInterval :: Interval -> Interval
expandInterval (Interval min max) = Interval newMin newMax
  where
    msbIndex = (finiteBitSize max) - 1
    newMin = findNewMin min max (2^msbIndex)
    newMax = findNewMax min max (2^msbIndex)

    findNewMin :: Value -> Value -> Value -> Value
    findNewMin min max 0 = 0
    findNewMin min max powerOfTwo = 
      if min >= powerOfTwo
        then min - powerOfTwo
        else if max >= powerOfTwo
          then 0
          else findNewMin min max (div powerOfTwo 2)

    findNewMax :: Value -> Value -> Value -> Value
    findNewMax min max 0 = max
    findNewMax min max powerOfTwo = 
      if max < powerOfTwo
        then max + powerOfTwo
        else if min < powerOfTwo
          then (powerOfTwo * 2) - 1
          else powerOfTwo + findNewMax (min - powerOfTwo) (max - powerOfTwo) (div powerOfTwo 2)

getPossibleFlipsTestAll :: Bool
getPossibleFlipsTestAll = 
  foldl (&&) True [
    let res = (getPossibleFlipsBrute (Interval la ra) (Interval lb rb)) == (getPossibleFlipsOld (Interval la ra) (Interval lb rb)) 
    in if res == False then trace ("fails for "++(show ((Interval la ra),(Interval lb rb)))) res else res
    
    | la <- [0..maxU], ra <- [0..maxU], lb <- [0..maxU], rb <- [0..maxU], la <= ra, lb <= rb]

getPossibleFlipsOld :: Interval -> Interval -> ([Value], [Value])
getPossibleFlipsOld (Interval origMin origMax) (Interval expandedMin expandedMax) = (positiveFlips, negativeFlips)
  where
    msbIndex = fromIntegral $ (finiteBitSize origMax) - 1
    positiveFlips = filter (canBitBeFlippedPos origMin origMax expandedMin expandedMax) [0..msbIndex]
    negativeFlips = filter (canBitBeFlippedNeg origMin origMax expandedMin expandedMax) [0..msbIndex]

    canBitBeFlippedPos :: Value -> Value -> Value -> Value -> Value -> Bool
    canBitBeFlippedPos origMin origMax expMin expMax bit = isValInInterval && isFlipInInterval && isFlipNoOverFlow && isSkipNoOverflow
      where 
        power = 2^bit 
        skipLength = (power - (origMin .&. (power - 1))) 
        nextValue = origMin + skipLength
        valueToFlip = if testBit origMin (fromIntegral bit) 
          then nextValue
          else origMin 
        
        isValInInterval  = valueToFlip <= origMax
        isFlipInInterval = valueToFlip + power <= expMax && valueToFlip + power >= expMin
        isFlipNoOverFlow = valueToFlip + power > valueToFlip
        isSkipNoOverflow = valueToFlip >= origMin

    canBitBeFlippedNeg :: Value -> Value -> Value -> Value -> Value -> Bool
    canBitBeFlippedNeg origMin origMax expMin expMax bit = isValInInterval && isFlipInInterval && isFlipNoOverFlow && isSkipNoOverflow
      where 
        power = 2^bit 
        skipLength = (origMax .&. (power - 1)) + 1 
        prevValue = origMax - skipLength
        valueToFlip = if testBit origMax (fromIntegral bit) 
          then origMax
          else prevValue 
        
        isValInInterval  = valueToFlip >= origMin
        isFlipInInterval = valueToFlip - power <= expMax && valueToFlip - power >= expMin
        isFlipNoOverFlow = valueToFlip - power < valueToFlip
        isSkipNoOverflow = valueToFlip <= origMax



getPossibleFlipsBrute :: Interval -> Interval -> ([Value], [Value])
getPossibleFlipsBrute a b = (getBitsToFlipPos a b, getBitsToFlipNeg a b)

getBitsToFlipPos :: Interval -> Interval -> [Value]
getBitsToFlipPos (Interval min max) intDst = S.toList $ S.fromList $ concat $ map (flip getBitsToFlipInPos intDst) [min..max]

getBitsToFlipNeg :: Interval -> Interval -> [Value]
getBitsToFlipNeg (Interval min max) intDst = S.toList $ S.fromList $ concat $ map (flip getBitsToFlipInNeg intDst) [min..max]

getBitsToFlipInPos :: Value -> Interval -> [Value]
getBitsToFlipInPos val int@(Interval min max) = 
  map (\(bitPos,_) -> bitPos) $ filter (\(_,val) -> insideInterval val int) (getFlippableValuesPos val)

getBitsToFlipInNeg :: Value -> Interval -> [Value]
getBitsToFlipInNeg val int@(Interval min max) = 
  map (\(bitPos,_) -> bitPos) $ filter (\(_,val) -> insideInterval val int) (getFlippableValuesNeg val)

insideInterval :: Value -> Interval -> Bool
insideInterval val (Interval min max) = val >= min && val <= max

getFlippableValuesPos :: Value -> [(Value, Value)]
getFlippableValuesPos val = filter (\(b,v) -> v > val) (getFlippableValues val)

getFlippableValuesNeg :: Value -> [(Value, Value)]
getFlippableValuesNeg val = filter (\(b,v) -> v < val) (getFlippableValues val)

-- getFlippableValues :: Value -> [(Value, Value)]
-- getFlippableValues val = getFlippableValues' val (fromIntegral $ (finiteBitSize val) - 1) []
--   where
--     getFlippableValues' :: Value -> Value -> [(Value, Value)] -> [(Value, Value)]
--     getFlippableValues' val 0 result = (0, complementBit val 0) : result
--     getFlippableValues' val bit result = getFlippableValues' val (bit - 1) ((bit, complementBit val (fromIntegral bit)) : result)

getPossibleFlips :: Interval -> Interval -> ([Value], [Value])
getPossibleFlips intervalFrom intervalTo = (positiveFlips, negativeFlips)
  where
    msbIndex = bitWidth - 1
    positiveFlips = filter (canBitBeFlippedPos intervalFrom intervalTo) [0..msbIndex]
    negativeFlips = filter (canBitBeFlippedNeg intervalFrom intervalTo) [0..msbIndex]


    canBitBeFlippedPos :: Interval -> Interval -> Value -> Bool
    canBitBeFlippedPos from@(Interval origMin origMax) to@(Interval expMin expMax) bit = 
      case (trimmedFrom, trimmedTo) of
        (Just tfrom, Just tto) -> 
          case intervalIntersection tfrom $ intervalSubConst tto power of
            Nothing -> False -- No overlap
            _ -> True -- some overlap
        _ -> False -- either interval couldnt be flipped from/to
      where
        power = 2^bit 
        trimmedFrom = trimIntervalToBitClear from bit
        trimmedTo   = trimIntervalToBitSet   to   bit


    canBitBeFlippedNeg :: Interval -> Interval -> Value -> Bool
    canBitBeFlippedNeg from@(Interval origMin origMax) to@(Interval expMin expMax) bit = 
      case (trimmedFrom, trimmedTo) of
        (Just tfrom, Just tto) -> 
          case intervalIntersection tfrom $ intervalAddConst tto power of
            Nothing -> False -- No overlap
            _ -> True -- some overlap
        _ -> False -- either interval couldnt be flipped from/to
      where 
        power = 2^bit 
        trimmedFrom = trimIntervalToBitSet   from bit
        trimmedTo   = trimIntervalToBitClear to   bit
    
    trimIntervalToBitClear :: Interval -> Value -> Maybe Interval
    trimIntervalToBitClear (Interval l r) bit = 
      let ltrim = nextValueWithBitClear l bit
          rtrim = prevValueWithBitClear r bit
      in if ltrim >= l && rtrim <= r then checkInterval (Interval ltrim rtrim) else Nothing
    trimIntervalToBitSet :: Interval -> Value -> Maybe Interval
    trimIntervalToBitSet (Interval l r) bit = 
      let ltrim = nextValueWithBitSet l bit
          rtrim = prevValueWithBitSet r bit
      in if ltrim >= l && rtrim <= r then checkInterval (Interval ltrim rtrim) else Nothing
      
    nextValueWithBitSet :: Value -> Value -> Value
    nextValueWithBitSet val bit = 
      let power = 2^bit
      in if (val .&. power) == power then val else val + power - (val .&. (power-1))

    nextValueWithBitClear :: Value -> Value -> Value
    nextValueWithBitClear val bit = 
      let power = 2^bit
      in if (val .&. power) == 0 then val else val + power - (val .&. (power-1))

    prevValueWithBitSet :: Value -> Value -> Value
    prevValueWithBitSet val bit = 
      let power = 2^bit
      in if (val .&. power) == power then val else val - (val .&. (power-1)) -1

    prevValueWithBitClear :: Value -> Value -> Value
    prevValueWithBitClear val bit = 
      let power = 2^bit
      in if (val .&. power) == 0 then val else val - (val .&. (power-1)) -1




positiveFlips :: Interval -> Value -> [Value]
positiveFlips (Interval min max) val = makeUnique $ concat $ map (getBitsToFlipPos val) [min..max]
  where
    getBitsToFlipPos :: Value -> Value -> [Value]
    getBitsToFlipPos dst src = 
      if dst <= src
        then []
        else map (\(bit, val) -> bit) $ filter (\(bit, val) -> val == dst) (getFlippableValues src)

negativeFlips :: Interval -> Value -> [Value]
negativeFlips (Interval min max) val = makeUnique $ concat $ map (getBitsToFlipNeg val) [min..max]
  where
    getBitsToFlipNeg :: Value -> Value -> [Value]
    getBitsToFlipNeg dst src = 
      if dst >= src 
        then []
        else map (\(bit, val) -> bit) $ filter (\(bit, val) -> val == dst) (getFlippableValues src)

getFlippableValues :: Value -> [(Value, Value)]
getFlippableValues val = getFlippableValues' val (fromIntegral $ (finiteBitSize val) - 1) []
  where
    getFlippableValues' :: Value -> Value -> [(Value, Value)] -> [(Value, Value)]
    getFlippableValues' val 0 result = (0, complementBit val 0) : result
    getFlippableValues' val bit result = getFlippableValues' val (bit - 1) ((bit, complementBit val (fromIntegral bit)) : result)

makeUnique :: Ord a => [a] -> [a]
makeUnique = S.toList . S.fromList

-- positiveFlips :: Interval -> Value -> [Value]
-- positiveFlips (Interval min max) val = getBitsToFlipPos (Interval min max) (Interval val val)

-- negativeFlips :: Interval -> Value -> [Value]
-- negativeFlips (Interval min max) val = getBitsToFlipNeg (Interval min max) (Interval val val)

-- getPositiveBitsToFlip :: Interval -> Value -> [Value]
-- getPositiveBitsToFlip int@(Interval min max) val = makeUnique $ concat $ map (getBitsToFlipInPos int) [min..max]
--   where
--     getBitsToFlipInPos :: Interval -> Value -> [Value]
--     getBitsToFlipInPos int@(Interval min max) val = 
--       map (\(bitPos,_) -> bitPos) $ filter (\(_,val) -> insideInterval val int) (getFlippableValuesPos val)

-- getBitsToFlipPos :: Interval -> Interval -> [Value]
-- getBitsToFlipPos (Interval min max) intDst = S.toList $ S.fromList $ concat $ map (flip getBitsToFlipInPos intDst) [min..max]

-- getBitsToFlipNeg :: Interval -> Interval -> [Value]
-- getBitsToFlipNeg (Interval min max) intDst = S.toList $ S.fromList $ concat $ map (flip getBitsToFlipInNeg intDst) [min..max]



-- getFlippableValuesPos :: Value -> [(Value, Value)]
-- getFlippableValuesPos val = filter (\(b,v) -> v > val) (getFlippableValues val)

-- getFlippableValuesNeg :: Value -> [(Value, Value)]
-- getFlippableValuesNeg val = filter (\(b,v) -> v < val) (getFlippableValues val)

-- getFlippableValues :: Value -> [(Value, Value)]
-- getFlippableValues val = getFlippableValues' val (fromIntegral $ (finiteBitSize val) - 1) []
--   where
--     getFlippableValues' :: Value -> Value -> [(Value, Value)] -> [(Value, Value)]
--     getFlippableValues' val 0 result = (0, complementBit val 0) : result
--     getFlippableValues' val bit result = getFlippableValues' val (bit - 1) ((bit, complementBit val (fromIntegral bit)) : result)
