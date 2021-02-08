{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TinyARM.Analysis.ValueNumbering where

import TinyARM.Analysis
import TinyARM.Language
import TinyARM.CFG
import TinyARM.Common

import Data.Lattice
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S



data Expression
    = Reg Register
    | Mem Expression
    | Imm Value
    | Add Int Int
    | Sub Int Int
    deriving (Eq,Ord)

instance Show Expression where
    show (Imm v) = "#"++(show v)
    show (Reg r) = show r
    show (Mem e) = "*("++(show e)++")"
    show (Add i1 i2) = (show i1) ++ "+" ++ (show i2)
    show (Sub i1 i2) = (show i1) ++ "-" ++ (show i2)

type ValueNumLatticeElem = (Int, M.Map Expression Int)

number :: Expression -> ValueNumLatticeElem -> ValueNumLatticeElem
number expr lat@(n,m) = case m M.!? expr of
    Nothing -> (n+1, M.insert expr n m)
    Just _ -> lat

latlookup :: ValueNumLatticeElem -> Expression -> Int
latlookup (n,m) expr = m M.! expr

pmToBinop :: PlusMinus -> BinOp
pmToBinop PLUS = ADD
pmToBinop MINUS = SUB

class Expressable a where
    express :: a -> ValueNumLatticeElem -> (Expression, ValueNumLatticeElem)

instance Expressable Register where
    express r lat = (Reg r, number (Reg r) lat)

instance Expressable Value where
    express v lat = (Imm v, number (Imm v) lat)

instance Expressable ValOrReg where
    express (IMMVAL v) = express v
    express (REGVAL r) = express r

instance Expressable (BinOp, Expression, Expression) where
    express (ADD, e1, e2) lat = 
        let n1 = latlookup lat e1
            n2 = latlookup lat e2
            expr = Mem $ Add (min n1 n2) (max n1 n2)
        in (expr, number expr lat)
    express (SUB, e1, e2) lat = 
        let n1 = latlookup lat e1
            n2 = latlookup lat e2
            expr = Mem $ Sub n1 n2
        in (expr, number expr lat)

instance Expressable AddrOrReg where
    express (IMMADDR v) lat = express v lat
    express (REGADDR r) lat = express r lat
    express (REGADDRIMMOFFSET r op v) lat = 
        let (a,lat')  = express r lat
            (b,lat'') = express v lat'
        in express (pmToBinop op, a, b) lat''
    express (REGADDRREGOFFSET r1 op r2) lat = 
        let (a,lat')  = express r1 lat
            (b,lat'') = express r2 lat'
        in express (pmToBinop op, a, b) lat''

-- instance Expressable AddrOrReg where
--     express (IMMADDR v) = express v
--     express (REGADDR r) = express r
--     express (REGADDRIMMOFFSET r PLUS v)    = Add (express r) (express v)
--     express (REGADDRIMMOFFSET r MINUS v)   = Sub (express r) (express v)
--     express (REGADDRREGOFFSET r1 PLUS r2)  = Add (express r1) (express r2)
--     express (REGADDRREGOFFSET r1 MINUS r2) = Sub (express r1) (express r2)
    



-- prettyPrintState :: ValueNumLatticeElem -> String
prettyPrintState m = printMultilineMap printMapping m
    where printMapping (expr, n) = (rightpad (show expr) ' ' 8)++" ↦ "++(show n)

-- valueNumberingAnalysis :: CFG -> (M.Map CFGKey ValueNumLatticeElem)
-- valueNumberingAnalysis :: CFG -> M.Map CFGKey (M.Map [Int] Expression)
valueNumberingAnalysis :: CFG -> M.Map CFGKey (M.Map Int [Expression])
valueNumberingAnalysis cfg = 
  let (outmap, inmap) = worklistAlgorithm cfg initialState initialState join transfer
  in mapmap (\(k,(n,m)) -> (k,invert m)) outmap    
  where
    initialState :: M.Map CFGKey ValueNumLatticeElem
    initialState = M.map (\_ -> bottom) cfg

    bottom :: ValueNumLatticeElem
    bottom = (0, M.empty)

    join :: [ValueNumLatticeElem] -> ValueNumLatticeElem
    join [] = bottom
    join [l] = l
    join ((na,ma):(nb,mb):xs) = error "undefined join functoin of value numbering analysis"
    
    assign :: Expression -> Expression -> ValueNumLatticeElem -> ValueNumLatticeElem
    assign lhs rhs lat@(n,m) = (n, M.insert lhs (m M.! rhs) m)

    transfer :: CFGKey -> CFGNode -> ValueNumLatticeElem -> ValueNumLatticeElem
    transfer _ (NDefault (IMOV _ _ r vr)) lat = 
        let (rhs, lat') = express vr lat
            lhs = Reg r
        in assign lhs rhs lat'
    transfer _ (NDefault (IBINOP _ _ op rdst rsrc1 vrsrc2)) lat = 
        let (src1, lat')   = express rsrc1 lat
            (src2, lat'')  = express vrsrc2 lat'
            (rhs, lat''')  = express (op, src1, src2) lat''
            lhs = Reg rdst
        in assign lhs rhs lat'''
        
    transfer _ (NDefault (ISTR _ rsrc addrdest)) lat =
        let (rhs, lat')  = express rsrc lat
            (lhs, lat'') = express addrdest lat'
        in assign lhs rhs lat''
        
    transfer _ (NDefault (ILDR _ rdst addrdest)) lat =
        let (rhs, lat') = express addrdest lat
            lhs = Reg rdst
        in assign lhs rhs lat'
    
    transfer _ (NDefault (ICMP _ _ _)) lat = lat
    transfer _ (NDefault (IB _ _)) lat = lat

    transfer _ (NDefault _) inElm = error "The anslysis does not support the instruction"
    transfer _ _ inElm = inElm

