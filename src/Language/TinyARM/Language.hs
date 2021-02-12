----------------------------------------------------------------------
-- 
-- TinyARM Assembly Language
--
----------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
module Language.TinyARM.Language where

import Common

import Prelude hiding (EQ,LT,GT)
import Data.Word
import Data.List
import Data.Char
import Data.Word.Odd
import Data.Bits
import qualified Data.Map as M

type Value = Word8
-- type Value = OddWord Word8 (One (One (One ())))
bitWidth :: Value
bitWidth = fromIntegral (finiteBitSize (0::Value))
maxS :: Value
maxS = 2^(bitWidth-1)-1
maxU :: Value
maxU = (maxBound::Value)
minU :: Value
minU = 0

type Address = Value
type Label = String

data Register
  = PC
  | SP
  | LR
  | RH Int
  | R Int
  deriving (Eq,Ord,Read)

instance Show Register where
  show PC = "PC"
  show SP = "sp"
  show LR = "lr"
  show (R i) = "r"++(show i)
  show (RH i) = "h"++(show i)

data Condition 
  = EQ    -- Z == 1
  | NE    -- Z == 0
  | CS    -- C == 1
  | CC    -- C == 0
  | MI    -- N == 1
  | PL    -- N == 0
  | VS    -- V == 1
  | VC    -- V == 0
  | HI    -- C == 1 && Z == 0
  | LS    -- C == 0 || Z == 1
  | GE    -- N == V
  | LT    -- N != V
  | GT    -- Z == 0 && N == V
  | LE    -- Z == 1 || N != V
  | NV    -- never
  | AL    -- always
  deriving (Eq,Ord,Read,Show)

data Flag 
  = FlagN 
  | FlagZ 
  | FlagC 
  | FlagV 
  deriving (Eq,Ord,Read,Show)

data SetFlags 
  = DoSet 
  | NoSet 
  deriving (Eq,Show)

data PlusMinus 
  = PLUS 
  | MINUS 
  deriving (Eq,Show)

data ValOrReg
  = IMMVAL Value
  | REGVAL Register
  deriving (Eq,Show)

data AddrOrReg
  = IMMADDR Address
  | REGADDR Register
  | REGADDRIMMOFFSET Register PlusMinus Value
  | REGADDRREGOFFSET Register PlusMinus Register
  deriving (Eq,Show)

data BranchTarget
  = DIRECT Address
  | RELATIVE PlusMinus Value
  | LABEL Label
  deriving (Eq,Show)

data BinOp 
  = ADD 
  | SUB 
  deriving (Eq,Show) 

data Instruction a
  = META a (Maybe (Instruction a))
  | IMOV SetFlags Condition Register ValOrReg
  | IBINOP SetFlags Condition BinOp Register Register ValOrReg
  | ICMP Condition Register ValOrReg
  | ILDR Condition Register AddrOrReg
  | ISTR Condition Register AddrOrReg
  | IB Condition BranchTarget
  | IPUSH Condition [Register]
  | IPOP Condition [Register]
  | IOUT Condition ValOrReg
  deriving (Eq, Show)

instance Functor Instruction where
  fmap f (META a (Just ins)) = META (f a) (Just (fmap f ins))
  fmap f (META a Nothing) = META (f a) Nothing

--
-- From here on, things pertain to the assembly language more than the core language itself
--

data SpecialInstructions 
  = HALT String 
  | COND Condition
  deriving (Eq,Ord,Read,Show)

type Instr = Instruction SpecialInstructions
type Code = M.Map Address Instr
type LabelMap = M.Map Label Address

data Program = Program 
  { code_ :: Code
  , labels_ :: LabelMap
  } deriving (Eq)

instance Show Program where
  show (Program { code_ = c, labels_ = l}) = 
    let
      lbls = M.toList $ mapmap (\(label, addr) -> (addr, "       "++label++":")) l
      codeLines = M.toList $ M.mapWithKey (\addr instr -> "["++(showHexWord addr)++"]   "++prettyPrintInstr instr) c
      addressedLines = sortOn (\(addr, str) -> addr) (lbls ++ codeLines)
      lines = map snd addressedLines
    in
      -- show addressedLines
      intercalate "\n" lines

emptyProgram :: Program
emptyProgram = Program
  { code_ = M.empty
  , labels_ = M.empty
  }

--
-- Helper functions
--

extractCondition :: Instr -> Condition
extractCondition (IMOV _ cond _ _) = cond
extractCondition (IBINOP _ cond _ _ _ _) = cond
extractCondition (ICMP cond _ _) = cond
extractCondition (ILDR cond _ _) = cond
extractCondition (ISTR cond _ _) = cond
extractCondition (IB cond _) = cond
extractCondition (IOUT cond _) = cond
extractCondition _ = AL

prettyPrintInstr :: Instruction SpecialInstructions -> String
prettyPrintInstr i = concat $ ppI i
  where
    ppI :: Instruction SpecialInstructions -> [String]
    ppI (META (HALT str) _ ) = [".HALT \"", str, "\""]
    ppI (META (COND c) _ ) = [ppC c]
    ppI (IMOV s c r rv) = ["mov", ppS s, ppC c, ppArgs [ppR r, ppRV rv]] 
    ppI (IBINOP s c op r1 r2 rv) = [ppOP op, ppS s, ppC c, ppArgs [ppR r1, ppR r2, ppRV rv]]
    ppI (ICMP c r rv) = ["cmp", ppC c, ppArgs [ppR r, ppRV rv]]
    ppI (ILDR c r ra) = ["ldr", ppC c, ppArgs [ppR r, ppRA ra]]
    ppI (ISTR c r ra) = ["str", ppC c, ppArgs [ppR r, ppRA ra]]
    ppI (IB c bt) =     ["b",   ppC c, ppArgs [ppBT bt]]
    ppI (IPUSH c rs) =  ["push", ppC c, ppRegLst rs]
    ppI (IPOP c rs)  =  ["pop",  ppC c, ppRegLst rs]
    ppI (IOUT c rv) =   ["out",  ppC c, ppArgs [ppRV rv]]

    ppOP ADD = "add"
    ppOP SUB = "sub"

    ppArgs args = (intercalate ", " args)
    
    ppRegLst rs = "{" ++ (intercalate ", " (map ppR rs)) ++ "}"

    ppPM PLUS = "+"
    ppPM MINUS = "-"

    ppS DoSet = "s"
    ppS NoSet = ""

    ppC AL = " "
    ppC c  = "_" ++ (map toLower $ show c) ++ " "
    
    -- ppR PC = "pc"
    -- ppR (R i) = "r"++(ppV i)
    ppR r = show r

    ppV v = show v
    ppImm imm = "#"++(ppV imm)

    ppBT (DIRECT a) = ppV a
    ppBT (RELATIVE pm a) = (ppPM pm) ++ (ppV a)
    ppBT (LABEL l) = l

    ppRV (IMMVAL i) = ppImm i
    ppRV (REGVAL r) = ppR r

    ppRA (IMMADDR a) = ppImm a
    ppRA (REGADDR r) = ppR r
    ppRA (REGADDRIMMOFFSET r PLUS v)    = concat ["[",ppR r, ", ", ppImm v,   "]"]
    ppRA (REGADDRIMMOFFSET r MINUS v)   = concat ["[",ppR r, ", ", ppImm (-v),"]"]
    ppRA (REGADDRREGOFFSET r1 PLUS r2)  = concat ["[",ppR r1,", ", ppR r2,    "]"]
    ppRA (REGADDRREGOFFSET r1 MINUS r2) = concat ["[",ppR r1,", -",ppR r2,    "]"]

