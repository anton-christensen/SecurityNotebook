----------------------------------------------------------------------
-- 
-- TinyARM Assembly Interpreter
--
----------------------------------------------------------------------
module Language.TinyARM.Interpreter where

import Prelude hiding (EQ,LT,GT)
import Data.Word
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (when,liftM,replicateM_,replicateM)
import qualified Control.Monad.State as S
import Control.Monad.State (gets,modify)
import Text.Printf
import qualified Data.ByteString.Lazy as B
import Data.List (intercalate,foldl',sort)
import Data.Foldable (asum)
import Data.Text (pack)

import Language.TinyARM.Language
import Language.TinyARM.Common

type Heap = M.Map Address Value
type Registers = M.Map Register Value
type Flags = M.Map Flag Bool
type Output = [String]

data InterpreterState = IState 
  { heap_ :: Heap
  , regs_ :: Registers
  , flags_ :: Flags
  , output_ :: Output
  , program_ :: Program
  } deriving (Eq)


instance Show InterpreterState where
  show (IState { heap_ = h, regs_ = r, flags_ = f, output_ = o, program_ = p}) = 
    "Interpreter state: \n"
      ++ "---------------\n"
      ++ "-- Program   --\n"
      ++ "---------------\n"
      ++ (show p) ++ "\n\n"
      ++ "---------------\n"
      ++ "-- Heap      --\n"
      ++ "---------------\n"
      ++ (prettyHeap h) ++ "\n\n"
      ++ "---------------\n"
      ++ "-- Registers --\n"
      ++ "---------------\n"
      ++ (prettyRegs r) ++ "\n\n"
      ++ "---------------\n"
      ++ "-- Flags     --\n"
      ++ "---------------\n"
      ++ (prettyFlags f) ++ "\n\n"
      ++ "---------------\n"
      ++ "-- Output    --\n"
      ++ "---------------\n"
      ++ (prettyOutput o) ++ "\n\n"
      

prettyHeap :: Heap -> String
prettyHeap heap = intercalate "\n" $ map (\(addr, v) -> (showHexWord addr) ++ " = " ++ (show v)) $ M.toList heap
prettyRegs :: Registers -> String
prettyRegs regs = intercalate "\n" $ map (\(r, v) -> (show r) ++ " = " ++ (show v)) $ M.toList regs
prettyFlags :: Flags -> String
prettyFlags flags = "NZCV\n"++ (concat $ map toBit $ M.toList flags)
  where toBit (_, b) = if b == True then "1" else "0"
prettyOutput :: Output -> String
prettyOutput output = intercalate "\n" output


emptyState :: InterpreterState
emptyState = IState 
  { heap_ = M.empty
  , regs_ = M.empty
  , flags_ = M.empty
  , output_ = []
  , program_ = emptyProgram
  }

programToState :: Program -> InterpreterState
programToState program = IState
  { heap_ = M.empty
  , regs_ = M.fromList [(PC, 0), (SP, maxU)]
  , flags_ = M.empty
  , output_ = []
  , program_ = program
  }

type ARM a = S.State InterpreterState a

-------------------------
-- Get/set functions
-------------------------
getRegVal :: Register -> ARM Value
getRegVal reg = do
  rs <- gets regs_
  return $ maybe (error $ "ERROR: Register not initialized: " ++ (show reg)) id (M.lookup reg rs)

setRegVal :: Register -> Value -> ARM ()
setRegVal reg val = modify (\s -> s { regs_ = M.insert reg val (regs_ s) })

getFlagBool :: Flag -> ARM Bool
getFlagBool flag = do
  fs <- gets flags_
  return $ maybe (error $ "ERROR: Flag not initialized: " ++ (show flag)) id (M.lookup flag fs)

setFlagBool :: Flag -> Bool -> ARM ()
setFlagBool flag b = modify (\s -> s { flags_ = M.insert flag b (flags_ s) })

getMem :: Address -> ARM Value
getMem addr = do
  m <- gets heap_
  return $ maybe (error $ "ERROR: Uninitialized memory at address " ++ (show addr)) id (M.lookup addr m)

setMem :: Address -> Value -> ARM ()
setMem addr val = modify (\s -> s { heap_ = M.insert addr val (heap_ s) })

incRegVal :: Register -> Value -> ARM ()
incRegVal reg val = do
  tmp <- getRegVal reg
  setRegVal reg (tmp + val)

decRegVal :: Register -> Value -> ARM ()
decRegVal reg val = do
  tmp <- getRegVal reg
  setRegVal reg (tmp - val)

nextPC :: ARM ()
nextPC = incRegVal PC 1

output :: String -> ARM ()
output str = modify (\s -> s { output_ = (output_ s)++[str] })
 
getLabels :: ARM LabelMap
getLabels = gets program_ >>= (return . labels_)

getInstruction :: Address -> ARM Instr
getInstruction addr = do
  mins <- getInstr' addr
  return $ maybe (error $ "ERROR: No instruction at: " ++ show addr) id mins
  where
    getInstr' :: Address -> ARM (Maybe Instr)
    getInstr' addr = liftM ((M.lookup addr).code_) $ gets program_

getCurrentInstruction :: ARM Instr
getCurrentInstruction = getRegVal PC >>= getInstruction

-------------------------
-- Evaluation 
-------------------------
evalCond :: Condition -> ARM Bool
evalCond EQ = getFlagBool FlagZ
evalCond NE = liftM not (getFlagBool FlagZ)
evalCond CS = getFlagBool FlagC
evalCond CC = liftM not (getFlagBool FlagC)
evalCond MI = getFlagBool FlagN
evalCond PL = liftM not (getFlagBool FlagN)
evalCond VS = getFlagBool FlagV
evalCond VC = liftM not (getFlagBool FlagV)
evalCond HI = do
  fc <- getFlagBool FlagC
  fz <- getFlagBool FlagZ
  return $ fc && (not fz)
evalCond LS = do
  fc <- getFlagBool FlagC
  fz <- getFlagBool FlagZ
  return $ (not fc) || fz
evalCond GE = do
  fn <- getFlagBool FlagN
  fv <- getFlagBool FlagV
  return $ fn == fv
evalCond LT = do
  fn <- getFlagBool FlagN
  fv <- getFlagBool FlagV
  return $ fn /= fv
evalCond GT = do
  fz <- getFlagBool FlagZ
  fn <- getFlagBool FlagN
  fv <- getFlagBool FlagV
  return $ (not fz) && (fn == fv)
evalCond LE = do
  fz <- getFlagBool FlagZ
  fn <- getFlagBool FlagN
  fv <- getFlagBool FlagV
  return $ fz || (fn /= fv)
evalCond NV = return False
evalCond AL = return True

evalPlusMinus :: PlusMinus -> Value -> Value -> Value
evalPlusMinus pm v1 v2 = v1 `op` v2
  where 
    op = case pm of
      PLUS  -> (+)
      MINUS -> (-)

evalBranchTarget :: BranchTarget -> ARM Address
evalBranchTarget (DIRECT addr) = return addr
evalBranchTarget (RELATIVE pm val) = do
  pc <- getRegVal PC
  return $ evalPlusMinus pm pc val
evalBranchTarget (LABEL lbl) = do
  lbls <- getLabels
  return $ maybe (error $ "ERROR: Not a label: " ++ lbl) id (M.lookup lbl lbls)

evalValOrReg :: ValOrReg -> ARM Value
evalValOrReg (IMMVAL val) = return val
evalValOrReg (REGVAL reg) = getRegVal reg

evalAddrOrReg :: AddrOrReg -> ARM Address
evalAddrOrReg (IMMADDR addr) = return addr
evalAddrOrReg (REGADDR reg) = do
  addr <- getRegVal reg
  return addr
evalAddrOrReg (REGADDRIMMOFFSET reg pm val) = do
  addr <- getRegVal reg
  return $ evalPlusMinus pm addr val
evalAddrOrReg (REGADDRREGOFFSET reg1 pm reg2) = do
  addr <- getRegVal reg1
  val <- getRegVal reg2
  return $ evalPlusMinus pm addr val

evalBinOpWord :: BinOp -> Value -> Value -> Value
evalBinOpWord ADD v1 v2 = v1 + v2
evalBinOpWord SUB v1 v2 = v1 - v2

evalBinOpInteger :: BinOp -> Integer -> Integer -> Integer
evalBinOpInteger ADD v1 v2 = v1 + v2
evalBinOpInteger SUB v1 v2 = v1 - v2

evalInstruction :: Instr -> ARM ()
evalInstruction (IMOV NoSet _ dst src) = do
  val <- evalValOrReg src
  setRegVal dst val

evalInstruction (IMOV DoSet cond dst src) = do
  val <- evalValOrReg src
  setRegVal dst val
  setFlagBool FlagN (testBit val ((finiteBitSize val) - 1))
  setFlagBool FlagZ (val == 0)
  setFlagBool FlagC False
  setFlagBool FlagV False

evalInstruction (IBINOP NoSet cond op dst src1 src2) = do
  val1 <- getRegVal src1
  val2 <- evalValOrReg src2
  setRegVal dst (evalBinOpWord op val1 val2)

evalInstruction (IBINOP DoSet cond op dst src1 src2) = do
  val1 <- getRegVal src1
  val2 <- evalValOrReg src2
  let result = evalBinOpWord op val1 val2
  let flagParam = evalBinOpInteger op (toInteger val1) (toInteger val2)
  setRegVal dst result
  setFlagBool FlagN (testBit flagParam ((finiteBitSize val1) - 1))
  setFlagBool FlagZ (result == 0)
  -- I hope this is right for subtract
  setFlagBool FlagC (testBit flagParam (finiteBitSize val1))
  -- the following is wrong
  setFlagBool FlagV False

evalInstruction (ICMP cond reg operand) = do
  val1 <- getRegVal reg
  val2 <- evalValOrReg operand
  let result = evalBinOpWord SUB val1 val2
  let flagParam = evalBinOpInteger SUB (toInteger val1) (toInteger val2)
  setFlagBool FlagN (testBit flagParam ((finiteBitSize val1) - 1))
  setFlagBool FlagZ (result == 0)
  -- I hope this is right for subtract
  setFlagBool FlagC (testBit flagParam (finiteBitSize val1))
  -- the following is wrong
  setFlagBool FlagV False

evalInstruction (ILDR cond dst src) = do
  addr <- evalAddrOrReg src
  val <- getMem addr
  setRegVal dst val

evalInstruction (ISTR cond src dst) = do
  addr <- evalAddrOrReg dst
  val <- getRegVal src
  setMem addr val

evalInstruction (IB cond target) = do
  addr <- evalBranchTarget target
  setRegVal PC addr

evalInstruction (IPUSH cond regs) = 
  _evalPush $ reverse $ sort regs
  where 
    _evalPush [] = return ()
    _evalPush (r:regs) = do
      sp  <- getRegVal SP
      val <- getRegVal r
      setMem sp val
      setRegVal SP (sp-1)
      evalInstruction (IPUSH cond regs)

evalInstruction (IPOP cond regs) = 
  _evalPop $ sort regs
  where 
    _evalPop [] = return ()
    _evalPop (r:regs) = do
      sp  <- getRegVal SP
      val <- getMem (sp+1)
      setRegVal SP (sp+1)
      setRegVal r val
      evalInstruction (IPOP cond regs)

evalInstruction (IOUT cond value) = do
  val <- evalValOrReg value
  output $ (show val)

evalInstruction instr = 
  error $ "ERROR: Not an instruction: " ++ (show instr)

evalInstructionGeneric :: Instr -> ARM ()
evalInstructionGeneric (META (HALT str) Nothing) = do
  output $ "HALT: " ++ str
evalInstructionGeneric instr = do
  nextPC
  cond' <- evalCond $ extractCondition instr
  when cond' $ do
    evalInstruction instr

stepARM :: ARM ()
stepARM = getCurrentInstruction >>= evalInstructionGeneric

step :: InterpreterState -> InterpreterState
step state = S.execState stepARM state

exec :: InterpreterState -> InterpreterState
exec state = if pc == pc' then state' else exec state'
    where 
      pc = M.lookup PC (regs_ state)
      state' = step state
      pc' = M.lookup PC (regs_ state')
      