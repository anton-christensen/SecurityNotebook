-- -*- haskell -*- ---------------------------------------------------
--
-- Simple While simulator
--
----------------------------------------------------------------------
{-#LANGUAGE GADTs #-}
module While.Interpreter where

import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Monad.State (gets,modify)
import Control.Monad.Except

import While.Language
import Text.Parsec.Pos
import qualified While.Parser as Parser (parseProgramFile,parseProgram)

import System.Console.Haskeline


----------------------------------------------------------------------
-- The Language
----------------------------------------------------------------------


-- type Var = String
-- type Nat = Int

-- data BinOp = Plus | Minus | Mult | Div | Eq deriving (Eq,Show)

-- data AExpr a = LIT Nat
--              | VAR Var
--              | OP Expr BinOp Expr
--              | DEREF Expr
--              | CAST Expr
--              | EANNO a (AExpr a)
--              deriving (Eq,Show)

-- data ACmd a = ASGN Var Expr
--             | PTRASGN Expr Expr
--             | ALLOC Var Expr
--             | FREE Expr Expr
--             | SEQ [Cmd]
--             | IF Expr Cmd Cmd
--             | WHILE Expr Cmd
--             | LEAK Expr
--             | SKIP
--             | CANNO a (ACmd a)
--             deriving (Eq,Show)

-- type Expr = AExpr ()
-- type Cmd = ACmd ()

----------------------------------------------------------------------
-- Memory Manager
----------------------------------------------------------------------

-- 
newtype Loc = Loc Int deriving (Eq,Show,Ord)
type Pointer = (Loc,Int)
data Val = N Nat | PTR Pointer deriving (Eq,Show)

type Memory = M.Map Var Val

-- type Segment = (Loc,Loc)
type Segments = M.Map Loc Loc
type HeapFun = M.Map Loc Val
type Heap = (HeapFun,Segments)

data MemoryManager =
  MemMan { heap_        :: Heap
         , allocator_   :: Heap -> Nat -> Maybe (Heap,Loc)
         , deallocator_ :: Heap -> Loc -> Nat -> Maybe Heap
         , converter_   :: Heap -> Pointer -> Maybe Loc
         , cast_        :: Maybe (Pointer -> Nat)
         , heapupd_     :: Heap -> Loc -> Val -> Maybe Heap
         , heaplookup_  :: Heap -> Loc -> Maybe Val
         }

baseLoc :: Heap -> Loc -> Bool
baseLoc (_,seg) loc = M.member loc seg


--------------------------------------------------
-- Memory Manager 0
--------------------------------------------------

-- NOTE:
--  (1) No initialisation of new memory
--  (2) Allocates next location; may re-use old locations
--  (3) No sanitisation checks
--  (4) Cannot fail(!)
alloc0 :: Heap -> Nat -> Maybe (Heap,Loc)
alloc0 (hfun,seg) n
  | n < 1     = Nothing
  | otherwise = Just ((hfun,seg'),Loc loc)
  where
    loc = maybe 0 (\((Loc p),(Loc l)) -> l + 1) (M.lookupMax seg)
    seg' = M.insert (Loc loc) (Loc $ loc + n - 1) seg

-- NOTE:
--  (1) No initialisation/sanitisation of free'd memory
--  (2) Fails if "loc" does not correspond to a base pointer
dealloc0 :: Heap -> Loc -> Nat -> Maybe Heap
dealloc0 (hfun,seg) loc n
  | baseLoc (hfun,seg) loc = Just (hfun,seg')
  | otherwise              = Nothing
  where
    seg' = M.delete loc seg

-- NOTE:
--  (1) No sanity checks are performed
--  (2) Simple pointer arithmetic
converter0 :: Heap -> Pointer -> Maybe Loc
converter0 _ (Loc loc,off) = Just $ Loc (loc + off)

cast0 :: Pointer -> Nat
cast0 (Loc loc,off) = loc + off

-- NOTE:
--  (1) Returns random value (117) when reading uninitialised mem
heaplookup0 :: Heap -> Loc -> Maybe Val
heaplookup0 (hfun,_) loc = Just $ maybe (N 117) id (M.lookup loc hfun)

-- NOTE:
--  (1) Never fails(!)
--  (2) No sanity checking
heapupd0 :: Heap -> Loc -> Val -> Maybe Heap
heapupd0 (hfun,seg) loc v = Just (M.insert loc v hfun,seg)

--
memmgr0 :: MemoryManager
memmgr0 =
  MemMan { heap_        = (M.empty,M.empty)
         , allocator_   = alloc0
         , deallocator_ = dealloc0
         , converter_   = converter0
         , cast_        = Just cast0
         , heapupd_     = heapupd0
         , heaplookup_  = heaplookup0
         }

----------------------------------------------------------------------
-- The Interpreter Monad
----------------------------------------------------------------------

data InterpreterState = 
    IState { mem_         :: Memory
           , output_      :: String
           , adversary_   :: [String]
           , memmgr_      :: MemoryManager
           }

initstate :: InterpreterState
initstate = IState { mem_         = M.empty
                   , output_      = ""
                   , adversary_   = []
                   , memmgr_      = memmgr0
                   }

-- The interpreter monad
type Interp = ExceptT String (S.State InterpreterState)

-------------------------
--
-------------------------
b2n :: Bool -> Val
b2n True  = N 1
b2n False = N 0

lookupE :: String -> Maybe a -> Interp a
lookupE _ (Just a) = return a
lookupE err _      = throwError err

modifyMM :: (MemoryManager -> MemoryManager) -> Interp ()
modifyMM upd = modify (\s -> s { memmgr_ = upd (memmgr_ s) })

ptr2loc :: Pointer -> Interp Loc
ptr2loc ptr = do
  mgr <- gets memmgr_
  let conv = converter_ mgr
  let heap = heap_ mgr
  lookupE ("[ptr2loc] Invalid pointer: " ++ show ptr) (conv heap ptr)

--
-- getSeg :: Pointer -> Interp Segment
-- getSeg (l,o) = do
--   mgr <- gets memmgr_
--   let (_,seg) = heap_ mgr
--   lookupE ("[getSeg] Invalid pointer: " ++ (show (l,o))) (M.lookupLE l seg)

-- Normalise a pointer, i.e., replace location in pointer with corresponding
--   base pointer and calculate new offset
-- NOTE: only works for "valid" pointers, i.e., pointers that are not
--       overrunning their segment
-- normalisePtr :: Pointer -> Interp Pointer
-- normalisePtr ptr = do
--   (Loc lmin,Loc lmax) <- getSeg ptr
--   (Loc loc) <- ptr2loc ptr
--   unless (lmin <= loc && loc <= lmax)
--     (throwError $ "Invalid pointer: " ++ (show ptr))
--   let offset = loc - lmin  -- NEED: loc = lmin + offset
--   return (Loc lmin,offset)

alloc :: Nat -> Interp Pointer
alloc n = do
  mgr <- gets memmgr_
  let heap = heap_ mgr
  let myalloc = allocator_ mgr
  (newheap,loc) <- lookupE "[alloc] Allocation failed" $ myalloc heap n
  modifyMM (\mgr -> mgr { heap_ = newheap })
  output $ "ALLOC: " ++ (show n) ++ "@(" ++ (show loc) ++ ",0)"
  return (loc,0)

free :: Pointer -> Nat -> Interp ()
free ptr n = do
  mgr <- gets memmgr_
  let heap = heap_ mgr
  let dealloc = deallocator_ mgr
  loc <- ptr2loc ptr
  newheap <- lookupE ("Invalid free (only base pointers): " ++ (show ptr)) $
    dealloc heap loc n
  modifyMM (\mgr -> mgr { heap_ = newheap })
  output $ "FREE: " ++ (show ptr)
  return ()



-------------------------
--
-------------------------

cast :: Pointer -> Interp Nat
cast ptr = do
  mgr <- gets memmgr_
  mcast <- lookupE "`cast' not supported in this memory manager" (cast_ mgr)
  return $ mcast ptr

heaplookup :: Pointer -> Interp Val
heaplookup ptr = do
  mgr <- gets memmgr_
  let hlookup = heaplookup_ mgr
  let heap = heap_ mgr
  loc <- ptr2loc ptr
  lookupE ("[heaplookup] Lookup failed for loc: " ++ show loc) (hlookup heap loc)

heapupdate :: Pointer -> Val -> Interp ()
heapupdate ptr v = do
  mgr <- gets memmgr_
  let hupd = heapupd_ mgr
  let heap = heap_ mgr
  loc <- ptr2loc ptr
  newheap <- lookupE "[heapupdate] Update failed" $ hupd heap loc v
--  modify (\s -> s { memmgr_ = mgr { heap_ = newheap } })
  modifyMM (\mgr -> mgr { heap_ = newheap })

output :: String -> Interp ()
output str = modify (\s -> s { output_ = (output_ s) ++ str ++ "\n"})

copytoadv :: String -> Interp ()
copytoadv str = modify (\s -> s { adversary_ = str : (adversary_ s) })

memget :: Var -> Interp Val
memget x = do
  m <- gets mem_
  lookupE ("Unknown variable: " ++ (show x)) (M.lookup x m)

memset :: Var -> Val -> Interp ()
memset x v = modify (\s -> s { mem_ = M.insert x v (mem_ s) })

ptrArith :: Val -> BinOp -> Val -> Interp Val
ptrArith (PTR (p,l)) Plus (N i) | i >= 0 = return $ PTR (p,l+i)
ptrArith (PTR ptr1) Eq (PTR ptr2) = do
  loc1 <- ptr2loc ptr1
  loc2 <- ptr2loc ptr2
  return $ b2n $ loc1 == loc2
ptrArith v1 bop v2 = throwError $ concat ["Illegal pointer arithmetic: "
                                         , (show v1)
                                         , " "
                                         , (show bop)
                                         , " "
                                         , (show v2)
                                         ]




----------------------------------------------------------------------
-- Evaluating Expressions
----------------------------------------------------------------------

evalBinOp :: Val -> BinOp -> Val -> Interp Val
evalBinOp (N i1) Plus  (N i2) = return $ N (i1 + i2)
evalBinOp (N i1) Minus (N i2) = return $ N (i1 - i2)
evalBinOp (N i1) Eq    (N i2) = return $ b2n (i1 == i2)
evalBinOp (N i1) Lt    (N i2) = return $ b2n (i1 < i2)
evalBinOp (N i1) LE    (N i2) = return $ b2n (i1 <= i2)
evalBinOp (N i1) Gt    (N i2) = return $ b2n (i1 > i2)
evalBinOp (N i1) GE    (N i2) = return $ b2n (i1 >= i2)
evalBinOp v1     bop   v2     = ptrArith v1 bop v2


isBool :: Val -> Interp Bool
isBool (N 0) = return False
isBool (N _) = return True
isBool v     = throwError $ "Bad bool: " ++ (show v)

isNat :: Val -> Interp Nat
isNat (N n) = return n
isNat v     = throwError $ "Bad nat: " ++ (show v)

isPtr :: Val -> Interp Pointer
isPtr (PTR p) = return p
isPtr v       = throwError $ "Bad pointer: " ++ (show v)

evalBoolExpr :: Expr -> Interp Bool
evalBoolExpr e = evalExpr e >>= isBool

evalNatExpr :: Expr -> Interp Nat
evalNatExpr e = evalExpr e >>= isNat

evalPtrExpr :: Expr -> Interp Pointer
evalPtrExpr e = evalExpr e >>= isPtr
  
evalExpr :: Expr -> Interp Val
evalExpr (LIT n) = return $ N n
evalExpr (VAR x) = memget x
evalExpr (OP e1 bop e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  evalBinOp v1 bop v2
evalExpr (CAST e) = do
  ptr <- evalPtrExpr e
  cptr <- cast ptr
  return $ N cptr
evalExpr (DEREF e) = do
  ptr <- evalPtrExpr e
  heaplookup ptr


--
stepE :: Expr -> InterpreterState -> Either String Val
stepE e s = S.evalState (runExceptT (evalExpr e)) s


evalCmd :: ACmd a -> Interp [ACmd a]
evalCmd SKIP = return []
evalCmd (ASGN x e) = do
  res <- evalExpr e
  memset x res
  return []
evalCmd (PTRASGN e1 e2) = do
  ptr <- evalPtrExpr e1
  v <- evalExpr e2
  heapupdate ptr v
  return []
evalCmd (SEQ []) = return []
evalCmd (SEQ (c:cmds)) = do
  cmds' <- evalCmd c
  return (cmds' ++ cmds)
evalCmd (IF e c1 Nothing) = do
  v <- evalBoolExpr e
  if v
    then evalCmd c1
    else return []
evalCmd (IF e c1 (Just c2)) = do
  v <- evalBoolExpr e
  evalCmd $ if v then c1 else c2
evalCmd (WHILE e c) = do
  v <- evalBoolExpr e
  if v
    then (do cmds <- evalCmd c
             return $ cmds ++ [WHILE e c]
         )
    else return []
evalCmd (LEAK e) = do
  v <- evalExpr e
  copytoadv (show v)
  return []
evalCmd (ALLOC x e) = do
  n <- evalNatExpr e
  ptr <- alloc n
  memset x (PTR ptr)
  return []
evalCmd (FREE e1 e2) = do
  p <- evalPtrExpr e1
  free p 0
  return []
evalCmd (INPUT x y) = do
  v <- memget ("_INPUT_" ++ y)
  output $ "INPUT (on channel " ++ y ++ ")"
  memset x v
  return []
evalCmd (OUTPUT e) = do
  v <- evalExpr e
  output (show v)
  return []
evalCmd (CANNO _ cmd) =
  evalCmd cmd
-- evalCmd cmd = do
--   throwError $ "[evalCmd] Command currently not supported: " ++ show cmd

--
stepC :: ACmd a -> InterpreterState -> Either String ([ACmd a],InterpreterState)
stepC c s = case status of
              Left err   -> Left err
              Right cmds -> Right (cmds,s')
  where
    (status,s') = S.runState (runExceptT (evalCmd c)) s

stepsC' :: [ACmd a] -> InterpreterState -> Maybe Int -> Either String ([ACmd a],InterpreterState)
stepsC' _  s (Just 0) = Right ([],s)
stepsC' [] s _        = Right ([],s)
stepsC' (c:cmds) s n = do
  (cmds',s') <- stepC c s
  stepsC' (cmds' ++ cmds) s' (fmap (\m -> m-1) n)

stepsC :: ACmd a -> InterpreterState -> Either String InterpreterState
stepsC c s = fmap snd $ stepsC' [c] s Nothing


----------------------------------------------------------------------
-- Interpreter / Interface
----------------------------------------------------------------------

myunlines :: [String] -> String
myunlines strs = unlines $ map ("| "++) strs

mybox :: String -> [String] -> String
mybox title strs =
  concat [ "+--| ", title, " |-----\n"
         , myunlines strs
         , "|"
         ]

showMem :: Memory -> String
showMem mem =
  mybox "Memory" $ map (\(k,v) -> concat [show k," --> ",show v] ) (M.assocs mem)

showHeap :: Heap -> String
showHeap (hfun,seg) =
  concat [ mybox "HeapFun" $ map (\(k,v) -> concat [show k," --> ",show v] ) (M.assocs hfun)
         , "\n"
         , mybox "Segments" $
           map (\(lb,le) -> concat ["[",show lb,",",show le,"]"]) (M.assocs seg)
         ]

showIState :: InterpreterState -> String
showIState istate =
  concat [ showHeap $ heap_ (memmgr_ istate)
         , "\n"
         , showMem $ mem_  istate
         , "\n"
         , mybox "OUTPUT" (lines $ output_ istate)
         , "\n"
         , mybox "LEAKS" $ reverse (adversary_ istate)
         ]

showRes :: InterpreterState -> String
showRes istate = mybox "FINAL STATE" $ lines $ showIState istate

showErr :: String -> String -> String
showErr title err = mybox title (lines err)

--------------------------------------------------

-- type GState a = Either String ([ACmd a],InterpreterState)

data GState a =
  GState { _g_err    :: Maybe String
         , _g_istate :: InterpreterState
         , _g_cmds   :: [ACmd a]
         , _g_prg    :: Either String String
         }
type GInteract a = GState a -> GState a

ginitstate :: GState a
ginitstate = GState { _g_err    = Nothing
                    , _g_istate = initstate
                    , _g_cmds   = []
                    , _g_prg    = Right ""
                    }

--
greset :: GState a
greset = ginitstate

--
gclearerr :: GInteract a
gclearerr gstate = gstate { _g_err = Nothing }

--
gclearprg :: GInteract a
gclearprg gstate = gstate { _g_prg = Right "" }

-- Set the command to be executed
gsetCmds :: [ACmd a] -> GInteract a
gsetCmds cmds gstate =
  (gclearerr gstate) { _g_cmds = cmds }

gaddCmds :: [ACmd a] -> GInteract a
gaddCmds cmds gstate =
  (gclearerr gstate) { _g_cmds = cmds ++ (_g_cmds gstate) }


-- Set the current interpreterstate
gsetState :: InterpreterState -> GInteract a
gsetState istate gstate = (gclearerr gstate) { _g_istate = istate }

--
gsetErr :: String -> GInteract a
gsetErr err gstate = gstate { _g_err = Just err }

gsetParseErr :: String -> GInteract a
gsetParseErr err gstate = gstate { _g_prg = Left err }

--
gloadString :: String -> GInteract ()
gloadString str gstate =
  case prg of
    Left err  -> gsetParseErr (show err) gstate
    Right cmd -> (gsetCmds [forgetCmdA cmd] gstate) { _g_prg = Right str }
  where
    prg = Parser.parseProgram str

-- Take one step in the G-interface
gstepC :: GInteract a
gstepC gstate@(GState { _g_err = (Just _) }) = gstate
gstepC gstate@(GState { _g_cmds = [] }) = gstate
gstepC gstate@(GState { _g_cmds = (c:cmds) }) =
  case (stepC c (_g_istate gstate)) of
    Left err -> gsetErr err gstate
    Right (cmds',istate') -> gsetState istate' $ gsetCmds (cmds'++cmds) gstate

--
gprettyGState :: Show a => GState a -> String
gprettyGState gstate = 
  concat [ "Runtime error: " ++ (show $ _g_err gstate)
         , "\n"
         , "Remaining commands: " ++ (show $ _g_cmds gstate)
         , "\n"
         , "Source: " ++ either (\x -> "PARSE ERROR: " ++ x) id (_g_prg gstate)
         , "\n"
         , mybox "Interpreter state" $ lines $ showIState $ _g_istate gstate
         ]

--
gshowGState :: Show a => GState a -> String
gshowGState gstate = mybox "CURRENT STATE" (lines $ gprettyGState gstate)



--------------------------------------------------

type IInterface = ExceptT String IO

data OPT = OMEM Var Val
         | OINPUT Var Val
         | OSTATE InterpreterState

loadCommand :: String -> IInterface (ACmd SourcePos)
loadCommand fname = do
  liftIO $ putStr "PARSING..."
  res <- liftIO $ Parser.parseProgramFile fname
  liftIO $ putStrLn "DONE."
  case res of
    Left err  -> throwError $ showErr "PARSE ERROR" (show err)
    Right cmd -> return cmd

loadString :: String -> IInterface (ACmd SourcePos)
loadString prg = do
  liftIO $ putStr "PARSING..."
  let res = Parser.parseProgram prg
  liftIO $ putStrLn "DONE."
  case res of
    Left err  -> throwError $ showErr "PARSE ERROR" (show err)
    Right cmd -> return cmd

execCmd :: ACmd a -> InterpreterState -> IInterface InterpreterState
execCmd cmd istate = do
  liftIO $ putStr "EXECUTING..."
  let res = stepsC cmd istate
  liftIO $ putStrLn "DONE."
  case res of
    Left err      -> throwError $ showErr "RUNTIME ERROR" (show err)
    Right istate' -> return istate'

runProgram' :: String -> InterpreterState -> IInterface ()
runProgram' fname istate = do
  cmd <- loadCommand fname
  res <- execCmd cmd istate
  liftIO $ putStrLn $ showRes res

handleOPT :: InterpreterState -> [OPT] -> InterpreterState
handleOPT istate [] = istate
handleOPT istate ((OSTATE istate'):opts) = handleOPT istate' opts
handleOPT istate ((OMEM x v):opts) =
  handleOPT (istate { mem_ = M.insert x v (mem_ istate)}) opts
handleOPT istate ((OINPUT x v):opts) =
  handleOPT (istate { mem_ = M.insert ("_INPUT_" ++ x) v (mem_ istate)}) opts

runProgram :: String -> [OPT] -> IO ()
runProgram fname opts = do
  res <- runExceptT $ runProgram' fname (handleOPT initstate opts)
  either putStrLn return res

runCommand' :: String -> InterpreterState -> IInterface ()
runCommand' prg istate = do
  cmd <- loadString prg
  res <- execCmd cmd istate
  liftIO $ putStrLn $ showRes res

runCommand :: String -> [OPT] -> IO ()
runCommand prg opts = do
  res <- runExceptT $ runCommand' prg (handleOPT initstate opts)
  either putStrLn return res
    
interpreter :: IO ()
interpreter = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "w> "
           case minput of
               Nothing -> return ()
               Just ":quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop

-- interpreter :: IO ()
-- interpreter = loop
--   where
--     loop :: IO ()
--     loop = do
--       putStr "while> "
--       line <- getLine
--       case line of
--         ":quit" -> return ()
--         x -> do putStrLn $ "FOO: " ++ x
--                 loop

----------------------------------------------------------------------
-- Examples and Test Programs
----------------------------------------------------------------------

testMem :: Memory
testMem = M.fromList [("x",PTR (Loc 41,1))]

testHeap :: Heap
testHeap = (M.fromList [(Loc 42,N 87)],M.fromList [(Loc 0,Loc 100)])

testState :: InterpreterState
testState = initstate { mem_ = testMem
                      , memmgr_ = (memmgr_ initstate) { heap_ = testHeap }
                      }

testExpr1 :: Expr
testExpr1 = VAR "x"

testExpr2 :: Expr
testExpr2 = CAST (VAR "x")

testExpr3 :: Expr
testExpr3 = CAST (VAR "y")

testExpr4 :: Expr
testExpr4 = DEREF (VAR "x")

runExprTests = do
  mapM_ (\e -> print $ stepE e testState) [testExpr1,testExpr2,testExpr3,testExpr4] 

ex'p1 :: Cmd
ex'p1 = SEQ [ ALLOC "p" (LIT 1)
            , IF (VAR "h") (ALLOC "q" (LIT 1)) Nothing
            , ALLOC "p'" (LIT 1)
            , LEAK (OP (CAST (VAR "p'")) Minus (CAST (VAR "p")))
            ]

ex'p2 :: Cmd
ex'p2 = SEQ [ ALLOC "p" (LIT 1)
            , LEAK (VAR "p")
            ]

ex'p3 :: Cmd
ex'p3 = SEQ [ ALLOC "q" (LIT 3)
            , FREE (VAR "q") (LIT 0)
            , IF (VAR "h") (ALLOC "p" (LIT 1000)) (Just (ALLOC "p" (LIT 1)))
            , IF (OP (VAR "p") Eq (VAR "q")) (ASGN "x" (LIT 0)) (Just (ASGN "x" (LIT 1)))
            , LEAK (VAR "x")
            ]

ex'p4 :: Cmd
ex'p4 = SEQ [ ALLOC "k" (LIT 2)
            , PTRASGN (OP (VAR "k") Plus (LIT 0)) (LIT 41)
            , PTRASGN (OP (VAR "k") Plus (LIT 1)) (LIT 42)
            , FREE (VAR "k") (LIT 0)
            , ALLOC "u" (LIT 3)
            , PTRASGN (OP (VAR "u") Plus (LIT 0)) (LIT 1)
            , PTRASGN (OP (VAR "u") Plus (LIT 2)) (LIT 3)
            , LEAK (DEREF (OP (VAR "u") Plus (LIT 1)))
            ]

-- #+NAME: p5
-- #+BEGIN_SRC
--   x = input(h);
--   p = alloc(1);
--   copy_to_adv(p);
-- #+END_SRC

-- #+NAME: p6
-- #+BEGIN_SRC
--   x = input(h);
--   x = 0;
--   p = alloc(1);
--   copy_to_adv(*p);  /* only deref'ed value of p is leaked */
-- #+END_SRC

ex'p7 :: Cmd
ex'p7 = SEQ [ ALLOC "p" (LIT 1)
            , IF (OP (DEREF (VAR "p")) Eq (LIT 42)) (LEAK (VAR "h")) Nothing
            ]

