module Language.While (parseWhile, executeWhile, dummyAnalysis, taintAnalysis) where

import Language.While.Parser
import Language.While.Interpreter
import Language.While.Language
import Language.While.CFG
import Language.While.Analysis
import Analysis
import CFG
import Text.Parsec

import qualified Language.While.Analysis.Taint as T
import Text.Read (readMaybe)
import Data.Maybe

parseWhile = parseProgram

executeWhile :: String -> String -> Int -> Either String (String, Int)
executeWhile code inputStr stepCount =
    case inputAsList of
        Nothing -> Left "Input contains line that doesn't parse as integer"
        Just input -> case parseProgram code of
            Left error -> Left $ "Parse error: " ++ (show error)
            Right prog -> case runToEnd prog (initstate input) stepCount of
                Left error -> Left $ "Exception: " ++ error
                Right (state, i) -> Right (showIState state, i)
    where
        inputAsList :: Maybe [Int]
        inputAsList = let 
            maybeList = map readMaybe $ lines inputStr
            properList = catMaybes maybeList
            in
                if length maybeList == length properList then Just properList else Nothing


        runToEnd :: ACmd a -> InterpreterState -> Int -> Either String (InterpreterState, Int)
        runToEnd prog initialState maxSteps = fmap (\(_, s, n) -> (s,n) ) $ runToEnd' [prog] initialState 0 $ if maxSteps >= 0 then Just maxSteps else Nothing


        runToEnd' :: [ACmd a] -> InterpreterState -> Int -> Maybe Int -> Either String ([ACmd a],InterpreterState, Int)
        runToEnd' _  s n (Just 0) = Right ([],s,n)
        runToEnd' [] s n _        = Right ([],s,n)
        runToEnd' (c:cmds) s n cnt = do
            (cmds',s') <- stepC c s
            runToEnd' (cmds' ++ cmds) s' (n+1) (fmap (\m -> m-1) cnt)



_analysis :: (Int -> WhileCFG -> WhileAnalysisResult a) -> (a -> String) -> CmdWithPos -> Int -> WhileGenericAnalysisResult
_analysis analysis statePrinter program nSteps =
    let result = analysis nSteps $ makeCFG program
    in analysisToGeneric result printSrcPos prettyPrintInstr statePrinter

taintAnalysis :: CmdWithPos -> Int -> WhileGenericAnalysisResult
taintAnalysis = _analysis T.taintAnalysis T.prettyPrintState

dummyAnalysis :: CmdWithPos -> Int -> GenericAnalysisResult SourcePos (ACmd ())
dummyAnalysis cmd _ = GenericAnalysisResult
    { gaDotGraph = toDotGraph (makeCFG cmd) printSrcPos prettyPrintInstr
    , gaSteps = 0
    , gaWorkList = []
    , gaGraph = makeCFG cmd }

-- parseWhile = parseProgram



exampleProgram = "\
    \ b = 0; \n\
    \ a = input(b); \n\
    \ if a < b { \n\
    \ a = b; \n\
    \ b = a; \n\
    \ } \n\
    \ output a ;"


-- (CANNO "<input string>" (line 1, column 2) (SEQ 
--     [ CANNO "<input string>" (line 1, column 2) (ASGN "b" (LIT 0))
--     , CANNO "<input string>" (line 2, column 2) (INPUT "a" "b")
--     , CANNO "<input string>" (line 3, column 2) (OUTPUT (VAR "a"))
--     ]
-- ))


-- (CANNO "<input string>" (line 1, column 2) (SEQ 
--     [ CANNO "<input string>" (line 1, column 2) (ASGN "b" (LIT 0))
--     , CANNO "<input string>" (line 2, column 2) (INPUT "a" "b")
--     , CANNO "<input string>" (line 3, column 2) (IF (OP (VAR "a") Lt (VAR "b")) (CANNO "<input string>" (line 4, column 2) (SEQ 
--         [ CANNO "<input string>" (line 4, column 2) (ASGN "a" (VAR "b"))
--         , CANNO "<input string>" (line 5, column 2) (ASGN "b" (VAR "a"))
--         ])) Nothing)
--     , CANNO "<input string>" (line 7, column 2) (OUTPUT (VAR "a"))]))