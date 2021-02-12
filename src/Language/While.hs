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

parseWhile = parseProgram

executeWhile :: String -> Int -> Either String String
executeWhile code stepCount = 
    case parseProgram code of
        Left error -> Left $ "Parse error: " ++ (show error)
        Right prog -> case stepsC prog initstate of
            Left error -> Left $ "Exception: " ++ error
            Right state -> Right $ showIState state 


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