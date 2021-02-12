module Language.While.Analysis where

import Language.While.Language
import Language.While.CFG
import CFG
import Analysis
import Text.Parsec

type WhileAnalysisResult = AnalysisResult SourcePos Cmd
type WhileGenericAnalysisResult = GenericAnalysisResult SourcePos Cmd
