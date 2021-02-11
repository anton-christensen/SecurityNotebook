module Language.TinyARM.Analysis where

import Language.TinyARM.Language
import Language.TinyARM.CFG
import CFG
import Analysis


type TinyArmAnalysisResult = AnalysisResult Address Instr
type TinyArmGenericAnalysisResult = GenericAnalysisResult Address Instr
