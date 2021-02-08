----------------------------------------------------------------------
-- 
-- TinyRISC Assembly Parser
--
----------------------------------------------------------------------
{-#LANGUAGE GADTs #-}
module TinyARM.RiscParser (parseRiscV) where

-- import Data.Maybe (catMaybes)
-- import Prelude hiding (EQ,LT,GT)
import Text.Parsec hiding (Line)
-- import Text.Parsec.String
-- import Data.Char (isSpace,digitToInt,isAlphaNum,toLower,toUpper)
-- import Data.Word
-- import qualified Data.Map as M

-- import TinyARM.Language


parseFile :: Parser Program
parseFile = (do 
  optional lineEndingWhiteSpaces
  cmds <- (parseLine `sepEndBy` lineEndingWhiteSpaces)
  eof
  return $ linesToProgram cmds) <?> "File"

linesToProgram :: [Line] -> Program
linesToProgram lines = 
  linesToProgram' lines 0 emptyProgram
  where 
    linesToProgram' :: [Line] -> Address -> Program -> Program
    linesToProgram' [] addr prog = 
      emptyProgram { code_ = M.insert addr (META (HALT "end of program") Nothing) (code_ prog) }
    linesToProgram' ((LInstruction instr):rest) addr prog = 
      let p = (linesToProgram' rest (addr+1) prog) in
        (p {code_ = M.insert addr instr (code_ p)})
    linesToProgram' ((LLabel lbl):rest) addr prog = 
      let p = (linesToProgram' rest addr prog) in
        (p {labels_ = M.insert lbl addr (labels_ p)})

parseRiscV :: String -> Either ParseError Program
parseRiscV input = (parse parseProg "(unknown)" input) 

parseProg = pLines

pLines = try $ pLine 
         <|> try $ pLine >> pLines
         <|> pStmt

pLine = try (pStmt >> pnl)
    <|> try pnl
    <|> error "newline"

pStmt = try $ pLabel >> pInstr
    <|> try $ pLabel >> pData
    <|> try $ pLabel
    <|> try $ pData
    <|> try $ pInstr
    <|> pDirective

pLabel = pLabelName >> char ':'

pInstr = try $ pRV32I
     <|> pPseudo

pPseudo = error "Pseudo"

-- pRV32I = 


