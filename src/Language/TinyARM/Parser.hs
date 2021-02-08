----------------------------------------------------------------------
-- 
-- TinyARM Assembly Parser
--
----------------------------------------------------------------------
{-#LANGUAGE GADTs #-}
module Language.TinyARM.Parser (parseTinyARM) where

import Data.Maybe (catMaybes)
import Prelude hiding (EQ,LT,GT)
import Text.Parsec hiding (Line)
import Text.Parsec.String
import Data.Char (isSpace,digitToInt,isAlphaNum,toLower,toUpper)
import Data.Word
import qualified Data.Map as M

import Language.TinyARM.Language

data Line 
  = LInstruction (Instruction SpecialInstructions)
  | LLabel Label
  deriving (Show)

symbol :: String -> Parser String
symbol name = lexeme (string name)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  many whitespace
  return x

-- | Debugging function
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof




skip :: Parser a -> Parser ()
skip p = p >> return ()

lfOrEof :: Parser ()
lfOrEof = skip (char '\n') <|> eof

isWhitespace :: Char -> Bool
isWhitespace c = isSpace c && c /= '\n' && c /= '\r'

whitespace :: Parser ()
whitespace = skip (satisfy isWhitespace) <?> "whitespace"

comment :: Parser ()
comment = skip (char ';' >> manyTill anyChar (lookAhead (lfOrEof))) <?> "comment"

whitespaceOrComment :: Parser ()
whitespaceOrComment = whitespace <|> comment

optionalWhitespaces :: Parser ()
optionalWhitespaces = optional $ try $ many whitespace 

-- | skip one or more lines of whitespace followed by newlines
lineEndingWhiteSpaces :: Parser ()
lineEndingWhiteSpaces =  skip $ many1 (try $ many whitespaceOrComment >> newline) -- maybe change to lfOrEof

-- | Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- | Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

int :: Parser Integer
int = (do 
  f <- lexeme sign
  n <- nat
  return (f n)) <?> "signed integer"

sign :: Parser (Integer -> Integer)
sign = 
  (char '-' >> return negate) <|> 
  (char '+' >> return id) <|> 
  return id <?> "sign function"

nat :: Parser Integer
nat = zeroNumber <|> 
      decimal <?> "nat"

zeroNumber :: Parser Integer
zeroNumber = do 
  char '0'
  hexadecimal <|> octal <|> decimal <|> return 0 <?> "zero number"

decimal :: Parser Integer
decimal = (number 10 digit) <?> "decimal notation number"

hexadecimal :: Parser Integer
hexadecimal = (oneOf "xX" >> number 16 hexDigit) <?> "hexadecimal integer"

octal :: Parser Integer
octal = (oneOf "oO" >> number 8 octDigit) <?> "Octal integer"

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)


parseBinOp :: Parser BinOp
parseBinOp =
  (caseInsensitiveString "add" >> return ADD) <|>
  (caseInsensitiveString "sub" >> return SUB)

parseSetFlags :: Parser SetFlags
parseSetFlags = option NoSet (caseInsensitiveChar 's' >> return DoSet)

parseCondition :: Parser Condition
parseCondition =
  try (caseInsensitiveString "_eq" >> return EQ) <|>
  try (caseInsensitiveString "_ne" >> return NE) <|>
  try (caseInsensitiveString "_cs" >> return CS) <|>
  try (caseInsensitiveString "_cc" >> return CC) <|>
  try (caseInsensitiveString "_mi" >> return MI) <|>
  try (caseInsensitiveString "_pl" >> return PL) <|>
  try (caseInsensitiveString "_vs" >> return VS) <|>
  try (caseInsensitiveString "_vc" >> return VC) <|>
  try (caseInsensitiveString "_hi" >> return HI) <|>
  try (caseInsensitiveString "_ls" >> return LS) <|>
  try (caseInsensitiveString "_ge" >> return GE) <|>
  try (caseInsensitiveString "_lt" >> return LT) <|>
  try (caseInsensitiveString "_gt" >> return GT) <|>
  try (caseInsensitiveString "_le" >> return LE) <|>
  try (caseInsensitiveString "_nv" >> return NV) <|>
  try (caseInsensitiveString "_al" >> return AL) <|>
  return AL

regNumber :: Parser Int
regNumber = do
  r <- nat
  if not (0 <= r && r <= 13) 
  then fail $ "invalid register number: " ++ (show r)
  else return $ fromInteger r

parseRegister :: Parser Register
parseRegister = 
  try (caseInsensitiveString "pc" >> return PC) <|>
  try (caseInsensitiveString "sp" >> return SP) <|>
  try (caseInsensitiveString "lr" >> return LR) <|>
  parseGeneralRegister <?> "Register"

parseGeneralRegister :: Parser Register
parseGeneralRegister = 
  (caseInsensitiveChar 'r' >> regNumber >>= return . R)  <|>
  (caseInsensitiveChar 'h' >> regNumber >>= return . RH) <?> "General register"

parsePlusMinus :: Parser PlusMinus
parsePlusMinus = 
  (char '+' >> return PLUS) <|> (char '-' >> return MINUS) <?> "Sign"

parseValue :: Parser Value
parseValue = nat >>= return . fromInteger <?> "Value"

parseImm :: Parser Value
parseImm = char '#' >> parseValue >>= return <?> "Immidiate"

parseAddress :: Parser Address
parseAddress = parseImm <?> "Address"

parseLabelIdentifier :: Parser Label
parseLabelIdentifier = many1 (satisfy (\c -> (isAlphaNum c) || elem c "._" )) <?> "Label"

parseValOrReg :: Parser ValOrReg
parseValOrReg = 
  (parseImm >>= return . IMMVAL) <|>
  (parseRegister >>= return . REGVAL) <?> "Val or Register"

parseAddrOrReg :: Parser AddrOrReg
parseAddrOrReg =
  try (parseAddress >>= return . IMMADDR) <|>
  try (parseRegister >>= return . REGADDR) <|>
  (do
    lbrack
    rn <- parseRegister
    many whitespace
    comma
    offset <- ((try 
      (do
      char '#'
      pm <- option PLUS parsePlusMinus
      val <- parseValue
      many whitespace
      return $ REGADDRIMMOFFSET rn pm val)
      ) <|> 
      (do
        pm <- option PLUS parsePlusMinus
        rm <- parseRegister
        many whitespace
        return $ REGADDRREGOFFSET rn pm rm)
      )
    rbrack
    return offset
  ) <?> "Address or Register"

parseBranchTarget :: Parser BranchTarget
parseBranchTarget = 
    (parseValue >>= return . DIRECT) <|>
    (do
      pm <- parsePlusMinus
      val <- parseValue
      return $ RELATIVE pm val
    ) <|>
    (do
      lbl <- parseLabelIdentifier
      return $ LABEL lbl
    ) <?> "branch target"

parseRegisterList :: Parser [Register]
parseRegisterList = do
  symbol "{"
  regs <- regOrRange `sepBy` argumentSeperator
  symbol "}"
  return $ concat regs
  where
    regOrRange = 
      try (do
        ra <- parseGeneralRegister
        char '-' 
        rb <- parseGeneralRegister
        case ra of
          R ia -> case rb of
            R ib -> if ia >= ib 
              then unexpected "register range must be strictly increasing"
              else return (map (R) [ia..ib])
            RH _ -> unexpected "incompatible register types in register list range"
          RH ia -> case rb of
            R _ -> unexpected "incompatible register types in register list range"
            RH ib -> if ia >= ib 
              then unexpected "register range must be strictly increasing"
              else return (map (RH) [ia..ib])
      ) 
      <|> 
      (parseRegister >>= \r -> return [r])
      <?>
      "Register list"

lbrack :: Parser ()
lbrack = (skip $ symbol "[") <?> "left square bracket"

rbrack :: Parser ()
rbrack = (skip $ symbol "]") <?> "right square bracket"

comma :: Parser ()
comma = (skip $ symbol ",") <?> "comma symbol"

argumentSeperator :: Parser ()
argumentSeperator = (skip $
  do
    many whitespace
    comma
    many whitespace)

  <?> "Argument seperator"

parseInstruction :: Parser (Instruction a)
parseInstruction = 
  try (do 
    caseInsensitiveString "mov"
    setFlags <- parseSetFlags
    cond <- parseCondition
    many1 whitespace
    rn  <- parseRegister
    argumentSeperator
    val <- parseValOrReg
    return $ IMOV setFlags cond rn val)
  <|>
  try (do
    binOp <- parseBinOp
    setFlags <- parseSetFlags
    cond <- parseCondition
    many1 whitespace
    rn <- parseRegister
    argumentSeperator
    rm <- parseRegister
    argumentSeperator
    val <- parseValOrReg
    return $ IBINOP setFlags cond binOp rn rm val)
  <|>
  try (do
    caseInsensitiveString "cmp"
    cond <- parseCondition
    many1 whitespace
    rn <- parseRegister
    argumentSeperator
    val <- parseValOrReg
    return $ ICMP cond rn val)
  <|>
  try (do
    caseInsensitiveString "ldr"
    cond <- parseCondition
    many1 whitespace
    rn <- parseGeneralRegister -- loading into PC not suported
    argumentSeperator
    addr <- parseAddrOrReg
    return $ ILDR cond rn addr)
  <|>
  try (do
    caseInsensitiveString "str"
    cond <- parseCondition
    many1 whitespace
    rn <- parseRegister
    argumentSeperator
    addr <- parseAddrOrReg
    return $ ISTR cond rn addr)
  <|>
  try (do
    caseInsensitiveString "b"
    cond <- parseCondition
    many1 whitespace
    target <- parseBranchTarget
    return $ IB cond target)
  <|>
  try (do
    caseInsensitiveString "push"
    cond <- parseCondition
    many1 whitespace
    regs <- parseRegisterList
    return $ IPUSH cond regs)
  <|>
  try (do
    caseInsensitiveString "pop"
    cond <- parseCondition
    many1 whitespace
    regs <- parseRegisterList
    return $ IPOP cond regs)
  <|>
  (do
    caseInsensitiveString "out"
    cond <- parseCondition
    many1 whitespace
    val <- parseValOrReg
    return $ IOUT cond val)
  <?> "Instruction"

parseLabel :: Parser Label
parseLabel = parseLabelIdentifier >>= \id -> char ':' >> return id <?> "Label"

parseLine :: Parser Line
parseLine = 
  try ( many whitespace >> parseInstruction >>= return . LInstruction) <|>
  (parseLabel >>= return . LLabel) <?> "Line"

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

parseTinyARM :: String -> Either ParseError Program
parseTinyARM input = (parse parseFile "(unknown)" input) 
