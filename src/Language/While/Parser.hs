module Language.While.Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- import qualified Text.Parsec as P  -- for P.parserTrace

import Language.While.Language hiding (Cmd)

type Cmd = ACmd SourcePos

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = (alphaNum <|> oneOf "'_")
           , Token.reservedNames   = [ "if"
                                     , "else"
                                     , "while"
                                     , "skip"
                                     , "alloc"
                                     , "free"
                                     , "copy_to_adv"
                                     , "output"
                                     , "leak"
                                     , "input"
                                     , "forget"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "=", "=="
                                     , "<", ">", "&&", "||", "~", "<=", ">="
                                     , "++"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
comma = Token.comma lexer
braces = Token.braces lexer
strlit = Token.stringLiteral lexer

program :: Parser Cmd
program = do
  whiteSpace
  cmd <- commands
  eof
  return cmd

command :: Parser Cmd
command = block <|> annocmd

block :: Parser Cmd
block = braces commands

commands :: Parser Cmd
commands = do
  pos <- getPosition
  cmds <- many1 annocmd -- command'
  return $ if length cmds == 1 then head cmds else CANNO pos $ SEQ cmds

csemi :: Parser Cmd -> Parser Cmd
csemi cmd = cmd >>= \v -> semi >> return v

annocmd :: Parser Cmd
annocmd = do
  pos <- getPosition
  cmd <- command'
  return $ CANNO pos cmd

command' :: Parser Cmd
command' = ifCmd
           <|> (try $ csemi ptrassignCmd)
           <|> (try $ csemi allocCmd)
           <|> (try $ csemi inputCmd)
           <|> (csemi assignCmd)
           <|> (csemi freeCmd)
           <|> (csemi outputCmd)
--           <|> (csemi forgetCmd)
           <|> (csemi leakCmd)
           <|> whileCmd
           <|> (csemi skipCmd)
--           <|> block


ifCmd :: Parser Cmd
ifCmd = do
  reserved "if"
  cond  <- expression
  cmd1 <- command
  elsepos <- getPosition
  cmd2 <- option Nothing $
          do reserved "else"
             fmap Just command
  return $ IF cond cmd1 cmd2

whileCmd :: Parser Cmd
whileCmd = do
  reserved "while"
  cond <- expression
  cmd <- command
  return $ WHILE cond cmd

skipCmd :: Parser Cmd
skipCmd = reserved "skip" >> return SKIP

assignCmd :: Parser Cmd
assignCmd = do
  var  <- identifier
  reservedOp "="
  expr <- expression
  return $ ASGN var expr

ptrassignCmd :: Parser Cmd
ptrassignCmd = do
  reservedOp "*"
  ptr <- expression
  reservedOp "="
  expr <- expression
  return $ PTRASGN ptr expr

allocCmd :: Parser Cmd
allocCmd = do
  var <- identifier
  reservedOp "="
  reserved "alloc"
  expr <- parens expression
  return $ ALLOC var expr

inputCmd :: Parser Cmd
inputCmd = do
  var <- identifier
  reservedOp "="
  reserved "input"
  var' <- parens identifier
  return $ INPUT var var'

exprPair :: Parser (Expr,Expr)
exprPair = parens $ do
  expr1 <- expression
  expr2 <- option (LIT 0) $
           do comma
              expression
  return (expr1,expr2)

freeCmd :: Parser Cmd
freeCmd = do
  reserved "free"
  (expr1,expr2) <- exprPair
  return $ FREE expr1 expr2

outputCmd :: Parser Cmd
outputCmd = do
  reserved "output"
  expr <- expression
  return $ OUTPUT expr

-- forgetCmd :: Parser Cmd
-- forgetCmd = do
--   reserved "forget"
--   (expr1,expr2) <- exprPair
--   return $ FORGET expr1 expr2

leakCmd :: Parser Cmd
leakCmd = do
  reserved "copy_to_adv"
  expr <- expression
  return $ LEAK expr

expression :: Parser Expr
expression = do
  expr <- buildExpressionParser operators terms
  return expr
    where
      mkfix :: BinOp -> Expr -> Expr -> Expr
      mkfix bop e1 e2 = OP e1 bop e2
      operators = [ [ Prefix (reservedOp "*" >> return DEREF) ]
                  , [ Infix  (reservedOp "*"   >> return (mkfix Mult )) AssocLeft
                    , Infix  (reservedOp "/"   >> return (mkfix Div  )) AssocLeft
                    ]
                  , [ Infix  (reservedOp "+"   >> return (mkfix Plus )) AssocLeft
                    , Infix  (reservedOp "-"   >> return (mkfix Minus)) AssocLeft
                    , Infix  (reservedOp "++"  >> return (mkfix Conc )) AssocLeft
                    ]
                  ,
                    [ Infix  (reservedOp "=="  >> return (mkfix Eq   )) AssocNone 
                    , Infix  (reservedOp "<"   >> return (mkfix Lt   )) AssocNone
                    , Infix  (reservedOp "<="  >> return (mkfix LE   )) AssocNone
                    , Infix  (reservedOp ">"   >> return (mkfix Gt   )) AssocNone
                    , Infix  (reservedOp ">="  >> return (mkfix GE   )) AssocNone
                    ]
                  ]

castexpr :: Parser Expr
castexpr = do
  reserved "cast"
  expr <- parens expression
  return $ CAST expr

derefexpr :: Parser Expr
derefexpr = do
  reservedOp "*"
  expr <- expression
  return $ DEREF expr

terms :: Parser Expr
terms =
  parens expression
  <|> derefexpr
  <|> castexpr
  <|> liftM VAR identifier
  <|> liftM (LIT . fromInteger) integer
  <|> liftM STR strlit

--------------------------------------------------

parseProgramFile :: String -> IO (Either ParseError Cmd)
parseProgramFile fname = parseFromFile program fname

parseProgram :: String -> Either ParseError Cmd
parseProgram prg = parse program "<input string>" prg
