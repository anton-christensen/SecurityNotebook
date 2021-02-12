{-# LANGUAGE DeriveFunctor #-}
-- -*- haskell -*- ---------------------------------------------------
--
-- Simple While Language
--
----------------------------------------------------------------------
module Language.While.Language where

import qualified Data.Set as S
import Text.Parsec

type Var = String
type Nat = Int

data BinOp = Plus | Minus | Mult | Div | Eq | Lt | LE | Gt | GE
               deriving (Eq,Show)

data AExpr a = LIT Nat
             | VAR Var
             | OP Expr BinOp Expr
             | DEREF Expr
             | CAST Expr
             | EANNO a (AExpr a)
             deriving (Eq,Show,Functor)

data ACmd a = ASGN Var Expr
            | PTRASGN Expr Expr
            | ALLOC Var Expr
            | FREE Expr Expr
            | SEQ [ACmd a]
            | IF Expr (ACmd a) (Maybe (ACmd a))
            | WHILE Expr (ACmd a)
            | LEAK Expr
            | OUTPUT Expr
            | SKIP
            | INPUT Var Var
            | CANNO a (ACmd a)
            deriving (Eq,Show,Functor)

type Expr = AExpr ()
type Cmd = ACmd ()
type CmdWithPos = ACmd SourcePos

forgetExprA :: AExpr a -> AExpr ()
forgetExprA expra = fmap (\_ -> ()) expra

forgetCmdA :: ACmd a -> ACmd ()
forgetCmdA cmda = fmap (\_ -> ()) cmda

printSrcPos :: SourcePos -> String
printSrcPos pos = (show $ sourceLine pos) ++ ":"  ++ (show $ sourceColumn pos)


extractVarsFromExpr :: AExpr a -> S.Set Var
extractVarsFromExpr (LIT _) = S.empty
extractVarsFromExpr (VAR v) = S.singleton v
extractVarsFromExpr (OP e1 _ e2) = S.union (extractVarsFromExpr e1) (extractVarsFromExpr e2)
extractVarsFromExpr (DEREF e) = extractVarsFromExpr e
extractVarsFromExpr (CAST e) = extractVarsFromExpr e
extractVarsFromExpr (EANNO _ e) = extractVarsFromExpr e

prettyPrintInstr :: ACmd a -> String
prettyPrintInstr cmd = concat $ ppCmd cmd
  where
    ppCmd :: ACmd a -> [String]
    ppCmd (ASGN v e) = [ppV v, " = ", ppE e]
    ppCmd (PTRASGN e1 e2) = ["*(", ppE e1, ") = ", ppE e2]
    ppCmd (ALLOC v e) = [ppV v, " = alloc(", ppE e, ")"]
    ppCmd (FREE e1 e2) = ["free (", ppE e1, ", ", ppE e1, ")"]
    ppCmd (SEQ cmds) = ["{ ... }"]
    ppCmd (IF e _ _) = ["if ", ppE e]
    ppCmd (WHILE e _) = ["while ", ppE e]
    ppCmd (LEAK e) = ["copy_to_adv ", ppE e]
    ppCmd (OUTPUT e) = ["output ", ppE e]
    ppCmd (SKIP) = ["skip"]
    ppCmd (INPUT v1 v2) = [ppV v1, " = input(", ppV v2, ")"]
    ppCmd (CANNO _ _) = ["?"]

    ppV :: Var -> String
    ppV v = v

    ppE :: Expr -> String
    ppE (LIT n) = show n
    ppE (VAR v) = ppV v
    ppE (OP e1 op e2) = concat [ppE e1, ppOP op, ppE e2]
    ppE (DEREF e) = "*(" ++ (ppE e) ++ ")"
    ppE (CAST e) = "cast(" ++ (ppE e) ++ ")"
    ppE (EANNO _ _ ) = "?"

    ppOP :: BinOp -> String
    ppOP Plus = " + "
    ppOP Minus = " - "
    ppOP Mult = " * "
    ppOP Div = " / "
    ppOP Eq = " == "
    ppOP Lt = " < "
    ppOP LE = " <= "
    ppOP Gt = " > "
    ppOP GE = " >= "