{-# LANGUAGE DeriveFunctor #-}
-- -*- haskell -*- ---------------------------------------------------
--
-- Simple While Language
--
----------------------------------------------------------------------
module Language.While.Language where

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

forgetExprA :: AExpr a -> AExpr ()
forgetExprA expra = fmap (\_ -> ()) expra

forgetCmdA :: ACmd a -> ACmd ()
forgetCmdA cmda = fmap (\_ -> ()) cmda
