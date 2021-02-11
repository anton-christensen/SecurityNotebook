module Language.LatticeViz.Types where

import Data.UUID

data Relation = Pair String String deriving Show

data Poset = Poset Set [ Relation ] deriving Show
type Set = [ String ]

data Lattice 
    = LatPoset Poset
    | LatPowerset Set
    | LatMap Set Lattice
    | LatProduct Lattice Lattice
    | LatSmashProduct Lattice Lattice
    | LatLift Lattice
    deriving (Show)

data Graph = Graph {
        names :: [(UUID, String)],
        relations :: [(UUID, UUID)]
    }






type PStatements = [PStatement]
data PStatement  = PStmAssignment PAssignment
                 | PStmExpression PExpression
                 deriving (Show)

data PAssignment = PAssignment PVar PExpression deriving (Show)

data PExpression = PExprFunction PFunction
                 | PExprVal PSubExpression
                 deriving (Show)

data PParen = PParen PExpression deriving (Show)

data PFunction = PFunPowerset PExpression
               | PFunLift PExpression
               | PFunProduct PSubExpression PExpression
               | PFunSmash PSubExpression PExpression
               | PFunMap PSubExpression PExpression
               deriving (Show)

data PSubExpression = PSExprParen PParen
                    | PSExprPoset PPoset
                    | PSExprSet PSet
                    | PSExprVar PVar
                    deriving (Show)

type PSet = [PVar]

data PPoset     = PPoset PRelations deriving (Show)
type PRelations = [PRelation]
data PRelation  = POrderedUnder [PVar]
                | PUnordered PVar
                deriving (Show)

type PVar = String



-- intermediate types --
data PValue
    = PVSet Set
    | PVLattice PLattice

data PLattice 
    = PLatPowerset Set
    | PLatLift PLattice
    | PLatProduct PLattice PLattice
    | PLatSmash PLattice PLattice
    | PLatMap Set PLattice
    | PLatPoset Poset
