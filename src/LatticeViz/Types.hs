module LatticeViz.Types where

import Data.UUID

data Relation = Pair String String deriving Show

type Poset = [ Relation ]
type Set = [ String ]

data Lattice 
    = LatPoset Poset
    | LatPowerset Set
    | LatMap Set Lattice
    | LatProduct Lattice Lattice
    | LatRef_ String -- parser used reference to a named lattice
    | LatMap_ Lattice Lattice -- parser used LatMap to allow for LatRef as set
    deriving (Show)

data NamedLattice = NamedLattice String Lattice deriving (Show)


data Graph = Graph {
        names :: [(UUID, String)],
        relations :: [(UUID, UUID)]
    }