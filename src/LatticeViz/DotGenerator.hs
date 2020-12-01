module LatticeViz.DotGenerator where

import Data.UUID
import LatticeViz.Types
import Data.UUID.V4
import Data.Map ((!), fromList, toList)
-- import Data.Monoid
import Data.List 


printGraph :: Graph -> String
printGraph g = unlines [
    "digraph G {",
    "  graph [ pad=\"1\", ranksep=\"1\", nodesep=\"1\", splines=\"line\" ];",
    "  node [ shape=\"none\", fixedsize=true, width=1, height=0.5 ];",
    "  edge [ arrowhead=none ];",
    "  "] ++ printNames (names g) ++ printRelations (relations g) ++ "\n}"
    where
        printNames :: [(UUID, String)] -> String
        printNames names = unlines $ map (\(id, name) -> "    " ++ showid id ++ "[label=\"" ++ name ++ "\"]") names
        printRelations :: [(UUID, UUID)] -> String
        printRelations relations = unlines $ map (\(from, to) -> "    " ++ showid from ++ " -> " ++ showid to ++ ";") relations 
        showid :: UUID -> String
        showid id = "ID" ++  filter (/= '-') (show id)

latticeToGraph :: Lattice -> IO Graph
latticeToGraph (LatPowerset s) = do
    idmap <- mapM (\s -> nextRandom >>= (\id -> return (id,s)) ) (powerset s)
    let 
        names = map (\(id,s) -> (id,toSetNotation s)) idmap
        relations = [(id1,id2) | (id1,s1) <- idmap, (id2,s2) <- idmap, length (s1 \\ s2) == 1 && length s1 == length s2 + 1]
    return Graph {names=names, relations=relations}

latticeToGraph (LatPoset relations) = do
    let 
        symbols = nub $ [x | Pair x _ <- relations] ++ [x | Pair _ x <- relations]
    names_ <- mapM (\s -> nextRandom >>= (\id -> return (id,s)) ) symbols
    let
        nameMap = fromList $ map (\(a,b) -> (b,a)) names_
        relations_ = map (\(Pair s1 s2) -> (nameMap ! s2, nameMap ! s1)) relations
    return Graph {names=names_, relations=relations_}

latticeToGraph (LatProduct lat1 lat2) = do
    graph1 <- latticeToGraph lat1
    graph2 <- latticeToGraph lat2
    nameMap <- pairOfNames (names graph1) (names graph2) >>= return . fromList
    let 
        relations_ = nub $ concat $
                     [ [(fst $ nameMap ! (a,x), fst $ nameMap ! (a,y)),
                        (fst $ nameMap ! (b,x), fst $ nameMap ! (b,y)),
                        (fst $ nameMap ! (a,x), fst $ nameMap ! (b,x)),
                        (fst $ nameMap ! (a,y), fst $ nameMap ! (b,y))] | (a,b) <- relations graph1, (x,y) <- relations graph2] 

    return Graph {names=map ((\(id,name) -> (id,"("++name++")")) . snd) $ toList nameMap, relations=relations_}
latticeToGraph (LatMap s lat) = do
    graph <- latticeToGraph lat
    let graphs = [return graph {names=[(id,elm++"↦"++name) | (id,name) <- names graph]} | elm <- s]

    combinedGraph <- combinePairwise graphs
    return combinedGraph {names=[(id,"["++name++"]") | (id,name) <- names combinedGraph]}

        where

            combinePairwise :: [IO Graph] -> IO Graph
            combinePairwise []         = return Graph{names=[],relations=[]}
            combinePairwise [x]        = x
            combinePairwise (x1:x2:xs) = do
                graph1 <- x1
                graph2 <- x2
                nameMap <- pairOfNames (names graph1) (names graph2) >>= return . fromList
                let 
                    relations_ = nub $ concat $
                                [ [(fst $ nameMap ! (a,x), fst $ nameMap ! (a,y)),
                                    (fst $ nameMap ! (b,x), fst $ nameMap ! (b,y)),
                                    (fst $ nameMap ! (a,x), fst $ nameMap ! (b,x)),
                                    (fst $ nameMap ! (a,y), fst $ nameMap ! (b,y))] | (a,b) <- relations graph1, (x,y) <- relations graph2] 
                    combined = return Graph {names=map snd $ toList nameMap, relations=relations_}
                combinePairwise (combined:xs)

pairOfNames :: [(UUID,String)] -> [(UUID,String)] -> IO [((UUID,UUID),(UUID,String))]
pairOfNames a b = mapM (
                    \(id1,id2,s) -> nextRandom >>= 
                        (\id -> return ((id1,id2),(id,s)))
                    )  [ (id1,id2,s1++", "++s2) | (id1,s1) <- a, (id2,s2) <- b ]

toMapNotation :: [String] -> Relation -> [String] -> String
toMapNotation fullSet relation members = 
    "["++ 
        intercalate "," (toMapNotationAux fullSet relation members) ++
    "]"
    where 
        toMapNotationAux :: [String] -> Relation -> [String] -> [String]
        toMapNotationAux [] _ _ = []
        toMapNotationAux (x:xs) (Pair less more) members = 
            (if x `elem` members
                then x++"↦"++less
                else x++"↦"++more
            ): toMapNotationAux xs (Pair less more) members

toSetNotation :: [String] -> String
toSetNotation [] = "∅" 
toSetNotation l = "{"++ foldr1 (\a b -> a ++ "," ++ b) l ++ "}"

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

