{-#LANGUAGE GADTs #-}
module LatticeViz.Parser (parseLatticeNotation) where
import LatticeViz.Types


import Data.Maybe (catMaybes)
import Prelude hiding (EQ,LT,GT)
import Text.Parsec hiding (Line)
import Text.Parsec.String
import Data.Char (isSpace,digitToInt,isAlphaNum,toLower,toUpper)
import Data.Word
import qualified Data.Map as M

---

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

---


skip :: Parser a -> Parser ()
skip p = p >> return ()

isAnySpace :: Char -> Bool
isAnySpace c = isSpace c

anySpace :: Parser ()
anySpace = skip (satisfy isAnySpace) <?> "anyspace"

isLineSpace :: Char -> Bool
isLineSpace c = isSpace c && c /= '\n' && c /= '\r'

lineSpace :: Parser ()
lineSpace = skip (satisfy isLineSpace) <?> "linespace"

optAnySpace :: Parser ()
optAnySpace = skip $ many anySpace

optLineSpace :: Parser ()
optLineSpace = skip $ many lineSpace

quotedText :: Parser String
quotedText = do
    char '"'
    s <- manyTill anyChar ( lookAhead $ try $ noneOf "\\" >> char '"')
    c <- anyChar
    char '"'
    return (s ++ [c])

isNonReservedChar :: Char -> Bool
isNonReservedChar c = not $ elem c "{}[],<="

-- works
elmName :: Parser String
elmName = try quotedText <|> 
          many1 (satisfy (\c -> isNonReservedChar c && not (isAnySpace c)))


relationChain :: Parser [String]
relationChain = elmName `sepBy` try (optAnySpace >> char '<' >> optAnySpace)

parsePoset :: Parser Lattice
parsePoset = do
    char '['
    relationChains <- relationChain `sepBy` try (optAnySpace >> char ',' >> optAnySpace)
    char ']'
    let relationPairs = concatMap pairUp relationChains
        relations = [Pair a b | (a,b) <- relationPairs]
    return $ LatPoset relations
    where 
        pairUp :: [a] -> [(a,a)]
        pairUp l = zip l $ tail l

-- works
parseSet :: Parser Lattice
parseSet = do
    char '{'
    names <- elmName `sepBy` try (optAnySpace >> char ',' >> optAnySpace)
    char '}'
    return $ LatPowerset names

parseMapLattice :: Parser Lattice
parseMapLattice = do
    set <- (parseSet <|> parseLatticeRef)
    optAnySpace >> string "->" >> optAnySpace
    lat <- parseLattice
    return $ LatMap_ set lat

parseProductLattice :: Parser Lattice
parseProductLattice = do
    lat1 <- parseNonLeftRecursiveLattice
    optAnySpace >> char 'X' >> optAnySpace
    lat2 <- parseLattice
    return $ LatProduct lat1 lat2

parseLatticeRef :: Parser Lattice
parseLatticeRef = elmName >>= (return . LatRef_)

parseNonLeftRecursiveLattice :: Parser Lattice
parseNonLeftRecursiveLattice = 
    try parseMapLattice <|>
    try parsePoset <|>
    try parseSet <|>
    parseLatticeRef

parseLattice :: Parser Lattice
parseLattice = 
    try parseProductLattice <|>
    parseNonLeftRecursiveLattice

parseNamedLattice :: Parser NamedLattice
parseNamedLattice = do
    name <- elmName
    optLineSpace >> char '=' >> optAnySpace
    lattice <- parseLattice
    return $ NamedLattice name lattice

parseNamedLattices :: Parser [NamedLattice]
parseNamedLattices = 
    parseNamedLattice `endBy` (optLineSpace >> char '\n' >> optAnySpace)

parseUnnamedLattices :: Parser [Lattice]
parseUnnamedLattices = 
    parseLattice `endBy` (optLineSpace >> char '\n' >> optAnySpace)

parseFile :: Parser [NamedLattice]
parseFile = do
    namedLattices <- parseNamedLattices
    unnamedLattices <- parseUnnamedLattices
    
    if null unnamedLattices
        then return namedLattices
        else return $ namedLattices ++ [NamedLattice "$$MAIN$$" $ last unnamedLattices]

parseLatticeTopLevel :: String -> Either ParseError [NamedLattice]
parseLatticeTopLevel input = parse parseFile "(unknown)" (input++"\n")

parseLatticeNotation :: String -> Either String Lattice
parseLatticeNotation input = 
    case parseLatticeTopLevel input of
        Left err -> Left $ show err
        Right namedLattices -> parseResultToSingleLattice namedLattices

parseResultToSingleLattice :: [NamedLattice] -> Either String Lattice
parseResultToSingleLattice [] = Left "No lattices provided"
parseResultToSingleLattice namedLattices = 
    let 
        NamedLattice _ target = last namedLattices
        latNameMap = M.fromList $ map (\(NamedLattice name lat) -> (name,lat)) namedLattices
    in
        expandTarget target latNameMap
    where
        expandTarget :: Lattice -> M.Map String Lattice -> Either String Lattice
        expandTarget (LatRef_ name) m = 
            case m M.!? name of
                Nothing -> Left $ "Unknown lattice name \""++name++"\""
                Just l -> Right l
        
        expandTarget (LatMap_ (LatRef_ name) other) m = 
            case expandTarget (LatRef_ name) m of
                Left err -> Left err
                Right s@(LatPowerset _) -> expandTarget (LatMap_ s other) m
                Right _ -> Left "Expected set as left-side of map lattice"
        expandTarget (LatMap_ (LatPowerset s) other) m =
            case expandTarget other m of
                Left err -> Left err
                Right l -> Right $ LatMap s l
        expandTarget (LatProduct l1 l2) m = 
            case expandTarget l1 m of
                Left err -> Left err
                Right l1e -> case expandTarget l2 m of
                    Left err -> Left err
                    Right l2e -> Right $ LatProduct l1e l2e
        expandTarget l m = Right l
