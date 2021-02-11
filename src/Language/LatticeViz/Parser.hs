{-#LANGUAGE GADTs #-}
module Language.LatticeViz.Parser (parseLatticeLanguage) where
import Language.LatticeViz.Types
import Text.Parsec hiding (Line)
import Text.Parsec.String
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Either

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

optAnySpaces :: Parser ()
optAnySpaces = skip $ many anySpace

anySpaces :: Parser ()
anySpaces = skip $ many1 anySpace

optLineSpaces :: Parser ()
optLineSpaces = skip $ many lineSpace

lineSpaces :: Parser ()
lineSpaces = skip $ many1 lineSpace

anySpacesWithNewline :: Parser ()
anySpacesWithNewline = optLineSpaces >> char '\n' >> optAnySpaces

sepBy2 :: ParsecT s u m a1 -> ParsecT s u m a2 -> ParsecT s u m [a1]
sepBy2 p sep = do 
    x1 <- p
    x2 <- sep >> p
    xs <- many $ try (sep >> p)
    return (x1:x2:xs)

bracketParser :: Char -> Char -> Parser a -> Parser a
bracketParser l r p = do
    char l >> optAnySpaces
    v <- p
    optAnySpaces >> char r
    return v

seperatorParser :: String -> Parser ()
seperatorParser s = try (optAnySpaces >> string s >> optAnySpaces)

parseQuotedText :: Parser String
parseQuotedText = do
    char '"'
    s <- manyTill anyChar ( lookAhead $ try $ noneOf "\\" >> char '"')
    c <- anyChar
    char '"'
    return (s ++ [c])

parseVar :: Parser PVar
parseVar = try parseQuotedText <|> 
          many1 (satisfy (\c -> c `notElem` "(){}[],<=" && not (isAnySpace c)))

parseSet :: Parser PSet
parseSet = 
    bracketParser '{' '}'
        (parseVar `sepBy` seperatorParser ",")

parseRelation :: Parser PRelation
parseRelation = 
    try (parseVar `sepBy2` seperatorParser "<" >>= return . POrderedUnder) <|>
        (parseVar >>= return . PUnordered)

parsePoset :: Parser PPoset
parsePoset = 
    bracketParser '[' ']' 
        (parseRelation `sepBy` seperatorParser "," >>= return . PPoset)

parseParen :: Parser PParen
parseParen = 
    bracketParser '(' ')' 
        (parseExpression >>= return . PParen)

parseSubExpression :: Parser PSubExpression
parseSubExpression = try (parseParen >>= return . PSExprParen) <|>
             try (parsePoset >>= return . PSExprPoset) <|>
             try (parseSet >>= return . PSExprSet) <|>
                 (parseVar >>= return . PSExprVar)


unaryFunction :: String -> (PExpression -> PFunction) -> Parser PFunction
unaryFunction prefixName function = string prefixName >> bracketParser '(' ')' parseExpression >>= return . function

binaryFunction :: String -> (PSubExpression -> PExpression -> PFunction) -> Parser PFunction
binaryFunction infixName function = do
    lhs <- parseSubExpression
    case lhs of
        PSExprVar _ -> anySpaces
        _         -> optAnySpaces
    string infixName
    optAnySpaces
    rhs <- parseExpression
    return $ function lhs rhs


parseFunMap :: Parser PFunction
parseFunMap = binaryFunction "->" PFunMap
parseFunProduct :: Parser PFunction
parseFunProduct = binaryFunction "X" PFunProduct
parseFunSmash :: Parser PFunction
parseFunSmash = binaryFunction "^" PFunSmash
parseFunLift :: Parser PFunction
parseFunLift = unaryFunction "L" PFunLift
parseFunPowerset :: Parser PFunction
parseFunPowerset = unaryFunction "P" PFunPowerset

parseFunction :: Parser PFunction
parseFunction = try parseFunPowerset <|>
                try parseFunLift <|>
                try parseFunSmash <|>
                try parseFunProduct <|>
                    parseFunMap


parseExpression :: Parser PExpression
parseExpression = try (parseFunction >>= return . PExprFunction) <|>
                      (parseSubExpression >>= return . PExprVal)

parseAssignment :: Parser PAssignment
parseAssignment = do 
    x <- parseVar
    optLineSpaces
    char '='
    optLineSpaces
    exp <- parseExpression
    return $ PAssignment x exp
    

parseStatement :: Parser PStatement
parseStatement = try (parseAssignment >>= return . PStmAssignment) <|> 
                     (parseExpression >>= return . PStmExpression)

parseStatements :: Parser PStatements
parseStatements = do
    optAnySpaces
    stms <- parseStatement `endBy` anySpacesWithNewline
    optAnySpaces
    return stms


parseCommentLine :: Parser ()
parseCommentLine = skip $ string "//" >> manyTill anyChar (lookAhead $ char '\n')
parseCommentBlock :: Parser ()
parseCommentBlock = skip $ string "/*" >> manyTill anyChar (string "*/")

parseComments :: Parser String
parseComments = 
    let p = do {
        notFollowedBy $ string "//";
        -- notFollowedBy $ string "/*";
        anyChar;
    }
    in many p `sepBy` parseCommentLine >>= return . concat

removeComments :: String -> String
removeComments input = fromRight input $ parse parseComments "" input

----------------------- ^ -----------------------
-------------------- PARSERS --------------------
-------------------------------------------------

-------------------------------------------------
-------------- PARSE TREE REDUCERS --------------
----------------------- v -----------------------

parseLatticeLanguage :: String -> Either String Lattice
parseLatticeLanguage input = 
    either (Left . show) computeLattice
        $ parse parseStatements "(Unknown)" (removeComments input++"\n")

computeLattice :: PStatements -> Either String Lattice
computeLattice stms = 
    case computeStms stms M.empty of
        Left err -> Left err
        Right (PVSet _) -> Left "Last statement cannot be a set ( Maybe you meant P({...})) )"
        Right (PVLattice pl) -> Right $ parserLatticeToLattice pl

parserLatticeToLattice :: PLattice -> Lattice
parserLatticeToLattice (PLatPowerset s) = LatPowerset s
parserLatticeToLattice (PLatLift pl) = LatLift $ parserLatticeToLattice pl
parserLatticeToLattice (PLatProduct pl1 pl2) = 
    LatProduct (parserLatticeToLattice pl1) (parserLatticeToLattice pl2)
parserLatticeToLattice (PLatSmash pl1 pl2) = 
    LatSmashProduct (parserLatticeToLattice pl1) (parserLatticeToLattice pl2)
parserLatticeToLattice (PLatMap s pl) = LatMap s (parserLatticeToLattice pl)
parserLatticeToLattice (PLatPoset poset) = LatPoset poset


computeStms :: PStatements -> M.Map PVar PValue -> Either String PValue
computeStms [] _ = Left "No lattice provided"
computeStms [PStmExpression exp] env = computeExpr exp env
computeStms [PStmAssignment (PAssignment _ exp)] env = computeExpr exp env
computeStms ((PStmExpression _):stms) env = computeStms stms env
computeStms ((PStmAssignment (PAssignment x exp)):stms) env = 
    case env M.!? x of
        Just _  -> Left ("var " ++ x ++ " is assigned to more than once")
        Nothing -> 
            case computeExpr exp env of
                err@(Left _) -> err
                Right l -> computeStms stms (M.insert x l env)

computeExpr :: PExpression -> M.Map PVar PValue -> Either String PValue
computeExpr (PExprFunction fun) env = computeFunction fun env
computeExpr (PExprVal val) env = computeSubExpression val env

computeFunction :: PFunction -> M.Map PVar PValue -> Either String PValue
computeFunction (PFunPowerset expr) env = 
    either 
        Left 
        (\v ->
            case v of 
                PVSet s -> Right $ PVLattice $ PLatPowerset s
                PVLattice (PLatPowerset s) -> Right $ PVLattice $ PLatPowerset $ map toSetNotation (powerset s)
                PVLattice (PLatPoset (Poset s _)) -> Right $ PVLattice $ PLatPowerset s
                _ -> Left "Left side of Map must be a set"
        ) $ computeExpr expr env
    where
        toSetNotation :: [String] -> String
        toSetNotation [] = "∅"
        toSetNotation l = "{"++ foldr1 (\a b -> a ++ "," ++ b) l ++ "}"

        powerset :: [a] -> [[a]]
        powerset [] = [[]]
        powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs
computeFunction (PFunLift expr) env = 
    either
        (Left)
        (\v -> 
            case v of
                PVLattice l -> Right $ PVLattice $ PLatLift l
                _           -> Left "Lift must be applied to a lattice")
        (computeExpr expr env)
computeFunction (PFunProduct sexp exp) env = 
    let e1 = computeSubExpression sexp env
        e2 = computeExpr exp env
    in 
        case (e1,e2) of 
            (Left err, _) -> Left err
            (_, Left err) -> Left err
            (Right v1, Right v2) -> computeProduct v1 v2
    where
        computeProduct :: PValue -> PValue -> Either String PValue
        computeProduct (PVSet s1) (PVSet s2) = Right $ PVSet ["("++a++","++b++")" | a <- s1, b <- s2]
        computeProduct (PVLattice l1) (PVLattice l2) = Right $ PVLattice $ PLatProduct l1 l2
        computeProduct _ _  = Left "Can't find product of set with lattice.\n(Maybe you meant product of powerset and lattice)"
computeFunction (PFunSmash sexp exp) env = 
    let e1 = computeSubExpression sexp env
        e2 = computeExpr exp env
    in 
        case (e1,e2) of 
            (Left err, _) -> Left err
            (_, Left err) -> Left err
            (Right (PVLattice l1), Right (PVLattice l2)) -> Right $ PVLattice $ PLatSmash l1 l2
            _ -> Left "Cannot calculate smash product of non-lattice"
computeFunction (PFunMap sexp exp) env = 
    let e1 = computeSubExpression sexp env
        e2 = computeExpr exp env
    in 
        case (e1,e2) of 
            (Left err, _) -> Left err
            (_, Left err) -> Left err
            (Right (PVSet s), Right (PVLattice l)) -> Right $ PVLattice $ PLatMap s l
            (Right _, Right (PVLattice _)) -> Left "Left side of \"->\" must be a set"
            (Right (PVSet _), Right _) -> Left "Right side of \"->\" must be a lattice\n(Maybe you meant to take the powerset of the right hand side)"
            _ -> Left "Unknown error in computation of map lattice"



computeSubExpression :: PSubExpression -> M.Map PVar PValue -> Either String PValue
computeSubExpression (PSExprParen (PParen expr)) env = computeExpr expr env
computeSubExpression (PSExprPoset pposet) _ = 
    case computePoset pposet of Left err -> Left err; Right p -> Right $ PVLattice $ PLatPoset p
computeSubExpression (PSExprSet s) _ = if nub s == s 
    then Right $ PVSet s 
    else Left  $ "Set has multiple instances of same element: " ++ show s
computeSubExpression (PSExprVar x) env = 
    case env M.!? x of
        Just l -> Right l
        Nothing -> Left $ "Use of undefined variable " ++ x

computePoset :: PPoset -> Either String Poset
computePoset (PPoset rs) = 
    let
        set = nub $ extractSymbols rs
        relationChains = extractOrdered rs
        relationPairs = pairUp relationChains
    in 
        Right $ Poset set relationPairs
        -- TODO: missing check of validity
    where
        extractSymbols :: [PRelation] -> Set
        extractSymbols [] = []
        extractSymbols ((PUnordered x):rs) = x:(extractSymbols rs)
        extractSymbols ((POrderedUnder chain):rs) = chain++(extractSymbols rs)

        extractOrdered :: [PRelation] -> [[PVar]]
        extractOrdered [] = []
        extractOrdered ((POrderedUnder chain):rs) = chain:(extractOrdered rs)
        extractOrdered (x:rs) = extractOrdered rs
            
        pairUp :: [[PVar]] -> [Relation]
        pairUp chains = [Pair a b | (a,b) <- concatMap pairUp_ chains]
        pairUp_ :: [a] -> [(a,a)]
        pairUp_ l = zip l $ tail l