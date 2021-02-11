module Language.While (executeWhile) where

import Language.While.Parser
import Language.While.Interpreter


executeWhile :: String -> Int -> Either String String
executeWhile code stepCount = 
    case parseProgram code of
        Left error -> Left $ "Parse error: " ++ (show error)
        Right prog -> case stepsC prog initstate of
            Left error -> Left $ "Exception: " ++ error
            Right state -> Right $ showIState state 


-- parseWhile = parseProgram



exampleProgram = "\
    \ b = 0; \n\
    \ a = input(b); \n\
    \ output a ;"