{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (interpretProgram) where 

import Types
import Parser (parseProgram)
import Data.Map (empty, insert, (!?))

interpretProgram :: String -> IO () 
interpretProgram input = 
        case parseProgram input of  
            Just program -> runProgram initFuncs program
            Nothing -> putStr "Syntax Error\n"
    
runProgram :: GlobalFuncs -> Program -> IO ()
runProgram _ [] =  return ()
runProgram funcs (a:as) = 
    completeAction funcs a >>= 
        \newFuncs -> runProgram newFuncs as  

completeAction :: GlobalFuncs -> Action -> IO GlobalFuncs 
completeAction gf (Display ex) = 
    putStr ((++ "\n") . show $ interpretExpress gf ex) >> return gf 
completeAction gf (Definition funcName perameters body) =
    return $ insert funcName (perameters, body) gf 

interpretExpress :: GlobalFuncs -> Express -> Atom  
interpretExpress _ (A (Value v)) = Value v 
interpretExpress gf (A (Ident funcName)) = callFunction gf funcName []   
interpretExpress gf (Comb (A(Operator op):[ex1, ex2])) = applyOperation gf op ex1 ex2
interpretExpress gf (Comb (A(Ident funcName):args)) = callFunction gf funcName args 
interpretExpress _ _ = Error SyntaxError

applyOperation :: GlobalFuncs -> Char -> Express  -> Express  -> Atom 
applyOperation gf op ex1 ex2 =
    applyOpToAtoms
        (case op of 
            '+' -> (+) 
            '-' -> (-)
            '*' -> (*)
            '/' -> div
            _ -> (\_ _ -> 999) -- Note: should not occur, just doing this to satisfy compiler for now. Maybe make op type 
            )
        (interpretExpress gf ex1)
        (interpretExpress gf ex2)

applyOpToAtoms :: (Int -> Int -> Int) -> Atom -> Atom -> Atom 
applyOpToAtoms f (Value v1) (Value v2) = Value $ f v1 v2
applyOpToAtoms _ (Error e) _ = Error e 
applyOpToAtoms _ _ (Error e) = Error e 
applyOpToAtoms _ _ _ = Error InvalidArguments

callFunction :: GlobalFuncs -> Ident -> [Express] -> Atom 
callFunction funcs funcName args = 
    case funcs !? funcName of 
        Nothing -> Error UnboundVariable
        Just (perameters, express) ->
            case bindArguments funcs perameters args of
                Nothing -> Error InvalidArguments 
                Just scopedFuncs -> interpretExpress scopedFuncs express  

bindArguments :: GlobalFuncs -> [Ident] -> [Express] -> Maybe GlobalFuncs 
bindArguments gf perameters args = 
    if length perameters /= length args 
        then Nothing 
    else 
        Just $ foldl 
            (\funcs (name, value) -> insert name ([], value) funcs) 
            gf 
            (zip perameters args) 

initFuncs :: GlobalFuncs 
initFuncs = empty 