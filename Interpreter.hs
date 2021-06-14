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
interpretExpress gf (Comb (A(Operator op):[ex1, ex2])) = applyOperation gf op ex1 ex2
interpretExpress gf (Comb (A(Ident funcName):args)) = callFunction gf funcName args
interpretExpress gf (Comb [ex]) = interpretExpress gf ex 
interpretExpress _ (A (Value v)) = Value v 
interpretExpress gf (A (Ident funcName)) = callFunction gf funcName []    
interpretExpress _ (A(Error err)) = Error err 
interpretExpress _ ex = Error . Debug $ show ex

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
applyOpToAtoms _ _ _ = Error ValueError

callFunction :: GlobalFuncs -> Ident -> [Express] -> Atom 
callFunction funcs funcName args = 
    case funcs !? funcName of 
        Nothing -> Error UnboundVariable
        Just (perameters, express) ->
            interpretExpress funcs $ bindArguments perameters args express  

bindArguments :: [Ident] -> [Express] -> Express -> Express   
bindArguments [] [] express = express
bindArguments [] args _ = A . Error . InvalidArguments $ length args
bindArguments params [] _ = A . Error . InvalidArguments . negate $ length params
bindArguments (p:ps) (a:as) express = bindArguments ps as $ bindArgument p a express

bindArgument :: Ident -> Express -> Express -> Express 
bindArgument param arg (A (Ident ident)) 
    | ident == param  = arg 
    | otherwise = A $ Ident ident
bindArgument _ _ (A atom) = A atom 
bindArgument param arg (Comb es) = Comb $ map (bindArgument param arg) es  

initFuncs :: GlobalFuncs 
initFuncs = empty 