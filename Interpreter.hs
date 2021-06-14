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
interpretExpress gf (Comb (A(Ident funcName):args)) = callFunction gf funcName args
interpretExpress gf (Comb [ex]) = interpretExpress gf ex 
interpretExpress _ (A (Value v)) = Value v 
interpretExpress gf (A (Ident funcName)) = callFunction gf funcName []    
interpretExpress _ (A(Error err)) = Error err 
interpretExpress _ _ = Error UnknownError  

callFunction :: GlobalFuncs -> Ident -> [Express] -> Atom 
callFunction funcs funcName args = 
    case funcs !? funcName of 
        Nothing -> preDefinedFuncs funcName (map (interpretExpress funcs) args) 
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

preDefinedFuncs :: Ident -> [Atom] -> Atom   
preDefinedFuncs ident [a1, a2] =
    (\case 
        Nothing -> Error UnboundVariable 
        Just f -> applyTwoArgFuncToAtom f a1 a2)
    $ case ident of  
        "+" -> Just (+)
        "-" -> Just (-)
        "*" -> Just (*)
        "/" -> Just div
        _ -> Nothing  
preDefinedFuncs _ _ = Error UnboundVariable
     
applyTwoArgFuncToAtom ::  (Int -> Int -> Int) -> Atom -> Atom -> Atom 
applyTwoArgFuncToAtom _ (Error e) _ = Error e
applyTwoArgFuncToAtom _ _ (Error e) = Error e
applyTwoArgFuncToAtom f (Value v1) (Value v2) = Value $ f v1 v2
applyTwoArgFuncToAtom _ _ _ = Error ValueError 