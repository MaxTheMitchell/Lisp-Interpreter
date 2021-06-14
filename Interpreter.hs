{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (interpretProgram) where 

import Types
    ( Error(ValueError, UnknownError, InvalidArguments,
            UnboundVariable),
      Ident,
      Atom(..),
      Express(..),
      Program,
      GlobalFuncs )
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
    interpretExpress funcs a >>= 
        \(newFuncs, _) -> runProgram newFuncs as  

interpretExpress :: GlobalFuncs -> Express -> IO (GlobalFuncs, Atom)  
interpretExpress gf (Comb (A(Ident funcName):args)) = callFunction gf funcName args
interpretExpress gf (Comb [ex]) = interpretExpress gf ex 
interpretExpress gf (A (Value v)) = return (gf, Value v) 
interpretExpress gf (A (Ident funcName)) = callFunction gf funcName []    
interpretExpress gf (A(Error err)) = return (gf, Error err) 
interpretExpress gf _ = return (gf, Error UnknownError)

callFunction :: GlobalFuncs -> Ident -> [Express] -> IO (GlobalFuncs, Atom) 
callFunction funcs funcName args = 
    case funcs !? funcName of 
        Nothing -> preDefinedFuncs funcs funcName args
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

preDefinedFuncs :: GlobalFuncs -> Ident -> [Express] -> IO (GlobalFuncs, Atom)
preDefinedFuncs gf "display" [ex] =
    interpretExpress gf ex >>= \(newGf, atom) -> 
    putStr (show atom ++ "\n") >> return (newGf, atom) 
preDefinedFuncs gf "define" [perameters, body] =
    case expressToIdents perameters of 
        Just (funcName:parameterNames) ->
             return (insert funcName (parameterNames, body) gf, Error ValueError) 
        _ -> return (gf, Error UnknownError) 
preDefinedFuncs gf ident [ex1, ex2] =
        (\case 
            Nothing -> return (gf, Error UnboundVariable)
            Just f -> 
                interpretExpress gf ex1 >>= \(_, a1) -> 
                interpretExpress gf ex2 >>= \(_,a2) -> 
                return (gf, applyTwoArgFuncToAtom f a1 a2) 
        ) $ case ident of  
            "+" -> Just (+)
            "-" -> Just (-)
            "*" -> Just (*)
            "/" -> Just div
            _ -> Nothing
preDefinedFuncs gf _ _ = return (gf, Error UnboundVariable)
     
applyTwoArgFuncToAtom ::  (Int -> Int -> Int) -> Atom -> Atom -> Atom
applyTwoArgFuncToAtom _ (Error e) _ = Error e
applyTwoArgFuncToAtom _ _ (Error e) = Error e
applyTwoArgFuncToAtom f (Value v1) (Value v2) = Value $ f v1 v2
applyTwoArgFuncToAtom _ _ _ = Error ValueError 

expressToIdents :: Express -> Maybe [Ident] --Clean me up
expressToIdents (Comb []) = Just []
expressToIdents (Comb ((A (Ident i) ):lst)) = (:) i <$> expressToIdents (Comb lst) 
expressToIdents (A (Ident i)) = Just [i]
expressToIdents _ = Nothing 