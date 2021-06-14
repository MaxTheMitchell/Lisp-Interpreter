{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (interpretProgram) where 

import Types
import Parser (parseProgram)
import Data.Map (Map(..), empty, insert) 

type GlobalFuncs = Map String ([String], Express)

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
completeAction gf (Display ex) = putStr (show $ interpretExpress gf ex) >> return gf 
completeAction gf _ = return gf 


interpretExpress :: GlobalFuncs -> Express -> Atom  
interpretExpress _ (A a) = a
interpretExpress gf (Comb (A(Operator op):[ex1, ex2])) = applyOperation gf op ex1 ex2 
interpretExpress _ _ = Error SyntaxError

applyOperation :: GlobalFuncs -> Char -> Express  -> Express  -> Atom 
applyOperation gf op ex1 ex2 =
    applyOpToAtoms
        (case op of 
            '+' -> (+) 
            '-' -> (-)
            '*' -> (*)
            '/' -> div
            _ -> (\_ _ -> 0) -- TODO: make cause error
            )
        (interpretExpress gf ex1)
        (interpretExpress gf ex2)

applyOpToAtoms :: (Int -> Int -> Int) -> Atom -> Atom -> Atom 
applyOpToAtoms f (Value v1) (Value v2) = Value $ f v1 v2
applyOpToAtoms _ _ _ = Error InvalidArguments

initFuncs :: GlobalFuncs 
initFuncs = empty 