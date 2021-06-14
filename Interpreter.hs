{-# OPTIONS_GHC -Wall #-}

module Interpreter (interpretExpress) where 

import Types

interpretExpress :: Express -> Atom  
interpretExpress (A a) = a
interpretExpress (Comb (A(Operator op):args)) = applyOperation op args
interpretExpress _ = Error SyntaxError

applyOperation :: Char -> [Express] -> Atom 
applyOperation op [ex1,ex2] =
    applyOpToAtoms
        (case op of 
            '+' -> (+) 
            '-' -> (-)
            '*' -> (*)
            '/' -> div
            _ -> (\_ _ -> 0) -- TODO: make cause error
            )
        (interpretExpress ex1)
        (interpretExpress ex2)
applyOperation _ _ = Error OperatorError

applyOpToAtoms :: (Int -> Int -> Int) -> Atom -> Atom -> Atom 
applyOpToAtoms f (Value v1) (Value v2) = Value $ f v1 v2
applyOpToAtoms _ _ _ = Error InvalidArguments