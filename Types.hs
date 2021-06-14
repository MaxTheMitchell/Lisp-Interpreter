{-# OPTIONS_GHC -Wall #-}

module Types where 

data Atom 
    = Ident String 
    | Operator Char
    | Value Int 
    | Error Error
    deriving Show

data Express 
    = A Atom 
    | Comb [Express]
    deriving Show

data Error 
    = SyntaxError
    | OperatorError
    | InvalidArguments 
    deriving Show

errorMessage :: Error -> String
errorMessage SyntaxError = "Syntax Error"
errorMessage OperatorError = "Operator Error"
errorMessage InvalidArguments = "Invalid Arguments" 