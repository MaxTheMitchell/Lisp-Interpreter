{-# OPTIONS_GHC -Wall #-}

module Types where 

type Program = [Action]

data Action 
    = Display Express
    | Definition String [String] Express 
    deriving Show

data Express 
    = A Atom 
    | Comb [Express]
    deriving Show

data Atom 
    = Ident Ident
    | Operator Char
    | Value Int 
    | Error Error
    deriving Show

type Ident = String

data Error 
    = SyntaxError
    | OperatorError
    | InvalidArguments 
    deriving Show

errorMessage :: Error -> String
errorMessage SyntaxError = "Syntax Error"
errorMessage OperatorError = "Operator Error"
errorMessage InvalidArguments = "Invalid Arguments" 