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

instance Show Atom where 
    show (Ident i) = "No definition of function: " ++ i 
    show (Operator _) = show OperatorError
    show (Value v) = show v
    show (Error e) = show e 

type Ident = String

data Error 
    = SyntaxError
    | OperatorError
    | InvalidArguments 

instance Show Error where 
    show SyntaxError = "Syntax Error"
    show OperatorError = "Operator Error"
    show InvalidArguments = "Invalid Arguments" 