{-# OPTIONS_GHC -Wall #-}

module Types where 

import Data.Map (Map)

type GlobalFuncs = Map Ident Lambda

type Lambda = ([Ident], Express)

type Program = [Action]

data Action 
    = Display Express
    | Definition Ident [Ident] Express 
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
    | UnboundVariable

instance Show Error where 
    show SyntaxError = "Syntax Error"
    show OperatorError = "Operator Error"
    show InvalidArguments = "Invalid Arguments" 
    show UnboundVariable = "Unbound Variable"