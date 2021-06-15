{-# OPTIONS_GHC -Wall #-}

module Types where 

import Data.Map (Map)

type State = (GlobalFuncs, Atom)

type GlobalFuncs = Map Ident Lambda

type Lambda = ([Ident], Express)

type Program = [Express]

data Express 
    = A Atom 
    | Comb [Express]
    deriving Show

data Atom 
    = Ident Ident
    | Value Value 
    | Error Error

instance Show Atom where 
    show (Ident i) = i 
    show (Value v) = show v
    show (Error e) = show e 

type Ident = String

data Value 
    = Int Int 
    | Bool Bool

instance Show Value where
    show (Int i) = show i
    show (Bool b) = show b

data Error 
    = OperatorError
    | InvalidArguments Int 
    | ValueError
    | UnboundVariable String
    | UnknownError 
    | Debug String

instance Show Error where 
    show OperatorError = "Operator Error"
    show (InvalidArguments args) = 
        "Invalid Arguments: " 
        ++ show (abs args) 
        ++ if args > 0 
            then " too many arguments"
            else " too few arguments"  
    show (UnboundVariable str) = "Unbound Variable: " ++ str 
    show ValueError = "Value Error"
    show UnknownError = "Unknown Error"
    show (Debug str) = "Debug: " ++ str 