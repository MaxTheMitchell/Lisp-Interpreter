{-# OPTIONS_GHC -Wall #-}

module Types where 

import Data.Map (Map)

type State = (GlobalFuncs, Atom)

type GlobalFuncs = Map Ident Lambda

type Lambda = ([Ident], Express)

type Program = [Express]

data Express 
    = A Atom 
    | Comb Comb

type Comb = [Express]

instance Show Express where 
    show (A a) = show a 
    show (Comb exs) = 
        "(" ++ unwords (map show exs) ++ ")"

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
    = Number Number
    | Char Char 
    | String String
    | Bool Bool
    | List [Express]
    | Lambda Lambda

type Number = Float

instance Eq Value where
    Number n1 == Number n2 = n1 == n2 
    Char c1 == Char c2 = c1 == c2 
    String str1 == String str2 = str1 == str2 
    Bool b1 == Bool b2 = b1 == b2 
    _ == _ = False

instance Show Value where
    show (Number n) = 
        if fromInteger (floor n) == n
            then takeWhile (not . (==) '.') $ show n 
            else show n
    show (Char c) = [c]
    show (String s) = s     
    show (Bool b) = if b then "#t" else "#f"
    show (List ex) = "(" ++ unwords (map show ex) ++ ")"
    show (Lambda (ps, body)) = "(lambda (" ++ unwords ps ++ ") " ++ show body ++ ")"

data Error 
    = OperatorError
    | InvalidArguments Int 
    | TypeError
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
    show TypeError = "Type Error"
    show UnknownError = "Unknown Error"
    show (Debug str) = "Debug: " ++ str 