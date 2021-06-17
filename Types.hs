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
    = Int Int 
    | Float Float 
    | Bool Bool
    | List [Express]
    | Lambda Lambda
    | ValueError

instance Num Value where
    (Int i1) + (Int i2) = Int $ i1 + i2
    (Float f1) + (Float f2) = Float $ f1 + f2
    (Int i) + f@(Float _) = Float (fromIntegral i) + f
    f@(Float _) + i@(Int _) = i + f
    (+) _ _ = ValueError
    (Int i1) * (Int i2) = Int $ i1 * i2
    (Float f1) * (Float f2) = Float $ f1 * f2
    (Int i) * f@(Float _) = Float (fromIntegral i) * f
    f@(Float _) * i@(Int _) = i * f
    (*) _ _ = ValueError
    abs (Int i) = Int $ abs i 
    abs (Float f) = Float $ abs f
    abs _ = ValueError
    signum (Int i) = Int $ signum i 
    signum (Float f) = Float $ signum f 
    signum _ = ValueError
    fromInteger = Int . fromInteger 
    negate (Int i) = Int $ negate i  
    negate (Float f) = Float $ negate f
    negate _ = ValueError 

instance Fractional Value where 
    fromRational = Float . fromRational 
    (Float f1) / (Float f2) = Float $ f1 / f2
    (Int i) / val = Float (fromIntegral i) / val 
    val / (Int i) =  val / Float (fromIntegral i) 
    (/) _ _ = ValueError

instance Show Value where
    show (Int i) = show i
    show (Float f) = show f
    show (Bool b) = if b then "#t" else "#f"
    show (List ex) = "(" ++ unwords (map show ex) ++ ")"
    show (Lambda (ps, body)) = "(lambda (" ++ unwords ps ++ ") " ++ show body ++ ")"
    show ValueError = "Value Error"


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