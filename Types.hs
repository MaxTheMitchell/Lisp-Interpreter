{-# OPTIONS_GHC -Wall #-}

module Types where 

data Atom 
    = Ident String 
    | Operator Char
    | Value Int 
    deriving Show

data Express 
    = A Atom 
    | Comb [Express]
    deriving Show
