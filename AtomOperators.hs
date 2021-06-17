{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module AtomOperators (applyAtomOperator) where 

import Types
    ( Atom(Error, Value),
      Error(UnboundVariable, UnknownError, TypeError),
      Ident,
      Value(Bool, Int) )

applyAtomOperator :: Ident -> Atom  -> Atom  -> Atom
applyAtomOperator ident a1 a2 = 
    case twoArgFunc ident of 
        Nothing -> Error $ UnboundVariable ident 
        Just f -> f a1 a2 

twoArgFunc :: Ident -> Maybe (Atom -> Atom -> Atom)
twoArgFunc ident = 
    foldl (\acc new -> 
        case acc of 
            Nothing -> new ident
            Just _ -> acc     
        ) Nothing [intFuncs, boolFuncs]

intFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
intFuncs ident = 
    (\f a1 a2 ->
        case (a1,a2) of  
            (Value v1, Value v2) -> Value $ f v1 v2 
            _ -> Error TypeError  
    ) <$>
    case ident of 
        "+" -> Just (+)
        "-" -> Just (-)
        "*" -> Just (*)
        "/" -> Just (/)
        _ -> Nothing 

boolFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
boolFuncs ident =
    wrapAdamBoolFunc <$> 
        case ident of 
            ">" -> Just (>)
            "<" -> Just (<)
            "=" -> Just (==)
            ">=" -> Just (>=)
            "<=" -> Just (<=)
            "/=" -> Just (/=)
            _ -> Nothing 

wrapAdamBoolFunc :: (Int  -> Int -> Bool) -> (Atom -> Atom -> Atom)
wrapAdamBoolFunc f a1 a2 = case (a1, a2) of 
    (Value (Int b1),Value (Int b2)) -> Value . Bool $ f b1 b2
    _ -> carryOverError UnknownError [a1, a2] 

carryOverError :: Error -> [Atom] -> Atom 
carryOverError fallback = 
    Error 
    . foldl (\acc new -> case new of 
        Error e -> e
        _ -> acc) fallback