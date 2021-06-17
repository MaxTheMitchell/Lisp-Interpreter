{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module AtomOperators (applyAtomOperator) where 

import Types
    ( Atom(Error, Value),
      Error(UnknownError, UnboundVariable),
      Ident,
      Value(Bool, Number),
      Number )

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
    wrapAdamNumFunc <$>
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

wrapAdamNumFunc :: (Number -> Number -> Number) -> (Atom -> Atom -> Atom)
wrapAdamNumFunc f a1 a2 = case (a1, a2) of 
    (Value (Number n1),Value (Number n2)) -> Value . Number $ f n1 n2
    _ -> carryOverError UnknownError [a1, a2] 

wrapAdamBoolFunc :: (Number -> Number -> Bool) -> (Atom -> Atom -> Atom)
wrapAdamBoolFunc f a1 a2 = case (a1, a2) of 
    (Value (Number n1),Value (Number n2)) -> Value . Bool $ f n1 n2
    _ -> carryOverError UnknownError [a1, a2] 

carryOverError :: Error -> [Atom] -> Atom 
carryOverError fallback = 
    Error 
    . foldl (\acc new -> case new of 
        Error e -> e
        _ -> acc) fallback