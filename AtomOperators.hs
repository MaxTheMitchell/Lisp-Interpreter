{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module AtomOperators (applyAtomOperator) where 

import Types

applyAtomOperator :: Ident -> Atom  -> Atom  -> Atom
applyAtomOperator ident a1 a2 = 
    \case 
        Nothing -> Error $ UnboundVariable ident 
        Just f -> f a1 a2 
    $ foldl (\acc new -> 
        case acc of 
            Nothing -> new ident
            Just _ -> acc     
        ) Nothing [miscFuncs, numberFuncs, compareFuncs, boolFuncs]

miscFuncs :: Ident -> Maybe (Atom -> Atom -> Atom) 
miscFuncs "eqv?" = Just (\a1 a2 -> 
    case (a1, a2) of  
        (Value v1, Value v2) -> Value . Bool $ v1 == v2 
        _ -> carryOverError TypeError [a1, a2])
miscFuncs _ = Nothing 

boolFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
boolFuncs ident =
    wrapAdamBoolFunc <$>
    case ident of 
        "and" -> Just (&&)
        "or" -> Just (||)
        _ -> Nothing 

numberFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
numberFuncs ident = 
    wrapAdamNumFunc <$>
    case ident of 
        "+" -> Just (+)
        "-" -> Just (-)
        "*" -> Just (*)
        "/" -> Just (/)
        _ -> Nothing 


compareFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
compareFuncs ident =
    wrapAdamCompareFunc <$> 
        case ident of 
            ">" -> Just (>)
            "<" -> Just (<)
            "=" -> Just (==)
            ">=" -> Just (>=)
            "<=" -> Just (<=)
            "/=" -> Just (/=)
            _ -> Nothing 

wrapAdamNumFunc :: (Number -> Number -> Number) -> (Atom -> Atom -> Atom)
wrapAdamNumFunc f (Value (Number n1)) (Value (Number n2)) = Value . Number $ f n1 n2
wrapAdamNumFunc _ a1 a2 =  carryOverError TypeError [a1, a2] 

wrapAdamCompareFunc :: (Number -> Number -> Bool) -> (Atom -> Atom -> Atom)
wrapAdamCompareFunc f (Value (Number n1)) (Value (Number n2)) = Value . Bool $ f n1 n2
wrapAdamCompareFunc _ a1 a2 =  carryOverError TypeError [a1, a2] 

wrapAdamBoolFunc :: (Bool -> Bool -> Bool) -> (Atom -> Atom -> Atom)
wrapAdamBoolFunc f (Value (Bool b1)) (Value (Bool b2)) = Value . Bool $ f b1 b2
wrapAdamBoolFunc _ a1 a2 =  carryOverError TypeError [a1, a2] 

carryOverError :: Error -> [Atom] -> Atom 
carryOverError fallback = 
    Error 
    . foldl (\acc new -> case new of 
        Error e -> e
        _ -> acc) fallback

