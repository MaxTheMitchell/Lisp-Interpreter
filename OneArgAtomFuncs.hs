{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module OneArgAtomFuncs (oneArgAtomFunc) where

import Types
import Data.Char (ord,chr)
import Text.Read (readMaybe)

oneArgAtomFunc :: Ident -> Atom -> Atom 
oneArgAtomFunc ident (Value val) =
    \case
        Just v -> Value v
        Nothing -> Error $ UnboundVariable ident 
    $ case (ident, val) of  
        ("not", Bool b) -> Just . Bool $ not b 
        _ -> typeCast ident val
oneArgAtomFunc _ _ = Error TypeError 

typeCast :: Ident  -> Value -> Maybe Value
typeCast "list->string" (List exs) = 
    String <$>
    foldr (\new acc ->     
        case new of 
            A (Value (Char c)) -> (c:) <$> acc 
            _ -> Nothing  
    ) (Just "") exs
typeCast "integer->char" (Number n) =
    if n /= fromInteger (floor n) 
        then Nothing 
        else Just . Char . chr $ floor n
typeCast "char->integer" (Char c) = Just . Number . fromIntegral $ ord c 
typeCast "number->string" (Number n) = Just . String $ show n
typeCast "string->number" (String str) = Number <$> readMaybe str 
typeCast "string->list" (String str) = Just . List $ map (A . Value . Char) str
typeCast _ _ = Nothing 