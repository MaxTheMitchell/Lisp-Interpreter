{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module OneArgAtomFuncs (oneArgAtomFunc) where

import Types
import Data.Char (ord,chr)
import Text.Read (readMaybe)

oneArgAtomFunc :: Ident -> Atom -> Atom 
oneArgAtomFunc ident (Value val) =
    \case 
        Nothing -> Error $ UnboundVariable ident 
        Just v -> Value v 
    $ foldl (\acc new -> 
        case acc of
            Nothing -> new ident val 
            Just _ -> acc
        ) Nothing [misc, introspection, typeCast]
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

introspection :: Ident -> Value -> Maybe Value 
introspection "list?" (List _) = Just $ Bool True 
introspection "list?" _ = Just $ Bool False  
introspection "number?" (Number _) = Just $ Bool True
introspection "number?" _ = Just $ Bool False
introspection "boolean?" (Bool _) = Just $ Bool True
introspection "boolean?" _ = Just $ Bool False 
introspection "char?" (Char _) = Just $ Bool True 
introspection "char?" _ = Just $ Bool False  
introspection _ _ = Nothing  

misc :: Ident -> Value -> Maybe Value 
misc "not" (Bool b) = Just . Bool $ not b 
misc _ _ = Nothing 