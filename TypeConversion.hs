{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module TypeConversion (convertTypes) where

import Types
    ( Atom(Error, Value),
      Error(TypeError, UnboundVariable),
      Express(A),
      Ident,
      Value(Char, String, List, Number) )
import Data.Char (ord,chr)
import Text.Read (readMaybe)

convertTypes :: Ident -> Atom -> Atom 
convertTypes ident (Value val) =
    \case
        Just v -> Value v
        Nothing -> Error $ UnboundVariable ident 
    $ case (ident, val) of 
        ("char->integer", Char c) ->
            Just . Number . fromIntegral $ ord c 
        ("integer->char", Number n) ->
            if n /= fromInteger (floor n) 
                then Nothing 
                else Just . Char . chr $ floor n
        ("string->number", String str) ->
            Number <$> readMaybe str 
        ("string->list", String str) -> 
                Just . List $ map (A . Value . Char) str
        ("list->string", List exs) -> 
            String <$>
            foldr (\new acc ->     
                case new of 
                    A (Value (Char c)) -> (c:) <$> acc 
                    _ -> Nothing  
            ) (Just "") exs
        _ -> Nothing 
convertTypes _ _ = Error TypeError 