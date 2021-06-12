{-# OPTIONS_GHC -Wall #-}

module Parsers (parseExpress) where 

import Types

import Text.ParserCombinators.ReadP
    ( munch, munch1, satisfy, ReadP, skipSpaces, char, readP_to_S)
import Data.Char ( isDigit, isAlpha, isAlphaNum )
import Control.Applicative ((<|>), Alternative (many))

valueParser :: ReadP Atom 
valueParser = Value . read <$> munch1 isDigit 

identParser :: ReadP Atom
identParser = (\c str -> Ident (c:str)) <$> satisfy isAlpha <*> munch isAlphaNum

operatorParser :: ReadP Atom 
operatorParser = Operator <$> satisfy (`elem` "+-*/")

atomParser :: ReadP Atom 
atomParser = valueParser <|> identParser <|> operatorParser

expressParser :: ReadP Express 
expressParser =
    A <$> (skipSpaces *> atomParser) <|> skipSpaces *> char '(' *> (Comb <$> many expressParser) <* skipSpaces <* char ')'

parseExpress :: String -> Express
parseExpress = fst . head . readP_to_S expressParser 