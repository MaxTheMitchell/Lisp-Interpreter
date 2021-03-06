{-# OPTIONS_GHC -Wall #-}

module Parser (parseProgram) where 

import Types
    ( Atom(Value, Ident), Express(..), Ident, Program, Value(..) ) 

import Text.ParserCombinators.ReadP
import Data.Char ( isDigit, isSpace )
import Control.Applicative ((<|>))


parseProgram :: String -> Maybe Program 
parseProgram str = case readP_to_S programParser str of
    [(program,"")] -> Just program
    _ -> Nothing

programParser :: ReadP Program 
programParser = many expressParser <* skipSpaces <* skipComments <* eof

expressParser :: ReadP Express 
expressParser =
    skipSpaces *> skipComments *> (A <$> atomParser <|> wrapInParenthesis (Comb <$> many expressParser))

atomParser :: ReadP Atom 
atomParser = Ident <$> identParser <|> Value <$> valueParser

valueParser :: ReadP Value 
valueParser = intParser <|> floatParser <|> charParser <|> stringParser <|> boolParser
    where 
        intParser = Number . read <$> munch1 isDigit
        floatParser = (\a b -> Number . read . (++) a . (++) b ) <$> munch1 isDigit <*> string "." <*> munch1 isDigit
        charParser = Char <$> (string "#\\" *> get) 
        stringParser = String <$> (char '"' *> manyTill get (char '"'))
        boolParser = Bool True <$ string "#t" <|> Bool False <$ string "#f" 

identParser :: ReadP Ident
identParser = (:) <$> (skipSpaces *> 
    satisfy (\c -> not (isDigit c || c `elem` "();#.\"" ) )) 
    <*> munch (\c -> not (isSpace c || c `elem` "();#.\""))

skipComments :: ReadP () 
skipComments = skipMany (skipSpaces *> char ';' *> manyTill get (char '\n' <|> ' ' <$ eof ) <* skipSpaces )

wrapInParenthesis :: ReadP a -> ReadP a 
wrapInParenthesis parser = skipSpaces *> char '(' *> skipSpaces *> parser <* skipSpaces <* char ')'
