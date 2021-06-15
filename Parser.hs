{-# OPTIONS_GHC -Wall #-}

module Parser where 

import Types 

import Text.ParserCombinators.ReadP
    ( munch, munch1, satisfy, ReadP, skipSpaces, char, readP_to_S, eof, string)
import Data.Char ( isDigit, isAlpha, isAlphaNum )
import Control.Applicative ((<|>), Alternative (many))


parseProgram :: String -> Maybe Program 
parseProgram str = case readP_to_S programParser str of
    [(program,"")] -> Just program
    _ -> Nothing

programParser :: ReadP Program 
programParser = many expressParser <* skipSpaces <* eof

expressParser :: ReadP Express 
expressParser =
    skipSpaces *> (A <$> atomParser <|> wrapInParenthesis (Comb <$> many expressParser))

atomParser :: ReadP Atom 
atomParser = Ident <$> identParser <|> Value <$> valueParser <|> operatorParser
    where
        operatorParser = (\c -> Ident [c]) <$> satisfy (`elem` "+-*/<>=")
            <|> Ident <$> ((string "/=" <|> string ">=") <|> string "<=")

valueParser :: ReadP Value 
valueParser = intParser <|> boolParser
    where 
        intParser = Int . read <$> munch1 isDigit
        boolParser = Bool True <$ string "#t" <|> Bool False <$ string "#f" 

identParser :: ReadP Ident
identParser = (:) <$> (skipSpaces *> satisfy isAlpha) <*> munch isAlphaNum

wrapInParenthesis :: ReadP a -> ReadP a 
wrapInParenthesis parser = skipSpaces *> char '(' *> skipSpaces *> parser <* skipSpaces <* char ')'
