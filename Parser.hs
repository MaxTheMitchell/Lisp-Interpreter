{-# OPTIONS_GHC -Wall #-}

module Parser where 

import Types 

import Text.ParserCombinators.ReadP
    ( munch, munch1, satisfy, ReadP, skipSpaces, char, readP_to_S, string, eof)
import Data.Char ( isDigit, isAlpha, isAlphaNum )
import Control.Applicative ((<|>), Alternative (many))


parseProgram :: String -> Maybe Program 
parseProgram str = case readP_to_S programParser str of
    [(program,"")] -> Just program
    _ -> Nothing

programParser :: ReadP Program 
programParser = many actionParser <* skipSpaces <* eof

actionParser :: ReadP Action  
actionParser = 
    let 
        printParser =
             Display <$> wrapInParenthesis (string "display" *> expressParser)
        defParser = 
            wrapInParenthesis
                (string "define" *> 
                wrapInParenthesis (Definition <$>  identParser <*> many identParser) 
                <*> expressParser)
    in
        skipSpaces *> (printParser <|> defParser)

expressParser :: ReadP Express 
expressParser =
    skipSpaces *> (A <$> atomParser <|> wrapInParenthesis (Comb <$> many expressParser))

atomParser :: ReadP Atom 
atomParser = Ident <$> identParser <|> valueParser <|> operatorParser
    where
        operatorParser = (\c -> Ident (c:""))  <$> satisfy (`elem` "+-*/")
        valueParser = Value . read <$> munch1 isDigit 

identParser :: ReadP Ident
identParser = (:) <$> (skipSpaces *> satisfy isAlpha) <*> munch isAlphaNum

wrapInParenthesis :: ReadP a -> ReadP a 
wrapInParenthesis parser = skipSpaces *> char '(' *> skipSpaces *> parser <* skipSpaces <* char ')'
