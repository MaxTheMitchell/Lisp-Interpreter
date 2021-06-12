{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Main where 

import Parser (parseExpress)
import Interpreter ( interpretExpress )
import Types ( Atom(Value) ) 

import System.Environment

runProgram :: String -> String 
runProgram = 
    (\case 
        Value v -> show v ++ "\n"
        _ -> "There is an error in your program\n")
    . interpretExpress
    . parseExpress

main :: IO ()
main = 
    getArgs >>= 
    mapM readFile >>= 
    putStr . concatMap runProgram  
