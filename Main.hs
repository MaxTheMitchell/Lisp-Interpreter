{-# OPTIONS_GHC -Wall #-}

module Main where 

import Interpreter ( interpretProgram )

import System.Environment

main :: IO ()
main = 
    getArgs >>= 
    mapM readFile >>= 
    foldl (\acc new -> interpretProgram new >> acc ) (return ())
