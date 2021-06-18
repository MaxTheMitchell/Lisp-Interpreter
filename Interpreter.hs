{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Interpreter (interpretProgram, interpretExpress, interpretAtom) where 

import Types
import AtomOperators (applyAtomOperator)
import OneArgAtomFuncs (oneArgAtomFunc)
import Parser (parseProgram)
import Data.Map (empty, (!?), insert)
import Control.Monad (void, (>=>))

interpretProgram :: String -> IO () 
interpretProgram input = 
        case parseProgram input of  
            Just program -> runProgram program initFuncs
            Nothing -> putStr "Syntax Error\n"
    
runProgram :: Program -> GlobalFuncs  -> IO ()
runProgram [] =  const $ return ()
runProgram (a:as) = 
    interpretExpress a >=> 
    \case 
        (Error e, _) -> void . putStr $ show e ++ "\n"
        (_, newGf) -> runProgram as newGf

interpretExpress :: Express -> GlobalFuncs -> IO State   
interpretExpress (Comb comb) = interpretComb comb
interpretExpress (A atom) = interpretAtom atom 

interpretComb :: Comb -> GlobalFuncs -> IO State 
interpretComb (A(Ident funcName):args) = callFunction funcName args
interpretComb (A(Value(Lambda lambda)):args) = callLambda lambda args
interpretComb comb = interpretCombHead comb 

interpretAtom ::  Atom -> GlobalFuncs -> IO State 
interpretAtom (Ident funcName) = callFunction funcName [] 
interpretAtom atom = return . (atom,)

interpretCombHead :: Comb -> GlobalFuncs -> IO State 
interpretCombHead [ex] = interpretExpress ex
interpretCombHead (ex:exs) = 
    interpretExpress ex >=> \case
        (err@(Error _), _) -> return (err, empty) 
        (atom, newGf) -> interpretExpress (Comb $ A atom:exs) newGf
interpretCombHead [] = return . (Error UnknownError,) 

callFunction :: Ident -> [Express] -> GlobalFuncs -> IO State 
callFunction funcName args gf = 
    case gf !? funcName of 
        Nothing -> preDefinedFuncs funcName args gf
        Just  lambda -> callLambda lambda args gf

callLambda ::  Lambda -> [Express] -> GlobalFuncs -> IO State
callLambda (perameters, body) args = 
     interpretExpress (bindArguments perameters args body)

bindArguments :: [Ident] -> [Express] -> Express -> Express   
bindArguments [] [] express = express
bindArguments [] args _ = A . Error . InvalidArguments $ length args
bindArguments params [] _ = A . Error . InvalidArguments . negate $ length params
bindArguments (p:ps) (a:as) express = bindArguments ps as $ bindArgument p a express

bindArgument :: Ident -> Express -> Express -> Express 
bindArgument param arg (A (Ident ident)) 
    | ident == param  = arg 
    | otherwise = A $ Ident ident
bindArgument _ _ (A atom) = A atom 
bindArgument _ _ comb@(Comb (A (Ident "lambda"):_)) = comb
bindArgument param arg (Comb es) = Comb $ map (bindArgument param arg) es  

initFuncs :: GlobalFuncs 
initFuncs = empty 

preDefinedFuncs :: Ident -> [Express] -> GlobalFuncs -> IO State 
preDefinedFuncs "list" exs = createList exs
preDefinedFuncs "newline" [] = newLine
preDefinedFuncs ident [ex] = oneArgFunc ident ex 
preDefinedFuncs ident [ex1, ex2] = twoArgFunc ident ex1 ex2 
preDefinedFuncs "if" [condition, thenCase, elseCase] = ifConditional condition thenCase elseCase
preDefinedFuncs ident _ = return . (Error $ UnboundVariable ident,)

oneArgFunc :: Ident -> Express -> GlobalFuncs -> IO State
oneArgFunc ident ex =
    interpretExpress ex >=>
        uncurry
        (case ident of 
            "display" -> display 
            "car" -> car 
            "cdr" -> cdr 
            "null?" -> isNull 
            "readFile" -> myReadFile
            _ -> applyOneArgAtomFunc ident)

twoArgFunc :: Ident -> Express -> Express -> GlobalFuncs -> IO State 
twoArgFunc "define" = defineFunction
twoArgFunc "cons" = cons 
twoArgFunc "lambda" = defineLambda
twoArgFunc op = applyOperator op 

newLine :: GlobalFuncs -> IO State 
newLine gf = putStr "\n" >> return (Value $ List [], gf)

display :: Atom -> GlobalFuncs -> IO State
display err@(Error _) = return . (err,)
display (Value (List l)) = displayList l 
display atom = (>>) (putStr $ show atom) . return . (atom,)

myReadFile :: Atom -> GlobalFuncs -> IO State 
myReadFile (Value (String str)) gf = (, gf) . Value . String <$> readFile str 
myReadFile _ _ = return (Error TypeError, empty)

displayList :: [Express] -> GlobalFuncs -> IO State
displayList exs = 
    combToAtoms exs >=> \case 
        (atom@(Value (List atoms)), gf) -> 
            putStr  ("(" ++ unwords (map show atoms) ++ ")") >> return (atom, gf)
        err -> return err 
    
defineFunction :: Express -> Express -> GlobalFuncs -> IO State 
defineFunction perameters body gf =
    case expressToIdents perameters of 
        Just (funcName:parameterNames) ->
             return (Value $ Lambda (parameterNames, body), insert funcName (parameterNames, body) gf) 
        _ -> return (Error UnknownError, gf) 

defineLambda :: Express -> Express -> GlobalFuncs -> IO State 
defineLambda ex body gf =
    return . (,gf) $
    case expressToIdents ex of 
        Just perameters -> Value $ Lambda (perameters, body)
        Nothing  -> Error TypeError

ifConditional :: Express -> Express -> Express -> GlobalFuncs -> IO State 
ifConditional condtion ifCase elseCase = 
    interpretExpress condtion >=> \case  
        (Value (Bool b), newGf) -> 
            interpretExpress (if b then ifCase else elseCase) newGf
        _ -> return (Error TypeError, empty) 

createList :: [Express] -> GlobalFuncs -> IO State 
createList exs gf = return . (,gf) . Value $ List exs 

car :: Atom -> GlobalFuncs -> IO State 
car (Value (List (h:_))) = interpretExpress h
car _ = return . (Error TypeError,)  

cdr :: Atom -> GlobalFuncs -> IO State 
cdr = 
    tryApplyListFunc (\case 
        (_:ex) -> Value $ List ex
        _ -> Error TypeError)
            
cons :: Express -> Express -> GlobalFuncs -> IO State 
cons ex lstEx = 
    interpretExpress lstEx >=>
        uncurry (tryApplyListFunc (Value . List . (:) ex)) 

isNull :: Atom -> GlobalFuncs -> IO State 
isNull = tryApplyListFunc (Value . Bool . null)

tryApplyListFunc :: ([Express] -> Atom) -> Atom -> GlobalFuncs -> IO State 
tryApplyListFunc f (Value (List lst)) = return . (f lst,)
tryApplyListFunc _ _  = return . (Error TypeError,)

applyOperator :: Ident -> Express -> Express -> GlobalFuncs -> IO State 
applyOperator ident ex1 ex2 = 
    interpretExpress ex1 >=> \(a1, gf1) -> 
    interpretExpress ex2 gf1 >>= \(a2, gf2) ->
        return (applyAtomOperator ident a1 a2, gf2)

applyOneArgAtomFunc :: Ident -> Atom -> GlobalFuncs -> IO State 
applyOneArgAtomFunc ident (Value (List comb)) =  
    combToAtoms comb >=> \(atoms, newGf) ->
        return (oneArgAtomFunc ident atoms, newGf)
applyOneArgAtomFunc ident atom = return . (oneArgAtomFunc ident atom,) 

expressToIdents :: Express -> Maybe [Ident]
expressToIdents (Comb exs) = 
    foldl (\acc ex -> (++) <$> acc <*> expressToIdents ex) (Just []) exs
expressToIdents (A (Ident i)) = Just [i]
expressToIdents _ = Nothing 

combToAtoms :: Comb -> GlobalFuncs -> IO State 
combToAtoms [] = return . (Value $ List [],)
combToAtoms (ex:exs) = 
    interpretExpress ex >=> \(atom, gf) -> 
        combToAtoms exs gf >>= \case 
            (Value (List comb), newGf) -> 
                return (Value . List $ A atom:comb, newGf)
            _ -> return (Error UnknownError, empty ) 