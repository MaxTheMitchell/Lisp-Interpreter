{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Interpreter (interpretProgram, interpretExpress) where 

import Types
import AtomOperators (applyAtomOperator)
import Parser (parseProgram)
import Data.Map (empty, (!?), insert)
import Control.Monad (void, (>=>))

interpretProgram :: String -> IO () 
interpretProgram input = 
        case parseProgram input of  
            Just program -> runProgram initFuncs program
            Nothing -> putStr "Syntax Error\n"
    
runProgram :: GlobalFuncs -> Program -> IO ()
runProgram _ [] =  return ()
runProgram gf (a:as) = 
    interpretExpress a gf >>= 
    \case 
        (_, Error e) -> void . putStr $ show e ++ "\n"
        (newGf, _) -> runProgram newGf as  

interpretExpress :: Express -> GlobalFuncs -> IO State   
interpretExpress (Comb comb) = interpretComb comb
interpretExpress (A atom) = interpretAtom atom 

interpretComb :: Comb -> GlobalFuncs -> IO State 
interpretComb (A(Ident funcName):args) = callFunction funcName args
interpretComb (A(Value(Lambda lambda)):args) = callLambda lambda args
interpretComb comb = interpretCombHead comb 

interpretAtom ::  Atom -> GlobalFuncs -> IO State 
interpretAtom (Ident funcName) = callFunction funcName [] 
interpretAtom atom = return . (,atom)

interpretCombHead :: Comb -> GlobalFuncs -> IO State 
interpretCombHead [ex] = interpretExpress ex
interpretCombHead (ex:exs) = 
    interpretExpress ex >=> \(newGf, atom) -> 
        interpretExpress (Comb $ A atom:exs) newGf
interpretCombHead [] = return . (,Error UnknownError) 

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
bindArgument param arg (Comb es) = Comb $ map (bindArgument param arg) es  

initFuncs :: GlobalFuncs 
initFuncs = empty 

preDefinedFuncs :: Ident -> [Express] -> GlobalFuncs -> IO State 
preDefinedFuncs "newline" [] = newLine
preDefinedFuncs ident [ex] = oneArgFunc ident ex 
preDefinedFuncs ident [ex1, ex2] = twoArgFunc ident ex1 ex2 
preDefinedFuncs "if" [condition, thenCase, elseCase] = ifConditional condition thenCase elseCase
preDefinedFuncs "list" exs = createList exs
preDefinedFuncs ident _ = return . (, Error $ UnboundVariable ident)

oneArgFunc :: Ident -> Express -> GlobalFuncs -> IO State
oneArgFunc "display" = display
oneArgFunc "car" = car
oneArgFunc "cdr" = cdr 
oneArgFunc "null?" = isNull
oneArgFunc ident = \_ _ -> return (empty, Error $ UnboundVariable ident )

twoArgFunc :: Ident -> Express -> Express -> GlobalFuncs -> IO State 
twoArgFunc "define" = defineFunction
twoArgFunc "cons" = cons 
twoArgFunc "lambda" = defineLambda
twoArgFunc op = applyOperator op 

newLine :: GlobalFuncs -> IO State 
newLine gf = putStr "\n" >> return (gf, Value $ List [] )

display :: Express -> GlobalFuncs -> IO State
display ex = 
    interpretExpress ex >=> 
    \case
        (newGf, Value (List l)) -> displayList l newGf 
        (newGf, atom) -> putStr (show atom) >> return (newGf, atom) 

displayList :: [Express] -> GlobalFuncs -> IO State
displayList exs gf =
    putStr "( " >> 
        foldl (\io ex ->
            io >>=  
            (display ex >=> \(newGf, _) -> 
            putStr " " >> return newGf)) 
        (return gf) exs >>= 
            \newGf ->
                putStr ")" >> return (newGf, Value $ List exs)
    
defineFunction :: Express -> Express -> GlobalFuncs -> IO State 
defineFunction perameters body gf =
    case expressToIdents perameters of 
        Just (funcName:parameterNames) ->
             return (insert funcName (parameterNames, body) gf, Ident funcName) 
        _ -> return (gf, Error UnknownError) 

defineLambda :: Express -> Express -> GlobalFuncs -> IO State 
defineLambda ex body gf =
    return . (gf,) $
    case expressToIdents ex of 
        Just perameters -> Value $ Lambda (perameters, body)
        Nothing  -> Error ValueError

ifConditional :: Express -> Express -> Express -> GlobalFuncs -> IO State 
ifConditional condtion ifCase elseCase = 
    interpretExpress condtion >=> \case  
        (newGf, Value (Bool b)) -> 
            interpretExpress (if b then ifCase else elseCase) newGf
        _ -> return (empty, Error ValueError) 

createList :: [Express] -> GlobalFuncs -> IO State 
createList exs gf = return . (gf,) . Value $ List exs 

car :: Express -> GlobalFuncs -> IO State 
car ex = 
    interpretExpress ex >=> 
    \case
        (newGf, Value (List (h:_))) -> interpretExpress h newGf 
        _ -> return (empty, Error ValueError)  

cdr :: Express -> GlobalFuncs -> IO State 
cdr = 
    tryApplyListFunc (\case 
        (_:ex) -> Value $ List ex
        _ -> Error ValueError)
        
isNull :: Express -> GlobalFuncs -> IO State 
isNull = tryApplyListFunc (Value . Bool . null)
    
cons :: Express -> Express -> GlobalFuncs -> IO State 
cons ex = tryApplyListFunc (Value . List . (:) ex) 

tryApplyListFunc :: ([Express] -> Atom) -> Express -> GlobalFuncs -> IO State 
tryApplyListFunc f ex = 
    interpretExpress ex >=> 
    \case
        (newGf, Value (List lst)) -> return (newGf,f lst)
        _ -> return (empty, Error ValueError)

applyOperator :: Ident -> Express -> Express -> GlobalFuncs -> IO State 
applyOperator ident ex1 ex2 = 
    interpretExpress ex1 >=> \(gf1, a1) -> 
    interpretExpress ex2 gf1 >>= \(gf2, a2) ->
        return (gf2, applyAtomOperator ident a1 a2)

expressToIdents :: Express -> Maybe [Ident]
expressToIdents (Comb exs) = 
    foldl (\acc ex -> (++) <$> acc <*> expressToIdents ex) (Just []) exs
expressToIdents (A (Ident i)) = Just [i]
expressToIdents _ = Nothing 
