{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (interpretProgram, interpretExpress) where 

import Types
import AtomOperators (applyAtomOperator)
import Parser (parseProgram)
import Data.Map (empty, (!?), insert)
import Control.Monad (void)

interpretProgram :: String -> IO () 
interpretProgram input = 
        case parseProgram input of  
            Just program -> runProgram initFuncs program
            Nothing -> putStr "Syntax Error\n"
    
runProgram :: GlobalFuncs -> Program -> IO ()
runProgram _ [] =  return ()
runProgram funcs (a:as) = 
    interpretExpress funcs a >>= 
    \case 
        (_, Error e) -> void . putStr $ show e
        (newFuncs, _) -> runProgram newFuncs as  

interpretExpress :: GlobalFuncs -> Express -> IO State   
interpretExpress gf (Comb (A(Ident funcName):args)) = callFunction gf funcName args
interpretExpress gf (Comb [ex]) = interpretExpress gf ex 
interpretExpress gf (A atom) = interpretAtom gf atom 
interpretExpress gf _ = return (gf, Error UnknownError)

interpretAtom :: GlobalFuncs -> Atom -> IO State 
interpretAtom gf (Value v) = return (gf, Value v)
interpretAtom gf (Ident funcName) = callFunction gf funcName [] 
interpretAtom gf (Error err) = return (gf, Error err)

callFunction :: GlobalFuncs -> Ident -> [Express] -> IO State 
callFunction funcs funcName args = 
    case funcs !? funcName of 
        Nothing -> preDefinedFuncs funcs funcName args
        Just (perameters, express) ->
            interpretExpress funcs $ bindArguments perameters args express  

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

preDefinedFuncs :: GlobalFuncs -> Ident -> [Express] -> IO State 
preDefinedFuncs gf "display" [ex] = display gf ex 
preDefinedFuncs gf "define" [perameters, body] = defineFunction gf perameters body 
preDefinedFuncs gf "if" [condition, thenCase, elseCase] = ifConditional gf condition thenCase elseCase
preDefinedFuncs gf "list" exs = createList gf exs
preDefinedFuncs gf "car" [ex] = car gf ex
preDefinedFuncs gf "cdr" [ex] = cdr gf ex
preDefinedFuncs gf "null?" [ex] = isNull gf ex  
preDefinedFuncs gf "cons" [ex1, ex2] = cons gf ex1 ex2
preDefinedFuncs gf ident [ex1, ex2] = applyOperator gf ident ex1 ex2
preDefinedFuncs gf ident _ = return (gf, Error $ UnboundVariable ident )

display :: GlobalFuncs -> Express -> IO State
display gf ex = 
    interpretExpress gf ex >>= \(newGf, atom) -> 
    putStr (show atom ++ "\n") >> return (newGf, atom) 

defineFunction :: GlobalFuncs -> Express -> Express -> IO State 
defineFunction gf perameters body =
    case expressToIdents perameters of 
        Just (funcName:parameterNames) ->
             return (insert funcName (parameterNames, body) gf, Ident funcName) 
        _ -> return (gf, Error UnknownError) 

ifConditional :: GlobalFuncs -> Express -> Express -> Express -> IO State 
ifConditional gf condtion ifCase elseCase = 
    interpretExpress gf condtion >>= \case  
        (newGf, Value (Bool b)) -> interpretExpress newGf $ if b then ifCase else elseCase
        _ -> return (gf, Error ValueError) 

createList :: GlobalFuncs -> [Express] -> IO State 
createList gf = return . (,) gf . Value . List 

car :: GlobalFuncs -> Express -> IO State 
car gf ex = 
    interpretExpress gf ex >>= 
    \case
        (newGf, Value (List (h:_))) -> interpretExpress newGf h 
        _ -> return (gf, Error ValueError)  

cdr :: GlobalFuncs -> Express -> IO State 
cdr gf ex = 
    interpretExpress gf ex >>= 
    \case
        (newGf, Value (List (_:t))) -> return (newGf, Value $ List t)
        _ -> return (gf, Error ValueError)  

isNull :: GlobalFuncs -> Express  -> IO State 
isNull gf ex = 
    interpretExpress gf ex >>= 
    \case
        (newGf, Value (List lst)) -> return (newGf, Value . Bool $ null lst )
        _ -> return (gf, Error ValueError)  

cons :: GlobalFuncs -> Express -> Express -> IO State 
cons gf ex listEx = 
    interpretExpress gf listEx >>= 
    \case
        (newGf, Value (List lst)) -> return (newGf, Value . List $ ex:lst)
        _ -> return (gf, Error ValueError)

applyOperator :: GlobalFuncs -> Ident -> Express -> Express -> IO State 
applyOperator gf1 ident ex1 ex2 = 
    interpretExpress gf1 ex1 >>= \(gf2, a1) -> 
    interpretExpress gf2 ex2 >>= \(gf3, a2) ->
        return (gf3, applyAtomOperator ident a1 a2)

expressToIdents :: Express -> Maybe [Ident]
expressToIdents (Comb exs) = 
    foldl (\acc ex -> (++) <$> acc <*> expressToIdents ex) (Just []) exs
expressToIdents (A (Ident i)) = Just [i]
expressToIdents _ = Nothing 
