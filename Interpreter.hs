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
        (_, Error e) -> void . putStr $ show e ++ "\n"
        (newFuncs, _) -> runProgram newFuncs as  

interpretExpress :: GlobalFuncs -> Express -> IO State   
interpretExpress gf (Comb (A(Ident funcName):args)) = callFunction gf funcName args
interpretExpress gf (Comb (A(Value(Lambda lambda)):args)) = callLambda gf lambda args 
interpretExpress gf (Comb exs) = interpretFirst gf exs 
interpretExpress gf (A atom) = interpretAtom gf atom 

interpretFirst :: GlobalFuncs -> [Express] -> IO State 
interpretFirst gf [ex] = interpretExpress gf ex 
interpretFirst gf (ex:exs) =
    interpretExpress gf ex >>= \(newGf, atom) -> 
    interpretExpress newGf (Comb $ A atom:exs)
interpretFirst gf [] = return (gf, Error UnknownError )

interpretAtom :: GlobalFuncs -> Atom -> IO State 
interpretAtom gf (Ident funcName) = callFunction gf funcName [] 
interpretAtom gf atom = return (gf, atom)

callFunction :: GlobalFuncs -> Ident -> [Express] -> IO State 
callFunction gf funcName args = 
    case gf !? funcName of 
        Nothing -> preDefinedFuncs gf funcName args
        Just  lambda -> callLambda gf lambda args

callLambda :: GlobalFuncs -> Lambda -> [Express] -> IO State
callLambda gf (perameters, body) args = 
     interpretExpress gf $ bindArguments perameters args body

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
preDefinedFuncs gf "newline" [] = putStr "\n" >> return (gf, Value $ List [] )
preDefinedFuncs gf ident [ex] = oneArgFunc ident gf ex 
preDefinedFuncs gf ident [ex1, ex2] = twoArgFunc ident gf ex1 ex2 
preDefinedFuncs gf "if" [condition, thenCase, elseCase] = ifConditional gf condition thenCase elseCase
preDefinedFuncs gf "list" exs = createList gf exs
preDefinedFuncs gf ident _ = return (gf, Error $ UnboundVariable ident )

oneArgFunc :: Ident -> GlobalFuncs -> Express -> IO State
oneArgFunc "display" = display
oneArgFunc "car" = car
oneArgFunc "cdr" = cdr 
oneArgFunc "null?" = isNull
oneArgFunc ident = \_ _ -> return (initFuncs , Error $ UnboundVariable ident )

twoArgFunc :: Ident -> GlobalFuncs  -> Express -> Express -> IO State 
twoArgFunc "define" = defineFunction
twoArgFunc "cons" = cons 
twoArgFunc "lambda" = defineLambda
twoArgFunc op = applyOperator op 

display :: GlobalFuncs -> Express -> IO State
display gf ex = 
    interpretExpress gf ex >>= 
    \case
        (newGf, Value (List l)) -> displayList newGf l 
        (newGf, atom) -> putStr (show atom) >> return (newGf, atom) 

displayList :: GlobalFuncs -> [Express] -> IO State
displayList gf exs =
    putStr "( " >> 
        foldl (\io ex ->
            io >>= \accGf ->  
            display accGf ex >>= \(newGf, _) -> 
            putStr " " >> return newGf) 
        (return gf) exs >>= 
            \newGf ->
                putStr ")" >> return (newGf, Value $ List exs)
    
defineFunction :: GlobalFuncs -> Express -> Express -> IO State 
defineFunction gf perameters body =
    case expressToIdents perameters of 
        Just (funcName:parameterNames) ->
             return (insert funcName (parameterNames, body) gf, Ident funcName) 
        _ -> return (gf, Error UnknownError) 

defineLambda :: GlobalFuncs -> Express -> Express -> IO State 
defineLambda gf ex body = 
    return . (,) gf $
    case expressToIdents ex of 
        Just perameters -> Value $ Lambda (perameters, body)
        Nothing  -> Error ValueError

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
cdr = 
    tryApplyListFunc (\case 
        (_:ex) -> Value $ List ex
        _ -> Error ValueError)
        
isNull :: GlobalFuncs -> Express  -> IO State 
isNull = tryApplyListFunc (Value . Bool . null)
    
cons :: GlobalFuncs -> Express -> Express -> IO State 
cons gf ex = tryApplyListFunc (Value . List . (:) ex) gf 

tryApplyListFunc :: ([Express] -> Atom) -> GlobalFuncs -> Express -> IO State 
tryApplyListFunc f gf ex = 
    interpretExpress gf ex >>= 
    \case
        (newGf, Value (List lst)) -> return (newGf,f lst)
        _ -> return (gf, Error ValueError)

applyOperator :: Ident -> GlobalFuncs -> Express -> Express -> IO State 
applyOperator ident gf1 ex1 ex2 = 
    interpretExpress gf1 ex1 >>= \(gf2, a1) -> 
    interpretExpress gf2 ex2 >>= \(gf3, a2) ->
        return (gf3, applyAtomOperator ident a1 a2)

expressToIdents :: Express -> Maybe [Ident]
expressToIdents (Comb exs) = 
    foldl (\acc ex -> (++) <$> acc <*> expressToIdents ex) (Just []) exs
expressToIdents (A (Ident i)) = Just [i]
expressToIdents _ = Nothing 
