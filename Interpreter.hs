{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (interpretProgram, interpretExpress) where 

import Types
import Parser (parseProgram)
import Data.Map (empty, (!?), insert)

interpretProgram :: String -> IO () 
interpretProgram input = 
        case parseProgram input of  
            Just program -> runProgram initFuncs program
            Nothing -> putStr "Syntax Error\n"
    
runProgram :: GlobalFuncs -> Program -> IO ()
runProgram _ [] =  return ()
runProgram funcs (a:as) = 
    interpretExpress funcs a >>= 
        \(newFuncs, _) -> runProgram newFuncs as  

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
             return (insert funcName (parameterNames, body) gf, Error ValueError) 
        _ -> return (gf, Error UnknownError) 

ifConditional :: GlobalFuncs -> Express -> Express -> Express -> IO State 
ifConditional gf condtion ifCase elseCase = 
    interpretExpress gf condtion >>= \case  
        (newGf, Value (Bool b)) -> interpretExpress newGf $ if b then ifCase else elseCase
        _ -> return (gf, Error ValueError) 

applyOperator :: GlobalFuncs -> Ident -> Express -> Express -> IO State 
applyOperator gf ident ex1 ex2 = 
    (\case 
        Nothing -> return (gf, Error $ UnboundVariable ident)
        Just f -> 
            interpretExpress gf ex1 >>= \(_, a1) -> 
            interpretExpress gf ex2 >>= \(_,a2) -> 
            return (gf, f a1 a2) 
    ) $ twoArgFunc ident 

twoArgFunc :: Ident -> Maybe (Atom -> Atom -> Atom)
twoArgFunc ident = 
    foldl (\acc new -> 
        case acc of 
            Nothing -> new ident
            Just _ -> acc     
        ) Nothing [intFuncs, boolFuncs]

intFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
intFuncs ident = 
    wrapAdamIntFunc <$>
        case ident of 
            "+" -> Just (+)
            "-" -> Just (-)
            "*" -> Just (*)
            "/" -> Just div
            _ -> Nothing 

boolFuncs :: Ident -> Maybe (Atom -> Atom -> Atom)
boolFuncs ident =
    wrapAdamBoolFunc <$> 
        case ident of 
            ">" -> Just (>)
            "<" -> Just (<)
            "=" -> Just (==)
            ">=" -> Just (>=)
            "<=" -> Just (<=)
            "/=" -> Just (/=)
            _ -> Nothing 

wrapAdamIntFunc :: (Int -> Int -> Int) -> (Atom -> Atom -> Atom)
wrapAdamIntFunc f a1 a2 = case (a1, a2) of 
    (Value (Int i1),Value (Int i2)) -> Value . Int $ f i1 i2
    _ -> carryOverError ValueError [a1, a2] 

wrapAdamBoolFunc :: (Int  -> Int -> Bool) -> (Atom -> Atom -> Atom)
wrapAdamBoolFunc f a1 a2 = case (a1, a2) of 
    (Value (Int b1),Value (Int b2)) -> Value . Bool $ f b1 b2
    _ -> carryOverError ValueError [a1, a2] 

expressToIdents :: Express -> Maybe [Ident]
expressToIdents (Comb exs) = 
    foldl (\acc ex -> (++) <$> acc <*> expressToIdents ex) (Just []) exs
expressToIdents (A (Ident i)) = Just [i]
expressToIdents _ = Nothing 

carryOverError :: Error -> [Atom] -> Atom 
carryOverError fallback = 
    Error 
    . foldl (\acc new -> case new of 
        Error e -> e
        _ -> acc) fallback