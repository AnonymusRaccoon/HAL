module Evaluator where
import Expressions

eval :: Statement -> LispEnv -> Either String (Atom, LispEnv)
eval (Expr expr) env = evalS expr env
eval (Atom (ASymbol symb)) env =
    case getSymbolValue symb env of
         Just v  -> Right (v, env)
         Nothing -> Left $ "**Error: Variable not bound " ++ symb ++ "**"
eval (Atom atom) env = Right (atom, env)

getSymbolValue :: String ->  LispEnv -> Maybe Atom
getSymbolValue symbol ((key, value):xs)
    | symbol == key = Just value
    | otherwise     = getSymbolValue symbol xs
getSymbolValue _ [] = Nothing

evalS :: SExpr -> LispEnv -> Either String (Atom, LispEnv)
evalS (SExpr ((Atom (ASymbol "define")):xs)) env = evalDefine xs env
evalS (SExpr ((Atom (ASymbol "cons")):xs)) env = evalCons xs env
evalS (SExpr ((Atom (ASymbol "car")):xs)) env = evalCar xs env
evalS expr _ = Left $ "**Error: couldn't evaluate " ++ show expr ++ "**"

evalDefine :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalDefine [Atom at@(ASymbol symbol), lo] env = do
    (atom, nEnv) <- eval lo env
    return (at, (symbol, atom):nEnv)
evalDefine [_, _] _ = Left "**Error: the first argument of define must be a symbol.**"
evalDefine args _ = Left $ "**Error: define expect 2 arguments. " ++ show (length args) ++ " where found.**"

evalCons :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalCons [first, second] env = do
    (fAtom, fEnv) <- eval first env
    (sAtom, sEnv) <- eval second fEnv
    return (ACons fAtom sAtom, sEnv)
evalCons args _ = Left $ "**Error: cons expect 2 arguments. " ++ show (length args) ++ " where found.**"

evalCar :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalCar [Atom (ACons f _)] env = Right (f, env)
evalCar [Expr expr] env = do
    (atom, nEnv) <- evalS expr env
    case atom of
        (ACons f _) -> return (f, nEnv)
        atom        -> Left $ "**Error: " ++ show atom ++ " is not a pair.**"
evalCar [Atom bad] _ = Left $ "**Error: " ++ show bad ++ " is not a pair.**"
evalCar _ _ = Left "**Error: incorect argument count in car**"