module Evaluator where
import Expressions

eval :: Statement -> LispEnv -> Either String (String, LispEnv)
eval (Expr expr) env = evalS expr env
eval (Atom (ASymbol symb)) env = case getSymbolValue symb env of
                                      Just v  -> Right (show v, env)
                                      Nothing -> Left $ "**Error: Variable not bound " ++ symb ++ "**"
eval (Atom atom) env = Right (show atom, env)

getSymbolValue :: String ->  LispEnv -> Maybe Atom
getSymbolValue symbol ((key, value):xs)
    | symbol == key = Just value
    | otherwise     = getSymbolValue symbol xs
getSymbolValue _ [] = Nothing

evalS :: SExpr -> LispEnv -> Either String (String, LispEnv)
evalS (SExpr ((ASymbol "define"):xs)) env = evalDefine xs env
evalS expr _ = Left $ "**Error: couldn't evaluate " ++ show expr ++ "**"

evalDefine :: [Atom] -> LispEnv -> Either String (String, LispEnv)
evalDefine [ASymbol symbol, atom] env = Right (symbol, (symbol, atom):env)
evalDefine [_, _] _ = Left "**Error: the first argument of define must be a symbol."
evalDefine args _ = Left $ "**Error: define expect 2 arguments. " ++ show (length args) ++ " where found.**"