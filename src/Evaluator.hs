module Evaluator where
import Expressions

eval :: SExpr -> LispEnv -> Either String (String, LispEnv)
eval (SExpr ((ASymbol "define"):xs)) env = evalDefine xs env
eval expr _ = Left $ "**Error: couldn't evaluate " ++ show expr ++ "**"

evalDefine :: [Atom] -> LispEnv -> Either String (String, LispEnv)
evalDefine [ASymbol symbol, atom] env = Right (symbol, (symbol, atom):env)
evalDefine [_, _] _ = Left "**Error: the first argument of define must be a symbol."
evalDefine args _ = Left $ "**Error: define expect 2 arguments. " ++ show (length args) ++ " where found.**"