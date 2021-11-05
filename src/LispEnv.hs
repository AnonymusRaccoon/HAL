module LispEnv where
import Expressions
import Evaluator


defaultEnv :: LispEnv
defaultEnv = [
        ("cons", ABuiltin "cons" evalCons),
        ("car", ABuiltin "car" evalCar),
        ("cdr", ABuiltin "cdr" evalCdr)
    ]


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

evalCdr :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalCdr [Atom (ACons _ s)] env = Right (s, env)
evalCdr [Expr expr] env = do
    (atom, nEnv) <- evalS expr env
    case atom of
        (ACons _ s) -> return (s, nEnv)
        atom        -> Left $ "**Error: " ++ show atom ++ " is not a pair.**"
evalCdr [Atom bad] _ = Left $ "**Error: " ++ show bad ++ " is not a pair.**"
evalCdr _ _ = Left "**Error: incorect argument count in cdr**"