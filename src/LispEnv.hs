module LispEnv where
import Expressions
import Evaluator
import Maths


defaultEnv :: LispEnv
defaultEnv = [
        ("cons", ABuiltin "cons" evalCons),
        ("car", ABuiltin "car" evalCar),
        ("cdr", ABuiltin "cdr" evalCdr),
        ("eq?", ABuiltin "eq?" evalEq),
        ("atom?", ABuiltin "atom?" evalAtom)
    ] ++ mathEnv

evalCons :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalCons [first, second] env = Right (ACons first second, env)
evalCons args _ = Left $ "**Error: cons expect 2 arguments. " ++ show (length args) ++ " where found.**"

evalCar :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalCar [ACons f _] env = Right (f, env)
evalCar [bad] _ = Left $ "**Error: " ++ show bad ++ " is not a pair.**"
evalCar _ _ = Left "**Error: incorect argument count in car**"

evalCdr :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalCdr [ACons _ s] env = Right (s, env)
evalCdr [bad] _ = Left $ "**Error: " ++ show bad ++ " is not a pair.**"
evalCdr _ _ = Left "**Error: incorect argument count in cdr**"

evalEq :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalEq [AInt f, AInt s] env = Right (_fromBool (f == s), env)
evalEq [ASymbol f, ASymbol s] env = Right (_fromBool (f == s), env)
evalEq [AString f, AString s] env = Right (AFalse, env) -- TODO Should do a reference equal
evalEq [AFloat f, AFloat s] env = Right (_fromBool(f == s), env)
evalEq [ACons _ _, _] env = Right (AFalse, env)
evalEq [ANil, ANil] env = Right (ATrue, env)
evalEq [AProcedure f _ _, AProcedure s _ _] env = Right (_fromBool (f == s), env)
evalEq [ABuiltin f _, ABuiltin s _] env = Right (_fromBool (f == s), env)
evalEq [ATrue, ATrue] env = Right (ATrue, env)
evalEq [AFalse, AFalse] env = Right (ATrue, env)
evalEq [AQuote quoted, other] env = evalEq [quoted, other] env
evalEq [other, AQuote quoted] env = evalEq [other, quoted] env
evalEq [_, _] env = Right (AFalse, env)
evalEq _ _ = Left "**Error: Invalid arguments in eq?**"

evalAtom :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalAtom [ACons _ _] env = Right (AFalse, env)
evalAtom [AQuote quoted] env = evalAtom [quoted] env
evalAtom [_] env = Right (ATrue, env)
evalAtom _ _ = Left "**Error: Invalid arguments in atom?**"