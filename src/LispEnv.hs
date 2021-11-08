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
        ("atom?", ABuiltin "atom?" evalAtom),
        ("cond", ABuiltin "cond" evalCond)
    ] ++ mathEnv

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


_fromBool :: Bool -> Atom
_fromBool True  = ATrue
_fromBool False = AFalse

evalEq :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalEq [Atom (AInt f), Atom (AInt s)] env = Right (_fromBool (f == s), env)
evalEq [Atom (ASymbol f), Atom (ASymbol s)] env = Right (_fromBool (f == s), env)
evalEq [Atom (AString f), Atom (AString s)] env = Right (AFalse, env) -- TODO Should do a reference equal
evalEq [Atom (AFloat f), Atom (AFloat s)] env = Right (_fromBool(f == s), env)
evalEq [Atom (ACons _ _), _] env = Right (AFalse, env)
evalEq [Atom ANil, Atom ANil] env = Right (ATrue, env)
evalEq [Atom (AProcedure f _ _), Atom (AProcedure s _ _)] env = Right (_fromBool (f == s), env)
evalEq [Atom (ABuiltin f _), Atom (ABuiltin s _)] env = Right (_fromBool (f == s), env)
evalEq [Atom ATrue, Atom ATrue] env = Right (ATrue, env)
evalEq [Atom AFalse, Atom AFalse] env = Right (ATrue, env)
evalEq [Expr exp, other] env = do
    (first, nEnv) <- evalS exp env
    evalEq [Atom first, other] nEnv
evalEq [other, Expr exp] env = do
    (first, nEnv) <- evalS exp env
    evalEq [Atom first, other] nEnv
evalEq [Atom (AQuote quoted), other] env = evalEq [Atom quoted, other] env
evalEq [other, Atom (AQuote quoted)] env = evalEq [Atom quoted, other] env
evalEq _ _ = Left "**Error: Invalid arguments in eq?**"

evalAtom :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalAtom [Atom (ACons _ _)] env = Right (AFalse, env)
evalAtom [Atom (AQuote quoted)] env = evalAtom [Atom quoted] env
evalAtom [Expr expr] env = do
    (atm, nEnv) <- evalS expr env
    evalAtom [Atom atm] nEnv
evalAtom [Atom _] env = Right (ATrue, env)
evalAtom _ _ = Left "**Error: Invalid arguments in atom?**"

evalCond :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalCond (Expr (SExpr [cond, value]):xs) env = do
    (res, nEnv) <- eval cond env
    case res of
         AFalse -> evalCond xs nEnv
         _ -> eval value nEnv
evalCond [] env = Right (ANothing, env)
evalCond _ _ = Left "**Error: Invalid syntax for cond.**"