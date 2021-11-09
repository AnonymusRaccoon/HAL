module Evaluator where
import Expressions

eval :: Statement -> LispEnv -> Either String (Atom, LispEnv)
eval (Expr expr) env = evalS expr env
eval (Atom (ASymbol "env")) env = Right (AString $ unwords (fst <$> env), env)
eval (Atom (ASymbol symb)) env =
    case getSymbolValue symb env of
         Just v  -> Right (v, env)
         Nothing -> Left $ "**Error: Variable not bound " ++ symb ++ "**"
eval (Atom (AQuote quoted)) env = Right (quoted, env)
eval (Atom atom) env = Right (atom, env)

getSymbolValue :: String ->  LispEnv -> Maybe Atom
getSymbolValue symbol ((key, value):xs)
    | symbol == key = Just value
    | otherwise     = getSymbolValue symbol xs
getSymbolValue _ [] = Nothing

evalS :: SExpr -> LispEnv -> Either String (Atom, LispEnv)
evalS (SExpr [Atom (ASymbol "type"), x]) env = do
    (atom, nEnv) <- eval x env
    return (AString $ showType atom, nEnv)
evalS (SExpr ((Atom (ASymbol "define")):xs)) env = evalDefine xs env
evalS (SExpr ((Atom (ASymbol "lambda")):xs)) env = evalLambda xs env
evalS (SExpr ((Atom (ASymbol "let")):xs)) env = evalLet xs env
evalS (SExpr ((Atom (ASymbol "cond")):xs)) env = evalCond xs env
evalS (SExpr ((Atom (AProcedure n dArgs body)):args)) env = do
    localEnv <- setupLocalVars dArgs args env
    (ret, _) <- evalS body localEnv
    return (ret, env)
evalS (SExpr ((Atom (ABuiltin n func)):args)) env = evalBuiltin func args env
evalS (SExpr ((Atom (ASymbol name)):xs)) env = 
    case getSymbolValue name env of
         Just atom -> evalS (SExpr (Atom atom:xs)) env
         Nothing   -> Left $ "**Error: Variable not bound " ++ name ++ "**"
evalS (SExpr (Expr nested:args)) env = do
    (atom, nEnv) <- evalS nested env
    evalS (SExpr (Atom atom:args)) nEnv
evalS expr _ = Left $ "**Error: couldn't evaluate " ++ show expr ++ "**"

evalSymbol :: String -> [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalSymbol name args env = 
    case getSymbolValue name env of
         (Just (ABuiltin n func)) -> evalBuiltin func args env
         (Just (AProcedure n dArgs body)) -> do
             nEnv <- setupLocalVars dArgs args env
             evalS body nEnv
         (Just (ASymbol symb)) -> evalSymbol symb args env
         (Just atom) -> Left $ "**Error: " ++ show atom ++ " is not a procedure.**"
         Nothing -> Left $ "**Error: Variable not bound " ++ name ++ "**"

evalBuiltin :: ProcedureFunc -> [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalBuiltin func args env = do
    (nArgs, nEnv) <- evalArgs args env
    func nArgs nEnv
    where
        evalArgs :: [Statement] -> LispEnv -> Either String ([Atom], LispEnv)
        evalArgs (x:xs) env = do
            (first, nEnv) <- eval x env
            (lo, lEnv) <- evalArgs xs nEnv
            return (first:lo, lEnv)
        evalArgs [] env = pure ([], env)


showType :: Atom -> String
showType (AQuote v)      = "(Quote " ++ show v ++ ")"
showType (AString v)     = "(String " ++ v ++ ")"
showType (ASymbol v)     = "(Symbol " ++ v ++ ")"
showType (AInt v)        = "(Int " ++ show v ++ ")"
showType (AFloat v)      = "(Float " ++ show v ++ ")"
showType ANil            = "(Nil)"
showType v@(ACons _ _)   = "(Cons " ++ show v ++ ")"
showType v@AProcedure {} = "(Procedure " ++ show v ++ ")"
showType v@ABuiltin {}   = "(Builtin " ++ show v ++ ")"
showType ATrue           = "(True #t)"
showType AFalse          = "(False #f)"
showType ANothing        = "(Nothing)"

setupLocalVars :: [String] -> [Statement] -> LispEnv -> Either String LispEnv
setupLocalVars (n:names) (v:values) env = do
    (value, nEnv) <- eval v env
    rest <- setupLocalVars names values nEnv
    return ((n, value):rest)
setupLocalVars [] [] env = Right env
setupLocalVars names [] env = Left "**Error: not enought arguments in procedure call**"
setupLocalVars [] values env = Left "**Error: too many arguments in procedure call**"

_toArgsList :: [Statement] -> Either String [String]
_toArgsList (Atom (ASymbol x):xs) = do
    rest <- _toArgsList xs
    return $ x:rest
_toArgsList (x:xs) = Left $ "**Error: " ++ show x ++ " is not a valid argument.**"
_toArgsList [] = Right []

evalDefine :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalDefine [Atom at@(ASymbol symbol), lo] env = do
    (atom, nEnv) <- eval lo env
    return (at, (symbol, atom):nEnv)
evalDefine [Expr (SExpr (Atom (ASymbol name):args)), Expr body] env = do
    pArgs <- _toArgsList args
    return (ASymbol name, (name, AProcedure name pArgs body):env)
evalDefine [_, _] _ = Left "**Error: the first argument of define must be a symbol or a list.**"
evalDefine args _ = Left $ "**Error: define expect 2 arguments. " ++ show (length args) ++ " where found.**"

evalLambda :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalLambda [Expr (SExpr args), Expr body] env = do
    pArgs <- _toArgsList args
    return (AProcedure "" pArgs body, env)
evalLambda _ _ = Left "**Error: invalid arguments to lambda.**"

evalLet :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalLet [Expr (SExpr vars), Expr body] env = do
    nEnv <- evaluateVars vars env
    evalS body nEnv
    where
        evaluateVars :: [Statement] -> LispEnv -> Either String LispEnv
        evaluateVars (Expr (SExpr [Atom (ASymbol name), body]):xs) env = do
            (atom, nEnv) <- eval body env
            rest <- evaluateVars xs nEnv
            return ((name, atom):rest)
        evaluateVars [] env = Right env
        evaluateVars (x:_) env = Left $ "**Error: invalid variable definition" ++ show x ++ " in let.**"
evalLet _ _ = Left "**Error: invalid syntax for let.**"

evalCond :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalCond (Expr (SExpr [cond, value]):xs) env = do
    (res, nEnv) <- eval cond env
    case res of
         AFalse -> evalCond xs nEnv
         _ -> eval value nEnv
evalCond [] env = Right (ANothing, env)
evalCond _ _ = Left "**Error: Invalid syntax for cond.**"
