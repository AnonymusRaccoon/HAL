module Maths where
import Expressions
import Evaluator

mathEnv :: LispEnv
mathEnv = [
        ("+", ABuiltin "+" evalPlus),
        ("-", ABuiltin "-" evalMinus),
        ("*", ABuiltin "*" evalMult),
        ("div", ABuiltin "div" evalDiv),
        ("mod", ABuiltin "mod" evalMod)
    ]

evalPlus :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalPlus (x:xs) env = do
    (value, nEnv) <- eval x env
    (rest, nnEnv) <- evalPlus xs nEnv
    case (value, rest) of
         (AInt f, AInt s) -> Right (AInt $ f + s, nnEnv)
         (AFloat f, AFloat s) -> Right (AFloat $ f + s, nnEnv)
         (AFloat f, AInt s) -> Right (AFloat $ f + fromIntegral s, nnEnv)
         (AInt f, AFloat s) -> Right (AFloat $ fromIntegral f + s, nnEnv)
         (value, _) -> Left $ "**Error: \"" ++ show value ++ "\" is not a number **"
evalPlus [] env = Right (AInt 0, env)

evalMinus :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalMinus (x:xs) env = do
    (value, nEnv) <- eval x env
    (rest, nnEnv) <- evalPlus xs nEnv
    case (value, rest) of
         (AInt f, AInt s) -> Right (AInt $ f - s, nnEnv)
         (AFloat f, AFloat s) -> Right (AFloat $ f - s, nnEnv)
         (AFloat f, AInt s) -> Right (AFloat $ f - fromIntegral s, nnEnv)
         (AInt f, AFloat s) -> Right (AFloat $ fromIntegral f - s, nnEnv)
         (value, _) -> Left $ "**Error: \"" ++ show value ++ "\" is not a number **"
evalMinus [] env = Right (AInt 0, env)

evalMult :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalMult (x:xs) env = do
    (value, nEnv) <- eval x env
    (rest, nnEnv) <- evalPlus xs nEnv
    case (value, rest) of
         (AInt f, AInt s) -> Right (AInt $ f * s, nnEnv)
         (AFloat f, AFloat s) -> Right (AFloat $ f * s, nnEnv)
         (AFloat f, AInt s) -> Right (AFloat $ f * fromIntegral s, nnEnv)
         (AInt f, AFloat s) -> Right (AFloat $ fromIntegral f * s, nnEnv)
         (value, _) -> Left $ "**Error: \"" ++ show value ++ "\" is not a number **"
evalMult [] env = Right (AInt 1, env)

evalDiv :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalDiv [_, Atom (AInt 0)] env = Left "**Error: Division by 0 is undefined.**"
evalDiv [_, Atom (AFloat 0)] env = Left "**Error: Division by 0 is undefined.**"
evalDiv [Atom (AInt f), Atom (AInt s)] env = Right (AInt (f `div` s), env)
evalDiv [Atom (AFloat f), Atom (AFloat s)] env = Right (AFloat (f / s), env)
evalDiv [Atom (AFloat f), Atom (AInt s)] env = Right (AFloat (f / fromIntegral s), env)
evalDiv [Atom (AInt f), Atom (AFloat s)] env = Right (AFloat (fromIntegral f / s), env)
evalDiv [Expr expr, other] env = do
    (res, nEnv) <- evalS expr env
    evalDiv [Atom res, other] nEnv
evalDiv [other, Expr expr] env = do
    (res, nEnv) <- evalS expr env
    evalDiv [other, Atom res] nEnv
evalDiv _ _ = Left "**Error: Invalid arguments in div.**"

evalMod :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalMod [_, Atom (AInt 0)] env = Left "**Error: mod by 0 is undefined.**"
evalMod [_, Atom (AFloat 0)] env = Left "**Error: mod by 0 is undefined.**"
evalMod [Atom (AInt f), Atom (AInt s)] env = Right (AInt (f `mod` s), env)
evalMod [Expr expr, other] env = do
    (res, nEnv) <- evalS expr env
    evalMod [Atom res, other] nEnv
evalMod [other, Expr expr] env = do
    (res, nEnv) <- evalS expr env
    evalMod [other, Atom res] nEnv
evalMod _ _ = Left "**Error: Invalid arguments in mod.**"