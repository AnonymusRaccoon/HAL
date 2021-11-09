module Maths where
import Expressions
import Evaluator

mathEnv :: LispEnv
mathEnv = [
        ("+", ABuiltin "+" evalPlus),
        ("-", ABuiltin "-" evalMinus),
        ("*", ABuiltin "*" evalMult),
        ("div", ABuiltin "div" evalDiv),
        ("mod", ABuiltin "mod" evalMod),
        ("<", ABuiltin "<" evalLessThan)
    ]

evalPlus :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalPlus (x:xs) env = do
    (rest, nEnv) <- evalPlus xs env
    case (x, rest) of
         (AInt f, AInt s) -> Right (AInt $ f + s, nEnv)
         (AFloat f, AFloat s) -> Right (AFloat $ f + s, nEnv)
         (AFloat f, AInt s) -> Right (AFloat $ f + fromIntegral s, nEnv)
         (AInt f, AFloat s) -> Right (AFloat $ fromIntegral f + s, nEnv)
         (value, _) -> Left $ "**Error: \"" ++ show value ++ "\" is not a number **"
evalPlus [] env = Right (AInt 0, env)

evalMinus :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalMinus (x:xs) env = do
    (rest, nEnv) <- evalPlus xs env
    case (x, rest) of
         (AInt f, AInt s) -> Right (AInt $ f - s, nEnv)
         (AFloat f, AFloat s) -> Right (AFloat $ f - s, nEnv)
         (AFloat f, AInt s) -> Right (AFloat $ f - fromIntegral s, nEnv)
         (AInt f, AFloat s) -> Right (AFloat $ fromIntegral f - s, nEnv)
         (value, _) -> Left $ "**Error: \"" ++ show value ++ "\" is not a number **"
evalMinus [] env = Right (AInt 0, env)

evalMult :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalMult (x:xs) env = do
    (rest, nEnv) <- evalPlus xs env
    case (x, rest) of
         (AInt f, AInt s) -> Right (AInt $ f * s, nEnv)
         (AFloat f, AFloat s) -> Right (AFloat $ f * s, nEnv)
         (AFloat f, AInt s) -> Right (AFloat $ f * fromIntegral s, nEnv)
         (AInt f, AFloat s) -> Right (AFloat $ fromIntegral f * s, nEnv)
         (value, _) -> Left $ "**Error: \"" ++ show value ++ "\" is not a number **"
evalMult [] env = Right (AInt 1, env)

evalDiv :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalDiv [_, AInt 0] env = Left "**Error: Division by 0 is undefined.**"
evalDiv [_, AFloat 0] env = Left "**Error: Division by 0 is undefined.**"
evalDiv [AInt f, AInt s] env = Right (AInt (f `quot` s), env)
evalDiv [AFloat f, AFloat s] env = Right (AFloat (f / s), env)
evalDiv [AFloat f, AInt s] env = Right (AFloat (f / fromIntegral s), env)
evalDiv [AInt f, AFloat s] env = Right (AFloat (fromIntegral f / s), env)
evalDiv _ _ = Left "**Error: Invalid arguments in div.**"

evalMod :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalMod [_, AInt 0] env = Left "**Error: mod by 0 is undefined.**"
evalMod [_, AFloat 0] env = Left "**Error: mod by 0 is undefined.**"
evalMod [AInt f, AInt s] env = Right (AInt (f `mod` s), env)
evalMod _ _ = Left "**Error: Invalid arguments in mod.**"

evalLessThan :: [Atom] -> LispEnv -> Either String (Atom, LispEnv)
evalLessThan [AInt f, AInt s] env = Right (_fromBool (f < s), env)
evalLessThan [AFloat f, AFloat s] env = Right (_fromBool (f < s), env)
evalLessThan [AFloat f, AInt s] env = Right (_fromBool (f < fromIntegral s), env)
evalLessThan [AInt f, AFloat s] env = Right (_fromBool (fromIntegral f < s), env)
evalLessThan _ _ = Left "**Error: Invalid arguments in <.**"