module Maths where
import Expressions
import Evaluator

mathEnv :: LispEnv
mathEnv = [
        ("+", ABuiltin "+" evalPlus)
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
         (value, _) -> Left $ "**Error: \"" ++ show value ++"\" is not a number **"
evalPlus [] env = Right (AInt 0, env)
