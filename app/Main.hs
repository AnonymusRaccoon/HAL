module Main where

import Lib
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import BasicParser
import LispParser
import Expressions
import Evaluator

main :: IO ()
main = runInputT defaultSettings (loop $ LispEnv [])
   where
        loop :: LispEnv -> InputT IO ()
        loop env = do
            minput <- getInputLine " > "
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just "quit" -> return ()
                Just input -> evalInput input env >>= loop
        
        evalInput :: String -> LispEnv -> InputT IO LispEnv
        evalInput input env =
            case parse pSExpr input of
                 (Just res, []) -> let (str, env) = eval res env
                                   in outputStrLn str >> return env
                 (_, lo) -> outputStrLn ("**Error: Invalid syntax near: " ++ lo ++ "**")
                            >> return env
