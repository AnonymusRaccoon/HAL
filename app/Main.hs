module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import BasicParser
import LispParser
import Expressions
import Evaluator
import LispEnv

main :: IO ()
main = runInputT defaultSettings (loop defaultEnv)
   where
        loop :: LispEnv -> InputT IO ()
        loop env = do
            minput <- getInputLine "> "
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just "quit" -> return ()
                Just input -> evalInput input env >>= loop
        
        evalInput :: String -> LispEnv -> InputT IO LispEnv
        evalInput input env =
            case parse pStatement input of
                 (Just res, []) ->
                     case eval res env of
                          (Right (str, env)) -> outputStrLn (show str) >> return env
                          (Left err) -> outputStrLn err >> return env
                 (_, lo) -> outputStrLn ("**Error: Invalid syntax near: " ++ lo ++ "**")
                            >> return env
