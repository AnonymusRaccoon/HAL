module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import BasicParser
import LispParser
import Evaluator
import LispEnv
import Expressions
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode (ExitFailure))
import Control.Exception (IOException, catch)

runRepl :: LispEnv -> IO ()
runRepl env = runInputT defaultSettings (loop env)
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
                          (Right (ANothing, env)) -> return env
                          (Right (at, env)) -> outputStrLn (show at) >> return env
                          (Left err) -> outputStrLn err >> return env
                 (_, lo) -> outputStrLn ("**Error: Invalid syntax near: " ++ lo ++ "**")
                            >> return env

evalFiles :: [String] -> LispEnv -> Either String (Atom, LispEnv)
evalFiles [x] env = evalFile x env
evalFiles (x:xs) env = do
    (ret, nEnv) <- evalFile x env
    evalFiles xs nEnv
evalFiles [] env = Right (ANothing, env)

evalFile :: String -> LispEnv -> Either String (Atom, LispEnv)
evalFile file env =
    case parse pLisp file of
            (Just statements, []) -> evalStatements statements env
            (_, lo)               -> Left $ "**Parse error near " ++ lo ++ "**"

evalStatements :: [Statement] -> LispEnv -> Either String (Atom, LispEnv)
evalStatements [x] env = eval x env
evalStatements (x:xs) env = do
    (ret, nEnv) <- eval x env
    evalStatements xs nEnv
evalStatements [] env = Right (ANothing, env)


main :: IO ()
main = do
    (args, repl) <- parseArgs <$> getArgs
    files <- catch (sequence $ readFile <$> args) handler
    case evalFiles files defaultEnv of
         Right (ANothing, env) -> if repl then runRepl env else exitSuccess
         Right (ret, env)      -> if repl then runRepl env else print ret
         Left err              -> putStrLn err >> exitWith (ExitFailure 84)
    where
        handler :: IOException -> IO [String]
        handler e = putStrLn ("Error: " ++ show e) >> exitWith (ExitFailure 84)

        parseArgs :: [String] -> ([String], Bool)
        parseArgs ("-i":xs) = let (files, _) = parseArgs xs
                              in (files, True)
        parseArgs [x]       = ([x], False)
        parseArgs (x:xs)    = let (files, repl) = parseArgs xs
                              in (x:files, repl)
        parseArgs []        = ([], True)