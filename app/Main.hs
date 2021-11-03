module Main where

import Lib
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import BasicParser
import LispParser

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine " > "
           case minput of
               Nothing -> return ()
               Just "exit" -> return ()
               Just "quit" -> return ()
               Just input -> (outputStrLn . show $ parse pLisp input) >> loop
