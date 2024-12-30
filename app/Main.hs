module Main (main) where

import Control.Exception (catch)
import System.IO.Error (isEOFError)
import System.Exit

import CubeInteract

main :: IO ()
main = cubeRepl `catch` (\e ->
    if isEOFError e
    then putStrLn "" >>= const exitSuccess
    else die $ "Error: " ++ show e)
