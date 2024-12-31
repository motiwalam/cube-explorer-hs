module Main (main) where

import Control.Exception (catch)
import System.IO.Error (isEOFError)
import System.Exit
import System.Environment (getArgs)

import CubeInteract
import CubeExplorerClient (generatorForPartialDesc)

main :: IO ()
main = do
    args <- getArgs
    moves <- generatorForPartialDesc (args !! 0)
    print moves
-- main = cubeRepl `catch` (\e ->
--     if isEOFError e
--     then putStrLn "" >>= const exitSuccess
--     else die $ "Error: " ++ show e)
