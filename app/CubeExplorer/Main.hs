module CubeExplorer.Main (main) where

import Control.Exception (catch)
import System.IO.Error (isEOFError)
import System.Exit
import System.Environment (getArgs, getProgName)

import CubeInteract
import CubeExplorerClient (generatorForPartialDesc, faceOrder)
import Cube (applyAlgorithm, solvedCube, cubeDescColor)
import Data.List (intercalate)

usage :: String -> String
usage arg0 = "USAGE:\n" ++
    arg0 ++ " repl    ; interactive Rubik's cube\n" ++
    arg0 ++ " <cube-desc>    ; print generator for <cube-desc>\n"

printUsage :: IO ()
printUsage = getProgName >>= return . usage >>= putStrLn
 
gracefulRepl :: IO ()
gracefulRepl = cubeRepl `catch` (\e ->
    if isEOFError e
    then putStrLn "" >>= const exitSuccess
    else die $ "Error: " ++ show e)

completeCubeFailedMsg :: String -> String
completeCubeFailedMsg desc = 
    "Failed to find a solution for: " ++ desc ++ "\n" ++
    "Possible reasons for failure:\n" ++
    "    1. The description is incorrect or ill-formed. The faces should be listed in the order '" ++ faceOrder ++ "' and use positional colors (i.e U instead of white)\n" ++
    "    2. The described state is impossible to reach on a Rubik's cube. Check that a color isn't used too often and that the orientation of fully specified pieces is actually possible.\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage
        (_:_:_) -> printUsage
        [x] -> if x == "repl" 
               then gracefulRepl
               else do
                mMoves <- generatorForPartialDesc x
                case mMoves of
                    Nothing -> putStrLn $ completeCubeFailedMsg x
                    Just moves -> do
                        putStrLn $ intercalate " " $ map show moves
                        putStrLn ""
                        putStrLn $ cubeDescColor $ applyAlgorithm moves solvedCube
