module CubeInteract where

import Text.Read (readMaybe)
import System.IO ( hFlush, stdout )

import Cube
import Util (trim)

helpText :: String
helpText = "Use \"q\" or \"quit\" to exit.\n" ++
           "Use \"h\" or \"help\" to print this message.\n" ++
           "Enter a space separated sequence of moves to execute an algorithm on the cube.\n" ++
           "A move is any one of the following: " ++ show [U .. Z2] ++ "\n"

cubeInteract :: Cube -> IO ()
cubeInteract cube = do
    putStrLn $ cubeDescColor cube
    moves <- parseMoves
    case moves of
        Nothing -> return ()
        Just moves -> cubeInteract $ applyAlgorithm moves cube
    where
        prompt :: String -> IO String
        prompt t = do
            putStr t
            hFlush stdout
            getLine

        parseMoves :: IO (Maybe [Move])
        parseMoves = do
            text <- trim <$> prompt "> "
            if text `elem` ["q", "quit"]
            then return Nothing
            else if text `elem` ["h", "help"]
            then do
                putStrLn helpText
                parseMoves
            else case mapM readMaybe (words text) of
                Nothing -> do
                    putStrLn "Error reading algorithm; please try again"
                    parseMoves
                v@(Just _) -> return v

cubeRepl :: IO ()
cubeRepl = do
    putStrLn "Welcome to the Rubik's CLI!"
    putStrLn "Use \"q\" to quit and \"h\" for help.\n"
    cubeInteract solvedCube