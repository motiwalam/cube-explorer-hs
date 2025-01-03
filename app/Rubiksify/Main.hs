module Rubiksify.Main (main) where
import System.Environment (getProgName, getArgs)
import Codec.Picture
import Rubiksify.Cubify (cubifyImage, Color (..))

import CubeExplorerClient (cubeExplorerDesc)
import Cube (faceDesc, Cube(..))

import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Char (toUpper)
import qualified Data.Map as Map

usage :: String -> String
usage arg0 = "USAGE:\n" ++
    arg0 ++ " <img-path> <R1> <G1> <B1> ... <R6> <G6> <B6> (where R,G,B in 0-255, order is URFDLB)"

printUsage :: IO ()
printUsage = getProgName >>= return . usage >>= putStrLn

parseArgs :: [String] -> Maybe (String, [PixelRGB8])
parseArgs args
    | length args /= (1 + 3*6) = Nothing
    | otherwise = let (s:ps) = args in do
        psInt <- mapM readMaybe ps
        let idxs = [(0 + 3*i, 1 + 3*i, 2 + 3*i) | i <- [0 .. 6-1]]
        let rgbs = [(psInt !! i, psInt !! j, psInt !! k) | (i,j,k) <- idxs]
        let pixels = [PixelRGB8 r g b | (r, g, b) <- rgbs]
        return (s, pixels)

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    case args of
        Nothing -> printUsage
        Just (s, pixels) -> do
            img' <- fmap convertRGB8 <$> readImage s
            case img' of
                Left err -> putStrLn $ "Error reading img at " ++ s ++ ": " ++ err
                Right img -> do
                    let color = D
                    let ctable = zip pixels [U,R,F,D,L,B]
                    let cubes' = cubifyImage color ctable img
                    case cubes' of 
                        Left err -> putStrLn $ "Error cubifying: " ++ err
                        Right cubes -> putStrLn $ intercalate "\n" [ show x ++ "," ++ show y ++ "," ++ (map toUpper $ cubeExplorerDesc c) ++ "," ++ os c | ((x,y),c) <- cubes]
    where
        os c = intercalate "," $ map (\(s,t) -> [s,t]) $ Map.toList $ cubeOrientation c