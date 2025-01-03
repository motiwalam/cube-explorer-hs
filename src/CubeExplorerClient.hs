{-# LANGUAGE OverloadedStrings #-}

module CubeExplorerClient where

import Data.Char (toLower, toUpper)
import Network.HTTP.Req

import Data.ByteString.Char8 (unpack)
import Data.Text (Text, pack)
import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import Cube
import Util (trim)
import PartialCube (PartialCube (..), PartialCubie (..), completeCube, isCompatibleWith)


faceOrder :: String
faceOrder = "URFDLB"

cubeExplorerDesc :: Cube -> String
cubeExplorerDesc cube = map toLower $ concat $ concatMap (faceDesc cube) faceOrder

descToPartialCube :: String -> CubeOrientation -> PartialCube
descToPartialCube desc o = 
    PartialCube {
            pOrientation = o
          , pCubies = Map.fromList $ map (\(p, v) -> (p, PartialCubie $ toVisibility v)) pieceToFacelets
        }
    where
        getFacelet :: Face -> Int -> Color
        getFacelet face idx = desc !! i where
            i = 9 * (fromJust $ elemIndex face faceOrder) + (idx - 1)
        
        toVisibility :: [(Face, Int)] -> [(Face, Color)]
        toVisibility = concatMap (\(f, i) -> let c = getFacelet f i in if c `elem` faceOrder then [(f, c)] else [])

        pieceToFacelets :: [(CubePos, [(Face, Int)])]
        pieceToFacelets = [
                (UBL, [('U', 1), ('B', 3), ('L', 1)])
              , (URB, [('U', 3), ('R', 3), ('B', 1)]) 
              , (ULF, [('U', 7), ('L', 3), ('F', 1)]) 
              , (UFR, [('U', 9), ('F', 3), ('R', 1)]) 
              , (DLB, [('D', 7), ('L', 7), ('B', 9)]) 
              , (DBR, [('D', 9), ('B', 7), ('R', 9)]) 
              , (DFL, [('D', 1), ('F', 7), ('L', 9)]) 
              , (DRF, [('D', 3), ('R', 7), ('F', 9)])
              , (UL,  [('U', 4), ('L', 2)])
              , (UB,  [('U', 2), ('B', 2)])
              , (UR,  [('U', 6), ('R', 2)])
              , (UF,  [('U', 8), ('F', 2)])
              , (LF,  [('L', 6), ('F', 4)])
              , (LB,  [('L', 4), ('B', 6)])
              , (RB,  [('R', 6), ('B', 4)])
              , (RF,  [('R', 4), ('F', 6)])
              , (DL,  [('D', 4), ('L', 8)])
              , (DB,  [('D', 8), ('B', 8)])
              , (DR,  [('D', 6), ('R', 8)])
              , (DF,  [('D', 2), ('F', 8)])
            ]

queryCubeExplorer :: Text -> Int -> Text -> IO [Move]
queryCubeExplorer domain portNo desc = runReq defaultHttpConfig $ do
    resp <- req
                GET
                (http domain)
                NoReqBody
                bsResponse
                (port portNo `mappend` queryFlag desc)
    let moveString = trim $ (!! 1) $ lines $ unpack $ responseBody resp
    return $ map read $ words moveString

queryCubeExplorerUOFT :: Text -> IO [Move]
queryCubeExplorerUOFT desc = runReq defaultHttpConfig $ do
    resp <- req
                GET
                (https "www.cs.toronto.edu" /: "~motiwala" /: "kociemba.cgi")
                NoReqBody
                bsResponse
                (queryFlag desc)
    let moveString = trim $ unpack $ responseBody resp
    return $ map read $ words moveString

simplifyGenerator :: PartialCube -> Cube -> [Move] -> [Move]
simplifyGenerator pc c moves = go (solvedCube { cubeOrientation = cubeOrientation c }) moves [] where
  go c [] acc = reverse acc
  go c (x:xs) acc
    | c `isCompatibleWith` pc = reverse acc
    | otherwise = go (applyMove x c) xs (x:acc)

generatorForPartialCube :: PartialCube -> IO (Maybe [Move])
generatorForPartialCube pc = case completeCube pc of
  [] -> return Nothing
  (c:_) -> fmap (Just . simplifyGenerator pc c . inverseOfAlgorithm) $ queryCubeExplorerUOFT $ pack $ map toUpper $ cubeExplorerDesc c

generatorForPartialDesc :: String -> IO (Maybe [Move])
generatorForPartialDesc = generatorForPartialCube . flip descToPartialCube defaultOrientation