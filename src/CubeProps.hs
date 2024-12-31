module CubeProps (
          cornerPermSign
        , edgePermSign
        , cornerOrientationsSum
        , edgeOrientationsSum
        , isSolvable
    ) where

import Cube

numInversions :: Ord a => [a] -> Int
numInversions [] = 0
numInversions (x:xs) = numInversions xs + (length $ filter (x >) xs)

permSign :: Ord a => [a] -> Int
permSign = ((-1)^) . numInversions

piecesPerm :: [CubePos] -> Cube -> [CubePos]
piecesPerm ps c = map (home . flip cubieAt c) ps

cornerPerm, edgePerm :: Cube -> [CubePos]
cornerPerm = piecesPerm corners
edgePerm = piecesPerm edges

cornerPermSign, edgePermSign :: Cube -> Int
cornerPermSign = permSign . cornerPerm
edgePermSign = permSign . edgePerm

pieceOrientationsSum :: [CubePos] -> Cube -> Int
pieceOrientationsSum ps c = sum $ map (orientation . flip cubieAt c) ps

cornerOrientationsSum, edgeOrientationsSum :: Cube -> Int
cornerOrientationsSum = (`mod` 3) . pieceOrientationsSum corners
edgeOrientationsSum = (`mod` 2) . pieceOrientationsSum edges

isSolvable :: Cube -> Bool
isSolvable cube = 
  edgePermSign cube == cornerPermSign cube
  && edgeOrientationsSum cube == 0
  && cornerOrientationsSum cube == 0
