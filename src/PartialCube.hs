module PartialCube where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List ((\\), delete, intersect, sortBy)
import Control.Monad (guard)

import Cube
import CubeProps


newtype PartialCubie = PartialCubie { 
                                 -- visible colors on each face
                                 -- has length 2 if position is edge, length 3 if position is corner
                                 pVisible :: [(Face, Color)] 
                                 }
    deriving (Eq, Show)

data PartialCube = PartialCube { pCubies :: Map.Map CubePos PartialCubie
                               , pOrientation :: CubeOrientation
                               }
    deriving (Eq, Show)

cubieMatchesVisibleAt :: Cubie -> [(Face, Color)] -> CubePos -> Bool
cubieMatchesVisibleAt cubie visible pos = all (\(f, c) -> cubieColorAt cubie pos f == c) visible

cubieCompatible :: Cube -> PartialCube -> CubePos -> Bool
cubieCompatible (Cube cm _) (PartialCube pm _) pos = case Map.lookup pos pm of
    Nothing -> True
    Just (PartialCubie visible) ->
        let cubie = fromJust $ Map.lookup pos cm
        in cubieMatchesVisibleAt cubie visible pos


isCompatibleWith :: Cube -> PartialCube -> Bool
isCompatibleWith cube partial = 
    cubeOrientation cube == pOrientation partial
    && all (cubieCompatible cube partial) allCubies

-- takes a partially specified cube
-- and returns a list of *solvable* cubes
-- that are "compatible" with the partial cube
completeCube :: PartialCube -> [Cube]
completeCube p =
    fillHomes p
    >>= fixPerms p
    >>= fixEdgeOrientations p
    >>= fixCornerOrientations p

-- takes a partially specified cube
-- and returns a list of filled in cubes
-- that are "compatible" with the partial cube
-- resulting cubes may or may not be solvable
fillHomes :: PartialCube -> [Cube]
fillHomes pc = map snd $ fillAllIn initiallyAvailable positionsToFill emptyCube where
    initiallyAvailable = allCubies
    
    partialPositions = map fst $ sortBy (\(_,(PartialCubie v1)) (_, PartialCubie v2) -> length v2 `compare` length v1)  $ Map.toList $ pCubies pc
    positionsToFill = partialPositions ++ (allCubies \\ partialPositions)

    emptyCube :: Cube
    emptyCube = Cube { cubies = Map.empty, cubeOrientation = pOrientation pc }

    fillAllIn :: [CubePos] -> [CubePos] -> Cube -> [([CubePos], Cube)]
    fillAllIn avail [] cube = return (avail, cube)
    fillAllIn avail (p:ps) cube = do
        (avail', cube') <- fillInAt avail p cube
        fillAllIn avail' ps cube'
            
    fillInAt :: [CubePos] -> CubePos -> Cube -> [([CubePos], Cube)]
    fillInAt [] _ _ = []
    fillInAt avail pos cube = do
        h <- filter viable avail
        o <- possibleOrientations h
        let avail' = delete h avail
        let cube' = cube { cubies = Map.insert pos (Cubie h o) (cubies cube) }
        return (avail', cube')

        where
            fcs = Map.lookup pos (pCubies pc)
            visibility = maybe [] pVisible fcs
            visibleColors = map snd visibility

            viable :: CubePos -> Bool
            viable h = 
                isCorner h == isCorner pos 
                && intersect visibleColors (show h) == visibleColors

            validOrientation :: CubePos -> Int -> Bool
            validOrientation h o = cubieMatchesVisibleAt (Cubie h o) visibility pos

            possibleOrientations :: CubePos -> [Int]
            possibleOrientations h = filter (validOrientation h) [0 .. twistModulus h - 1]

-- takes a partial cube, a cube compatible with it
-- and returns a list of cubes compatible with the partial
-- cube so that the sign of the edge permutation matches
-- the sign of the corner permutation
-- if this is already true, then the output is the singleton
-- list containing that cube
fixPerms :: PartialCube -> Cube -> [Cube]
fixPerms pc cube =
    if edgePermSign cube == cornerPermSign cube
    then [cube]
    -- interleave edge swap solutions with corner swap solutions
    -- for variety/efficiency
    else zip fixWithEdgeSwap fixWithCornerSwap >>= \(a,b) -> [a,b]
    where
        swapPieces :: CubePos -> Int -> CubePos -> Int -> Cube -> Cube
        swapPieces x ox y oy c = c { cubies = cubies' }
            where
                Cubie hx _ = cubieAt x c
                Cubie hy _ = cubieAt y c
                cubies' = Map.insert x (Cubie hy oy) $ Map.insert y (Cubie hx ox) $ cubies c
        
        fixWithSwapFrom :: [CubePos] -> [Int] -> [Cube]
        fixWithSwapFrom pieces orientations = do
            [x,y] <- sequence [pieces, pieces]
            guard $ x /= y
            [ox, oy] <- sequence $ replicate 2 orientations
            let cube' = swapPieces x ox y oy cube
            guard $ cube' `isCompatibleWith` pc
            return cube'

        fixWithEdgeSwap = fixWithSwapFrom edges [0 .. 1]
        fixWithCornerSwap = fixWithSwapFrom corners [0 .. 2]

twistPiece :: Cube -> CubePos -> Int -> Cube
twistPiece cube pos o = cube { cubies = Map.insert pos (Cubie h o) $ cubies cube }
    where
        Cubie h _ = cubieAt pos cube

-- takes a partial cube, a cube compatible with
-- the partial cube, and returns a list of cubes
-- compatible with the partial cube with the
-- orientations of edges fixed
-- if the edges are already well oriented, then
-- the output is the singleton list containing that
-- cube
fixEdgeOrientations :: PartialCube -> Cube -> [Cube]
fixEdgeOrientations pc cube =
    if edgeOrientationsSum cube == 0
    then [cube]
    else do
        edge <- edges
        let cube' = twistPiece cube edge (1 - (orientation $ cubieAt edge cube))
        guard $ cube' `isCompatibleWith` pc
        return cube'


-- takes a partial cube, a cube compatible with
-- the partial cube, and returns a list of cubes
-- compatible with the partial cube with the
-- orientations of corners fixed
-- if the corners are already well oriented, then
-- the output is the singleton list containing that
-- cube
fixCornerOrientations :: PartialCube -> Cube -> [Cube]
fixCornerOrientations pc cube =
    let s = cornerOrientationsSum cube 
        ns = (-s) `mod` 3
        newOs = filter ((ns ==) . (`mod` 3) . sum) $ sequence [[0 .. 2], [1 .. 2]]
    in
    if s == 0
    then [cube]
    -- we only need to twist either one or two corners
    else do
        [c1, c2] <- sequence [corners, corners]
        guard $ c1 /= c2
        let [o1, o2] = map (orientation . flip cubieAt cube) [c1, c2]
        [d1, d2] <- newOs
        let x1 = (d1 + o1) `mod` 3
        let x2 = (d2 + o2) `mod` 3
        let cube' = twistPiece (twistPiece cube c1 x1) c2 x2
        guard $ cube' `isCompatibleWith` pc
        return cube'
