{-# LANGUAGE TupleSections #-}
module Rubiksify.Cubify where

import Codec.Picture

import Cube (Cube, CubeOrientation, Face)
import qualified Cube as Cube
import PartialCube (PartialCube, completeCube)
import CubeExplorerClient (descToPartialCube)
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe)

whitePixel = PixelRGB8 255 255 255

pixelAtDef :: Pixel a => a -> Image a -> Int -> Int -> a
pixelAtDef _ img x y
    | 0 <= x && x < imageWidth img 
      && 0 <= y && y < imageHeight img = pixelAt img x y
pixelAtDef def _ _ _ = def

roundUpToMultipleOf :: Integral a => a -> a -> a
roundUpToMultipleOf m n = n + ((- n) `mod` m)

type WindowIndices = [(Int, Int)]

-- tile an n x m grid with k x l rectangles
-- windowIndices returns the indices of each rectangle
-- enumerated in order from left to right, top to bottom
-- precondition: k divides n, l divides m
windowIndices :: Int -> Int -> Int -> Int -> [((Int, Int), WindowIndices)]
windowIndices n m k l = do
    -- the order here is important!
    topLefty <- [0 .. (m `div` l) - 1]
    topLeftx <- [0 .. (n `div` k) - 1]
    return $ ((topLeftx,topLefty),) $ do
        oy <- [0 .. l-1]
        ox <- [0 .. k-1]
        return (topLeftx + ox, topLefty + oy)

data Color = U | D | L | R | F | B
    deriving (Eq, Show, Enum)

type ColorWindow = [Color]
type ColorTable a = [(a, Color)]

colorIfFront :: Color -> Color -> Color
colorIfFront f c = case f of
    U -> case c of
        U -> B; D -> F; L -> L; R -> R; F -> U; B -> D;
    D -> case c of
        U -> F; D -> B; L -> L; R -> R; F -> D; B -> U;
    L -> case c of
        U -> U; D -> D; L -> B; R -> F; F -> L; B -> R;
    R -> case c of
        U -> U; D -> D; L -> F; R -> B; F -> R; B -> L;
    F -> case c of
        U -> U; D -> D; L -> L; R -> R; F -> F; B -> B;
    B -> case c of
        U -> U; D -> D; L -> R; R -> L; F -> B; B -> F;


makeCubeOrientation :: Show a => [a] -> Map Face Cube.Color
makeCubeOrientation l = fromList $ map (\(s,t) -> (s, p t)) o where 
    p x = show x !! 0
    o = zip "UDLRFB" l
    
colorWindowToPartialCube :: ColorWindow -> PartialCube
colorWindowToPartialCube face =
    let center = face !! 4
        inverseColorIfFront y = head $ filter (\x -> colorIfFront center x == y) [U .. B] 
        face' = concatMap (show . inverseColorIfFront) face
        desc = replicate (9 * 2) 'X' ++ face' ++ replicate (9 * 3) 'X'
    in
        descToPartialCube desc $ makeCubeOrientation $ map (colorIfFront center) [U,D,L,R,F,B]

colorWindowAt :: (Pixel a, Show a) => a -> ColorTable a -> Image a -> WindowIndices -> Either String ColorWindow
colorWindowAt def ctable img idxs = mapM f idxs where
    f (x, y) = case lookup pixel ctable of
        Nothing -> Left $ "Error looking up pixel (" ++ show pixel ++ ") at index " ++ show (x,y) ++ " (def: " ++ show pixel ++ ")"
        Just c -> Right c
        where
            pixel = pixelAtDef def img x y

cubifyImage :: (Pixel a, Show a) => Color -> ColorTable a -> Image a -> Either String [((Int, Int), Cube)]
cubifyImage def ctable img = mapM f $ windowIndices n m 3 3 where
    [n, m] = map (roundUpToMultipleOf 3) $ [imageWidth img, imageHeight img]

    defaultPixel = fromMaybe (Left $ "No color for default: " ++ show def) $ (Right <$>) $ lookup def $ map (\(s,t) -> (t,s)) ctable

    f ((x,y), idxs) = do
        defPixel <- defaultPixel
        cWindow <- colorWindowAt defPixel ctable img idxs
        let pc = colorWindowToPartialCube cWindow
        case completeCube pc of
            [] -> Left $ "Could not complete cube for " ++ show cWindow
            (c:_) -> Right ((x,y), c)


    


-- cubifyWindow ::
