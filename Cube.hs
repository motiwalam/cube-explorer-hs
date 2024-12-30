module Cube where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Tuple as Tuple
import Data.Maybe (fromMaybe)

extractJust :: Maybe a -> a
extractJust Nothing = undefined
extractJust (Just x) = x

data CubePos = UBL | URB | ULF | UFR
             | DLB | DBR | DFL | DRF
             | UL | UB | UR | UF
             | LF | LB | RB | RF
             | DL | DB | DR | DF
    deriving (Eq, Show, Ord, Enum, Read)

corners = enumFromTo UBL DRF
edges = enumFromTo UL DF
allCubies = corners ++ edges

strToCubePos s = let ss = List.sort s in head $ filter (\p -> List.sort (show p) == ss) allCubies

isCorner :: CubePos -> Bool
isCorner p = UBL <= p && p <= DRF

isEdge :: CubePos -> Bool
isEdge p = UL <= p && p <= DF

type Face = Char
type Color = Char

type CubeOrientation = Map.Map Face Color

defaultOrientation :: CubeOrientation
-- front red, top white
defaultOrientation = Map.fromList
    [ ('U', 'W')
    , ('D', 'Y')
    , ('L', 'G')
    , ('R', 'B')
    , ('F', 'R')
    , ('B', 'O')
    ]

data Cubie = Cubie { home :: CubePos
                   , orientation :: Int
                   }
    deriving (Eq, Show)

data Cube = Cube { cubies :: Map.Map CubePos Cubie, cubeOrientation :: CubeOrientation }
    deriving (Eq)

cubieAt :: CubePos -> Cube -> Cubie
cubieAt p (Cube m _) = extractJust $ Map.lookup p m

solvedCube :: Cube
solvedCube = Cube (Map.fromList $ map (\p -> (p, Cubie { home=p, orientation=0 })) allCubies) defaultOrientation

isSolved :: Cube -> Bool
isSolved = (== solvedCube)

data Move = U | U' | U2
          | D | D' | D2
          | L | L' | L2
          | R | R' | R2
          | F | F' | F2
          | B | B' | B2
        -- M, E, S are the "slice" moves
        -- M is slice between L and R; clockwise is same as for L
        -- E is slice between U and D; clockwise is same as for D
        -- S is slice between F and B; clockwise is same as for F 
          | M | M' | M2
          | E | E' | E2
          | S | S' | S2
        -- X, Y, Z are clockwise rotations of entire cube about respective axes
        -- X axis is L to R
        -- Y axis is D to U
        -- Z axis is F to B
          | X | X' | X2
          | Y | Y' | Y2
          | Z | Z' | Z2
    deriving (Eq, Show, Ord, Enum)

type Algorithm = [Move]

inverseOfMove :: Move -> Move
inverseOfMove m = case m of
    U -> U'; U' -> U; U2 -> U2;
    D -> D'; D' -> D; D2 -> D2;
    L -> L'; L' -> L; L2 -> L2;
    R -> R'; R' -> R; R2 -> R2;
    F -> F'; F' -> F; F2 -> F2;
    B -> B'; B' -> B; B2 -> B2;
    M -> M'; M' -> M; M2 -> M2;
    E -> E'; E' -> E; E2 -> E2;
    S -> S'; S' -> S; S2 -> S2;
    X -> X'; X' -> X; X2 -> X2;
    Y -> Y'; Y' -> Y; Y2 -> Y2;
    Z -> Z'; Z' -> Z; Z2 -> Z2;

inverseOfAlgorithm :: [Move] -> [Move]
inverseOfAlgorithm moves = reverse $ map inverseOfMove moves

twistModulus :: CubePos -> Int
twistModulus p
    | isCorner p = 3
    | otherwise  = 2

moveTable :: Map.Map Move ([(CubePos, CubePos, Int)], [(Face, Face)])
moveTable = Map.fromList (basic ++ map invert basic) where
    invert (m, (s, o)) = (inverseOfMove m, (map (\(d, s, t) -> (s, d, (-t) `mod` twistModulus s)) s, map Tuple.swap o))

    basic = [ (U, ([ (UBL, ULF, 0)
                    , (URB, UBL, 0)
                    , (UFR, URB, 0)
                    , (ULF, UFR, 0)
                    , (UL, UF, 0)
                    , (UB, UL, 0)
                    , (UR, UB, 0)
                    , (UF, UR, 0)
                    ], []))
            , (D, ([ (DFL, DLB, 0)
                    , (DRF, DFL, 0)
                    , (DBR, DRF, 0)
                    , (DLB, DBR, 0)
                    , (DF, DL, 0)
                    , (DR, DF, 0)
                    , (DB, DR, 0)
                    , (DL, DB, 0)
                    ], []))
            , (L, ([ (ULF, UBL, 1)
                    , (DFL, ULF, 2)
                    , (DLB, DFL, 1)
                    , (UBL, DLB, 2)
                    , (LF, UL, 1)
                    , (DL, LF, 1)
                    , (LB, DL, 1)
                    , (UL, LB, 1)
                    ], []))
            , (R, ([ (URB, UFR, 1)
                    , (DBR, URB, 2)
                    , (DRF, DBR, 1)
                    , (UFR, DRF, 2)
                    , (UR, RF, 1)
                    , (RB, UR, 1)
                    , (DR, RB, 1)
                    , (RF, DR, 1)
                    ], []))
            , (F, ([ (ULF, DFL, 2)
                    , (UFR, ULF, 1)
                    , (DRF, UFR, 2)
                    , (DFL, DRF, 1)
                    , (LF, DF, 0)
                    , (UF, LF, 0)
                    , (RF, UF, 0)
                    , (DF, RF, 0)
                    ], []))
            , (B, ([ (DLB, UBL, 2)
                    , (DBR, DLB, 1)
                    , (URB, DBR, 2)
                    , (UBL, URB, 1)
                    , (DB, LB, 0)
                    , (LB, UB, 0)
                    , (UB, RB, 0)
                    , (RB, DB, 0)
                    ], []))
            , (M, ([ (UF, UB, 1)
                    , (DF, UF, 1)
                    , (DB, DF, 1)
                    , (UB, DB, 1)
                    ], [ ('U', 'B')
                        , ('F', 'U')
                        , ('D', 'F')
                        , ('B', 'D') ]))
            , (E, ([ (LF, LB, 1)
                    , (RF, LF, 1)
                    , (RB, RF, 1)
                    , (LB, RB, 1)
                    ], [ ('F', 'L')
                        , ('R', 'F')
                        , ('B', 'R')
                        , ('L', 'B') ]))
            , (S, ([ (UL, DL, 1)
                    , (UR, UL, 1)
                    , (DR, UR, 1)
                    , (DL, DR, 1)
                    ], [ ('U', 'L')
                        , ('R', 'U')
                        , ('D', 'R')
                        , ('L', 'D') ]))
            ]

redundantMoves :: Map.Map Move [Move]
redundantMoves = Map.fromList
    [ (U2, [U, U]), (D2, [D, D]), (L2, [L, L])
    , (R2, [R, R]), (F2, [F, F]), (B2, [B, B])
    , (M2, [M, M]), (E2, [E, E]), (S2, [S, S])
    , (X2, [X, X]), (Y2, [Y, Y]), (Z2, [Z, Z])
    , (X, [R, L', M']), (X', [M, L, R])
    , (Y, [U, D', E']), (Y', [E, D, U'])
    , (Z, [F, B', S]), (Z', [S', B, F'])
    ]

applyMove :: Move -> Cube -> Cube
applyMove mv cube@(Cube m o) = case Map.lookup mv redundantMoves of
    Just alg -> applyAlgorithm alg cube
    Nothing  -> Cube m' o' where
        (t, t') = extractJust $ Map.lookup mv moveTable
        m' = Map.map (updateCubie (map Tuple.swap t')) $ foldl helper m t
        helper m (dst, src, twist) =
            let (Cubie h o) = cubieAt src cube
                modulus = twistModulus h
                newCubie = Cubie h ((o + twist) `mod` modulus)
            in
                Map.insert dst newCubie m
        updateCubie t' (Cubie h o) = 
            let newHome = strToCubePos $ map (\d -> fromMaybe d $ lookup d t') $ show h
                oldRefFacelet = show h !! o
                newRefFacelet = fromMaybe oldRefFacelet $ lookup oldRefFacelet t'
                newOrientation = extractJust $ List.elemIndex newRefFacelet (show newHome)
            in
                Cubie newHome newOrientation
        o' = foldl (\m (d, s) -> Map.insert d (extractJust $ Map.lookup s o) m) o t'

applyAlgorithm :: Algorithm -> Cube -> Cube
applyAlgorithm ms c = foldl (flip applyMove) c ms

colorOf :: CubePos -> Int -> Color
colorOf p i = show p !! i

faces :: Map.Map Face [CubePos]
faces = Map.fromList
    [ ('U', [ UBL, UB, URB,
              UL,       UR,
              ULF, UF, UFR ])
    , ('D', [ DFL, DF, DRF,
              DL,       DR,
              DLB, DB, DBR ])
    , ('L', [ UBL, UL, ULF,
              LB,       LF,
              DLB, DL, DFL ])
    , ('R', [ UFR, UR, URB,
              RF,       RB,
              DRF, DR, DBR ])
    , ('F', [ ULF, UF, UFR,
              LF,       RF,
              DFL, DF, DRF ])
    , ('B', [ URB, UB, UBL,
              RB,       LB,
              DBR, DB, DLB ])
    ]

indexForFace :: Map.Map (Face, CubePos) Int
indexForFace = Map.fromList $
    [((f,p), extractJust $ List.elemIndex f (show p)) | (f, ps) <- Map.toList faces, p <- ps]

cubieColorAt :: Cubie -> CubePos -> Face -> Color
cubieColorAt (Cubie h o) pos face =
    let i = extractJust $ Map.lookup (face, pos) indexForFace
        m = twistModulus pos
    in
        colorOf h ((o + i) `mod` m)

faceDesc :: Cube -> Face -> [String]
faceDesc cube face =
    let positions = extractJust $ Map.lookup face faces
        cubieColors = map (\p -> cubieColorAt (cubieAt p cube) p face) positions
        faceString = take 4 cubieColors ++ [face] ++ drop 4 cubieColors
    in
        [take 3 faceString, take 3 (drop 3 faceString), drop 6 faceString]

cubeDesc :: Cube -> String
cubeDesc cube =
    let [u, d, l, r, f, b] = map (faceDesc cube) "UDLRFB"
        top = map ("   " ++) u
        mid = foldl (zipWith (++)) (repeat "") [l, f, r, b]
        bot = map ("   " ++) d
    in
        unlines (top ++ mid ++ bot)

ansiColors :: Map.Map Color String
ansiColors = Map.fromList
    [ ('W', "\x1b[48;2;255;255;255m")
    , ('Y', "\x1b[48;2;255;255;45m")
    , ('R', "\x1b[48;2;255;0;0m")
    , ('O', "\x1b[48;2;237;132;38m")
    , ('G', "\x1b[48;2;0;255;0m")
    , ('B', "\x1b[48;2;0;174;255m")
    ]


cubeDescColor :: Cube -> String
cubeDescColor cube@(Cube _ faceColors) =
    let desc = cubeDesc cube
        colorize ' ' = "  "
        colorize '\n' = "\n"
        colorize face = 
            let faceColor = extractJust (Map.lookup face faceColors) 
                ansiColor = extractJust (Map.lookup faceColor ansiColors)
            in 
                ansiColor ++ "  \x1b[49m"
    in
        concatMap colorize desc
