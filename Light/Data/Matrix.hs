{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module Light.Data.Matrix
	-- ADT
	( Matrix

	-- Construction
	, matrix, toList, fromRows, fromCols, toRows, toCols

	-- Default Instances
    , zero, identity

	-- Element Access
    , (!!), ix, row, col

	-- Arithmetic
    , transpose, (|+|), (|-|), (|*|), (|*), (|/), (|*^), (|*.), (^*|), (.*|)

	-- Transformation Matricies
	, scale, translate, rotate, frustum, perspective, ortho
	)
where

import Prelude hiding     ((!!))

import Data.Array.IArray  (array, elems, (!))
import Data.Array.Unboxed (UArray(..))
import Data.List          (intersperse)
import Data.Lens.Template (makeLens)
import Data.Lens.Common   (Lens, lens, (^.))
import Light.Data.Vector  (Vector, x, y, z)
import Light.Data.Point   (Point)

import qualified Data.Array.IArray as A
import qualified Data.List         as L
import qualified Light.Data.Vector as V
import qualified Light.Data.Point  as P

data Matrix a = Matrix !(UArray Int a)

matrix = Matrix . array (0, 15) . zip [0..]

toList :: Matrix -> [Float]
toList = concat . toRows

fromRows, fromCols :: [[Float]] -> Matrix
fromRows = matrix . concat
fromCols = matrix . concat . L.transpose

toRows, toCols :: Matrix -> [[Float]]
toRows m = [m^.(row i) | i <- [0..3]]
toCols m = [m^.(col i) | i <- [0..3]]

zero :: Matrix
zero = matrix [ 0, 0, 0, 0
              , 0, 0, 0, 0
              , 0, 0, 0, 0
              , 0, 0, 0, 0 ]

identity :: Matrix
identity = matrix [ 1, 0, 0, 0
                  , 0, 1, 0, 0
                  , 0, 0, 1, 0
                  , 0, 0, 0, 1 ]

(!!) :: Matrix -> (Int, Int) -> Float
(Matrix m) !! (r, c) = m ! (r*4 + c)

(//) :: Matrix -> [((Int, Int), Float)] -> Matrix
(Matrix m) // u = Matrix $ m A.// (map (\((r, c), f) -> (r*4+c, f)) u)

ix :: (Int, Int) -> Lens Matrix Float
ix p = lens (!! p)
            (\v m -> m // [(p, v)])

row :: Int -> Lens Matrix [Float]
row r = lens (\m -> [m !! (r, c) | c <- [0..3]])
             (\vs m -> m // [((r, c), v) | c <- [0..3] | v <- vs])

col :: Int -> Lens Matrix [Float]
col c = lens (\m -> [m !! (r, c) | r <- [0..3]])
             (\vs m -> m // [((r, c), v) | r <- [0..3] | v <- vs])

transpose :: Matrix -> Matrix
transpose = fromRows . toCols

dot :: [Float] -> [Float] -> Float
dot x y = sum $ zipWith (*) x y

(|+|), (|-|) :: Matrix -> Matrix -> Matrix
m |+| n = matrix $ zipWith (+) (toList m) (toList n)
m |-| n = matrix $ zipWith (-) (toList m) (toList n)

(|*), (|/) :: Matrix -> Float -> Matrix
m |* s = matrix $ map (* s) (toList m)
m |/ s = matrix $ map (/ s) (toList m)

(|*|) :: Matrix -> Matrix -> Matrix
m |*| n = matrix [ row `dot` col | row <- toRows m, col <- toCols n ]

(|*^) :: Matrix -> Vector -> Vector
m |*^ v = V.fromList $ map (`dot` V.toList v) (toRows m)

(^*|) :: Vector -> Matrix -> Vector
v ^*| m = V.fromList $ map (V.toList v `dot`) (toCols m)

(|*.) :: Matrix -> Point -> Point
m |*. p = P.fromList $ map (`dot` P.toList p) (toRows m)

(.*|) :: Point -> Matrix -> Point
p .*| m = P.fromList $ map (P.toList p `dot`) (toCols m)

scale :: Vector -> Matrix
scale v = matrix [ v^.x,    0,    0, 0
                 ,    0, v^.y,    0, 0
                 ,    0,    0, v^.z, 0
                 ,    0,    0,    0, 1 ]


translate :: Vector -> Matrix
translate v = matrix [ 0, 0, 0, v^.x
                     , 0, 0, 0, v^.y
                     , 0, 0, 0, v^.z
                     , 0, 0, 0,    0 ]

rotate :: Float -> Vector -> Matrix
rotate angle axis = matrix [ u*u*(1-c)+c,   u*v*(1-c)-w*s, u*w*(1-c)+v*s, 0
                           , v*u*(1-c)+w*s, v*v*(1-c)+c,   v*w*(1-c)-u*s, 0
                           , w*u*(1-c)-v*s, w*v*(1-c)+u*s, w*w*(1-c)+c,   0
                           , 0,             0,             0,             1 ]
  where c = cos angle; s = sin angle; u = axis^.x; v = axis^.y; w = axis^.z

frustum :: Float -> Float -> Float -> Float -> Float -> Float -> Matrix
frustum l r b t n f = matrix [ (2*n)/(r-l),           0,  a, 0
                             ,           0, (2*n)/(t-b),  b, 0
                             ,           0,           0,  c, d
                             ,           0,           0, -1, 0 ]
  where a = (r+l)/(r-l); b = (t+b)/(t-b); c = -(f+n)/(f-n); d = -(2*f*n)/(f-n)

perspective :: Float -> Float -> Float -> Float -> Matrix
perspective fovY aspect near far = frustum (-fW) fW (-fH) fH near far
  where fH = tan (fovY / 2) * near; fW = fH * aspect

ortho :: Float -> Float -> Float -> Float -> Float -> Float -> Matrix
ortho l r b t n f = matrix [ 2/(r-l),       0,       0, tx
                           ,       0, 2/(t-b),       0, ty
                           ,       0,       0, -2(f-n), tz
                           ,       0,       0,       0,  1 ]
  where tx = -(r+l)/(r-l); ty = -(t+b)/(t-b); tz = -(f+n)/(f-n)
 
instance Eq Matrix where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Matrix where
  show m = let showRow   = concat . intersperse ", "    . map show .  (\n -> m^.(row n))
               showRows  = concat $ intersperse "\n   " $ map showRow [0..3]
             in "#M(" ++ showRows ++ ")"
