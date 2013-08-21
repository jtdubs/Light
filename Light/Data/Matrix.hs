{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module Light.Data.Matrix
	-- ADT
	( Matrix, matrix, elems, ix, row, col, rows, cols

	-- Default Instances
    , zeroMatrix, identityMatrix

	-- Element Access
    , (!!), (//)

	-- Arithmetic
    , transpose, (|+|), (|-|), (|*|), (|*), (|/), (|*^), (|*.), (^*|), (.*|)

	-- Transformation Matricies
	, scaleMatrix, translationMatrix, rotationMatrix, frustumMatrix, perspectiveMatrix, orthoMatrix
	)
where

import Prelude hiding     ((!!))

import Data.Array.IArray  (array, (!))
import Data.Array.Unboxed (UArray(..))
import Data.List          (intersperse)
import Control.Lens       ((^.), (.~), (*~), (//~), Lens', lens, traversed)
import Light.Data.Vector  (Vector, dx, dy, dz, ds)
import Light.Data.Point   (Point, x, y, z, ps)

import qualified Data.Array.IArray as A
import qualified Data.List         as L

data Matrix = Matrix (UArray Int Float)

matrix = Matrix . array (0, 15) . zip [0..]

(Matrix m) !! (r, c) = m ! (r*4 + c)
(Matrix m) // u      = Matrix $ m A.// (map (\((r, c), f) -> (r*4+c, f)) u)

elems :: Lens' Matrix [Float]
elems = lens (\m -> m^.rows.traversed)
             (\m e -> rows.traversed .~ e $ m)

ix :: (Int, Int) -> Lens' Matrix Float
ix p = lens (!! p)
            (\m v -> m // [(p, v)])

row :: Int -> Lens' Matrix [Float]
row r = lens (\m -> [m !! (r, c) | c <- [0..3]])
             (\m vs -> m // [((r, c), v) | c <- [0..3] | v <- vs])

col :: Int -> Lens' Matrix [Float]
col c = lens (\m -> [m !! (r, c) | r <- [0..3]])
             (\m vs -> m // [((r, c), v) | r <- [0..3] | v <- vs])

rows :: Lens' Matrix [[Float]]
rows = lens (\m -> [m^.row i | i <- [0..3]])
            (\m vs -> matrix $ concat vs)

cols :: Lens' Matrix [[Float]]
cols = lens (\m -> [m^.col i | i <- [0..3]])
            (\m vs -> matrix $ concat $ L.transpose vs)

instance Eq Matrix where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (u^.elems) (v^.elems)

instance Show Matrix where
  show m = let showRow   = concat . intersperse ", "    . map show .  (\n -> m^.(row n))
               showRows  = concat $ intersperse "\n   " $ map showRow [0..3]
             in "#M(" ++ showRows ++ ")"

zeroMatrix = matrix [ 0, 0, 0, 0
                    , 0, 0, 0, 0
                    , 0, 0, 0, 0
                    , 0, 0, 0, 0 ]

identityMatrix = matrix [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 1, 0
                        , 0, 0, 0, 1 ]

transpose m = (cols .~ (m^.rows)) m

dot x y = sum $ zipWith (*) x y

m |+| n = matrix $ zipWith (+) (m^.elems) (n^.elems)
m |-| n = matrix $ zipWith (-) (m^.elems) (n^.elems)
m |*| n = matrix [ row `dot` col | row <- m^.rows, col <- n^.cols ]

m |* s = (elems.traversed  *~ s) m
m |/ s = (elems.traversed //~ s) m

m |*^ v = (ds .~ map (`dot` (v^.ds)) (m^.rows)) v
v ^*| m = (ds .~ map (`dot` (v^.ds)) (m^.cols)) v

m |*. p =  (ps .~ map (`dot` (p^.ps)) (m^.rows)) p
p .*| m =  (ps .~ map (`dot` (p^.ps)) (m^.cols)) p

scaleMatrix :: Vector -> Matrix
scaleMatrix v = matrix [ v^.dx,     0,     0, 0
                       ,     0, v^.dy,     0, 0
                       ,     0,     0, v^.dz, 0
                       ,     0,     0,     0, 1 ]

translationMatrix :: Vector -> Matrix
translationMatrix v = matrix [ 0, 0, 0, v^.dx
                             , 0, 0, 0, v^.dy
                             , 0, 0, 0, v^.dz
                             , 0, 0, 0,     0 ]

rotationMatrix :: Float -> Vector -> Matrix
rotationMatrix angle axis = matrix [ u*u*(1-c)+c,   u*v*(1-c)-w*s, u*w*(1-c)+v*s, 0
                                   , v*u*(1-c)+w*s, v*v*(1-c)+c,   v*w*(1-c)-u*s, 0
                                   , w*u*(1-c)-v*s, w*v*(1-c)+u*s, w*w*(1-c)+c,   0
                                   , 0,             0,             0,             1 ]
  where c = cos angle; s = sin angle; u = axis^.dx; v = axis^.dy; w = axis^.dz

frustumMatrix :: Float -> Float -> Float -> Float -> Float -> Float -> Matrix
frustumMatrix l r b t n f = matrix [ (2*n)/(r-l),           0,  a, 0
                                   ,           0, (2*n)/(t-b),  b, 0
                                   ,           0,           0,  c, d
                                   ,           0,           0, -1, 0 ]
  where a = (r+l)/(r-l); b = (t+b)/(t-b); c = -(f+n)/(f-n); d = -(2*f*n)/(f-n)

perspectiveMatrix :: Float -> Float -> Float -> Float -> Matrix
perspectiveMatrix fovY aspect near far = frustumMatrix (-fW) fW (-fH) fH near far
  where fH = tan (fovY / 2) * near; fW = fH * aspect

orthoMatrix :: Float -> Float -> Float -> Float -> Float -> Float -> Matrix
orthoMatrix l r b t n f = matrix [ 2/(r-l),       0,        0, tx
                                 ,       0, 2/(t-b),        0, ty
                                 ,       0,       0, -2*(f-n), tz
                                 ,       0,       0,        0,  1 ]
  where tx = -(r+l)/(r-l); ty = -(t+b)/(t-b); tz = -(f+n)/(f-n)
