{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module Light.Data.Matrix
	( Matrix, matrix
    , zeroMatrix, identityMatrix
    , element, (!!), row, col, rows, cols
    , transpose
    , (|+|), (|-|), (|*|), (|*), (|/), (|*^), (|*.), (^*|), (.*|)
	)
where

import Prelude hiding     ((!!))

import Data.Array.IArray  (array, elems, (!))
import Data.Array.Unboxed (UArray(..))
import Data.List          (intersperse)

import qualified Light.Data.Vector as V
import qualified Light.Data.Point  as P

data Matrix = Matrix !(UArray Int Float)

matrix fs = Matrix $ array (0, 15) $ zip [0..] fs

zeroMatrix :: Matrix
zeroMatrix     = matrix [0, 0, 0, 0,
                         0, 0, 0, 0,
                         0, 0, 0, 0,
                         0, 0, 0, 0]

identityMatrix :: Matrix
identityMatrix = matrix [1, 0, 0, 0,
                         0, 1, 0, 0,
                         0, 0, 1, 0,
                         0, 0, 0, 1]

element :: Matrix -> (Int, Int) -> Float
element (Matrix m) (row, col) = m ! (row*4 + col)

(!!) :: Matrix -> (Int, Int) -> Float
(!!) = element

row, col :: Matrix -> Int -> [Float]
row m r = [m !! (r, c) | c <- [0..3]]
col m c = [m !! (r, c) | r <- [0..3]]

rows, cols :: Matrix -> [[Float]]
rows m = map (row m) [0..3]
cols m = map (col m) [0..3]

toList :: Matrix -> [Float]
toList = concat . rows

transpose :: Matrix -> Matrix
transpose = matrix . concat . cols

dot :: [Float] -> [Float] -> Float
dot x y = sum $ zipWith (*) x y

(|+|), (|-|) :: Matrix -> Matrix -> Matrix
m |+| n = matrix $ zipWith (+) (toList m) (toList n)
m |-| n = matrix $ zipWith (-) (toList m) (toList n)

(|*), (|/) :: Matrix -> Float -> Matrix
m |* s = matrix $ map (* s) (toList m)
m |/ s = matrix $ map (/ s) (toList m)

(|*|) :: Matrix -> Matrix -> Matrix
m |*| n = matrix [ row `dot` col | row <- rows m | col <- cols n ]

(|*^) :: Matrix -> V.Vector -> V.Vector
m |*^ v = V.vector $ map (`dot` V.toList v) (rows m)

(^*|) :: V.Vector -> Matrix -> V.Vector
v ^*| m = V.vector $ map (V.toList v `dot`) (cols m)

(|*.) :: Matrix -> P.Point -> P.Point
m |*. p = P.point $ map (`dot` P.toList p) (rows m)

(.*|) :: P.Point -> Matrix -> P.Point
p .*| m = P.point $ map (P.toList p `dot`) (cols m)

instance Eq Matrix where
  u == v = all (< 0.00001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Matrix where
  show m = let showRow   = concat . intersperse ", "  . map show . (row  m)
               showRows  = concat $ intersperse "\n   " $ map showRow [0..3]
             in "#M(" ++ showRows ++ ")"
