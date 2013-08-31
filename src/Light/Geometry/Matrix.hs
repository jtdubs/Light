{-# LANGUAGE UnboxedTuples #-}

module Light.Geometry.Matrix
  -- ADT
  ( Matrix, matrix, elems

  -- Default Instances
  , zeroMatrix, identityMatrix

  -- Element Access
  , (!!)

  -- Arithmetic
  , transpose, (|+|), (|-|), (|*|), (|*), (|/), (|*^), (|*.), (|*!), (^*|), (.*|), (!*|)

  -- Transformation Matricies
  , scalingMatrix, translationMatrix, rotationMatrix, frustumMatrix, perspectiveMatrix, orthoMatrix
  )
where

import Prelude hiding ((!!))

import qualified Data.Vector.Unboxed as V

import Light.Geometry.Vector
import Light.Geometry.Normal
import Light.Geometry.Point

infixl 9 !!
infixl 6 |+|, |-|
infixl 7 |*|, |*, |/, |*^, ^*|, |*!, !*|, |*., .*|

data Matrix = Matrix !(V.Vector Double)

instance Show Matrix where
  show (Matrix m) = show m

instance Read Matrix where
  readsPrec n s = let [(fs, r)] = readsPrec n s in [(Matrix fs, r)]

matrix :: [Double] -> Matrix
matrix = Matrix . V.fromList

(!!) :: Matrix -> (Int, Int) -> Double
(Matrix m) !! (r, c) = m V.! (r*4 + c)

elems :: Matrix -> [Double]
elems (Matrix m) = V.toList m

type Quadruple = (# Double, Double, Double, Double #)

row :: Matrix -> Int -> () -> Quadruple
row m r = \_ -> (# m !! (r, 0), m !! (r, 1), m !! (r, 2), m !! (r, 3) #)

col :: Matrix -> Int -> () -> Quadruple
col m c = \_ -> (# m !! (0, c), m !! (1, c), m !! (2, c), m !! (3, c) #)

instance Eq Matrix where
  (Matrix m) == (Matrix n) = V.all ((< 0.000001) . abs) $ V.zipWith (-) m n

zeroMatrix :: Matrix
zeroMatrix = matrix [ 0, 0, 0, 0
                    , 0, 0, 0, 0
                    , 0, 0, 0, 0
                    , 0, 0, 0, 0 ]

identityMatrix :: Matrix
identityMatrix = matrix [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 1, 0
                        , 0, 0, 0, 1 ]

transpose :: Matrix -> Matrix
transpose m = matrix [ m !! (0, 0), m !! (1, 0), m !! (2, 0), m !! (3, 0)
                     , m !! (0, 1), m !! (1, 1), m !! (2, 1), m !! (3, 1)
                     , m !! (0, 2), m !! (1, 2), m !! (2, 2), m !! (3, 2)
                     , m !! (0, 3), m !! (1, 3), m !! (2, 3), m !! (3, 3) ]

dot :: (() -> Quadruple) -> (() -> Quadruple) -> Double
dot l r = let (# a, b, c, d #) = l ()
              (# x, y, z, w #) = r ()
          in a*x + b*y + c*z+ d*w

(|+|), (|-|) :: Matrix -> Matrix -> Matrix
(Matrix m) |+| (Matrix n) = Matrix $ V.zipWith (+) m n
(Matrix m) |-| (Matrix n) = Matrix $ V.zipWith (-) m n

(|*|) :: Matrix -> Matrix -> Matrix
m |*| n = matrix [ r 0 `dot` c 0, r 0 `dot` c 1, r 0 `dot` c 2, r 0 `dot` c 3
                 , r 1 `dot` c 0, r 1 `dot` c 1, r 1 `dot` c 2, r 1 `dot` c 3
                 , r 2 `dot` c 0, r 2 `dot` c 1, r 2 `dot` c 2, r 2 `dot` c 3
                 , r 3 `dot` c 0, r 3 `dot` c 1, r 3 `dot` c 2, r 3 `dot` c 3 ]
  where r = row m; c = col n

(|*), (|/) :: Matrix -> Double -> Matrix
(Matrix m) |* s = Matrix $ V.map (*s) m
(Matrix m) |/ s = Matrix $ V.map (/s) m

(|*^) :: Matrix -> Vector -> Vector
m |*^ (Vector x y z) = Vector (q `dot` row m 0) (q `dot` row m 1) (q `dot` row m 2)
  where q = \_ -> (# x, y, z, 0 #)

(^*|) :: Vector -> Matrix -> Vector
(Vector x y z) ^*| m = Vector (q `dot` col m 0) (q `dot` col m 1) (q `dot` col m 2)
  where q = \_ -> (# x, y, z, 0 #)

(|*!) :: Matrix -> Normal -> Normal
m |*! (Normal x y z) = Normal (q `dot` row m 0) (q `dot` row m 1) (q `dot` row m 2)
  where q = \_ -> (# x, y, z, 0 #)

(!*|) :: Normal -> Matrix -> Normal
(Normal x y z) !*| m = Normal (q `dot` col m 0) (q `dot` col m 1) (q `dot` col m 2)
  where q = \_ -> (# x, y, z, 0 #)

(|*.) :: Matrix -> Point -> Point
m |*. (Point x y z) = Point ((q `dot` row m 0)/w) ((q `dot` row m 1)/w) ((q `dot` row m 2)/w)
  where q = \_ -> (# x, y, z, 1 #)
        w = q `dot` row m 3

(.*|) :: Point -> Matrix -> Point
(Point x y z) .*| m = Point ((q `dot` col m 0)/w) ((q `dot` col m 1)/w) ((q `dot` col m 2)/w)
  where q = \_ -> (# x, y, z, 1 #)
        w = q `dot` col m 3

scalingMatrix :: Vector -> Matrix
scalingMatrix v = matrix [  dx v,     0,     0, 0
                         ,     0,  dy v,     0, 0
                         ,     0,     0,  dz v, 0
                         ,     0,     0,     0, 1 ]

translationMatrix :: Vector -> Matrix
translationMatrix v = matrix [ 1, 0, 0, dx v
                             , 0, 1, 0, dy v
                             , 0, 0, 1, dz v
                             , 0, 0, 0,    1 ]

rotationMatrix :: Double -> Vector -> Matrix
rotationMatrix angle axis = matrix [ u*u*(1-c)+c,   u*v*(1-c)-w*s, u*w*(1-c)+v*s, 0
                                   , v*u*(1-c)+w*s, v*v*(1-c)+c,   v*w*(1-c)-u*s, 0
                                   , w*u*(1-c)-v*s, w*v*(1-c)+u*s, w*w*(1-c)+c,   0
                                   , 0,             0,             0,             1 ]
  where c = cos angle; s = sin angle; u = dx axis; v = dy axis; w = dz axis

frustumMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
frustumMatrix l r b t n f = matrix [ (2*n)/(r-l),           0,  a , 0
                                   ,           0, (2*n)/(t-b),  b', 0
                                   ,           0,           0,  c , d
                                   ,           0,           0, -1 , 0 ]
  where a = (r+l)/(r-l); b' = (t+b)/(t-b); c = -(f+n)/(f-n); d = -(2*f*n)/(f-n)

perspectiveMatrix :: Double -> Double -> Double -> Double -> Matrix
perspectiveMatrix fovY aspect near far = frustumMatrix (-fW) fW (-fH) fH near far
  where fH = tan (fovY / 2) * near; fW = fH * aspect

orthoMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
orthoMatrix l r b t n f = matrix [ 2/(r-l),       0,        0, tx
                                 ,       0, 2/(t-b),        0, ty
                                 ,       0,       0, -2*(f-n), tz
                                 ,       0,       0,        0,  1 ]
  where tx = -(r+l)/(r-l); ty = -(t+b)/(t-b); tz = -(f+n)/(f-n)
