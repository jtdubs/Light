module Light.Geometry.Matrix
  -- ADT
  ( Matrix(..)

  -- Default Instances
  , zeroMatrix, identityMatrix

  -- Arithmetic
  , transpose, (|+|), (|-|), (|*|), (|*), (|/), (|*^), (|*.), (|*!), (^*|), (.*|), (!*|)

  -- Transformation Matricies
  , scalingMatrix, translationMatrix, rotationMatrix, frustumMatrix, perspectiveMatrix, orthoMatrix
  )
where

import Prelude hiding ((!!))

import Light.Geometry.Vector
import Light.Geometry.Normal
import Light.Geometry.Point

infixl 6 |+|, |-|
infixl 7 |*|, |*, |/, |*^, ^*|, |*!, !*|, |*., .*|

data Matrix = Matrix !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double !Double
            deriving (Show, Read)

toList :: Matrix -> [Double]
toList (Matrix a b c d e f g h i j k l m n o p) = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]

instance Eq Matrix where
  m == n = all ((< 0.000001) . abs) $ zipWith (-) (toList m) (toList n)

zeroMatrix :: Matrix
zeroMatrix = Matrix 0 0 0 0
                    0 0 0 0
                    0 0 0 0
                    0 0 0 0

identityMatrix :: Matrix
identityMatrix = Matrix 1 0 0 0
                        0 1 0 0
                        0 0 1 0
                        0 0 0 1

transpose :: Matrix -> Matrix
transpose (Matrix a b c d e f g h i j k l m n o p) = Matrix a e i m b f j n c g k o d h l p

(|+|) :: Matrix -> Matrix -> Matrix
(Matrix aa ab ac ad ae af ag ah ai aj ak al am an ao ap) |+| (Matrix ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp) =
  Matrix (aa+ba) (ab+bb) (ac+bc) (ad+bd)
         (ae+be) (af+bf) (ag+bg) (ah+bh)
         (ai+bi) (aj+bj) (ak+bk) (al+bl)
         (am+bm) (an+bn) (ao+bo) (ap+bp)

(|-|) :: Matrix -> Matrix -> Matrix
(Matrix aa ab ac ad ae af ag ah ai aj ak al am an ao ap) |-| (Matrix ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp) =
  Matrix (aa-ba) (ab-bb) (ac-bc) (ad-bd)
         (ae-be) (af-bf) (ag-bg) (ah-bh)
         (ai-bi) (aj-bj) (ak-bk) (al-bl)
         (am-bm) (an-bn) (ao-bo) (ap-bp)

(|*|) :: Matrix -> Matrix -> Matrix
(Matrix aa ab ac ad ae af ag ah ai aj ak al am an ao ap) |*| (Matrix ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp) =
  Matrix (aa*ba+ab*be+ac*bi+ad*bm) (aa*bb+ab*bf+ac*bj+ad*bn) (aa*bc+ab*bg+ac*bk+ad*bo) (aa*bd+ab*bh+ac*bl+ad*bp)
         (ae*ba+af*be+ag*bi+ah*bm) (ae*bb+af*bf+ag*bj+ah*bn) (ae*bc+af*bg+ag*bk+ah*bo) (ae*bd+af*bh+ag*bl+ah*bp)
         (ai*ba+aj*be+ak*bi+al*bm) (ai*bb+aj*bf+ak*bj+al*bn) (ai*bc+aj*bg+ak*bk+al*bo) (ai*bd+aj*bh+ak*bl+al*bp)
         (am*ba+an*be+ao*bi+ap*bm) (am*bb+an*bf+ao*bj+ap*bn) (am*bc+an*bg+ao*bk+ap*bo) (am*bd+an*bh+ao*bl+ap*bp)

(|*) :: Matrix -> Double -> Matrix
(Matrix a b c d e f g h i j k l m n o p) |* s =
  Matrix (a*s) (b*s) (c*s) (d*s)
         (e*s) (f*s) (g*s) (h*s)
         (i*s) (j*s) (k*s) (l*s)
         (m*s) (n*s) (o*s) (p*s)

(|/) :: Matrix -> Double -> Matrix
(Matrix a b c d e f g h i j k l m n o p) |/ s =
  Matrix (a/s) (b/s) (c/s) (d/s)
         (e/s) (f/s) (g/s) (h/s)
         (i/s) (j/s) (k/s) (l/s)
         (m/s) (n/s) (o/s) (p/s)

(|*^) :: Matrix -> Vector -> Vector
(Matrix a b c _ e f g _ i j k _ _ _ _ _) |*^ (Vector x y z) =
  Vector (a*x+b*y+c*z) (e*x+f*y+g*z) (i*x+j*y+k*z)

(|*!) :: Matrix -> Normal -> Normal
(Matrix a b c _ e f g _ i j k _ _ _ _ _) |*! (Normal x y z) =
  Normal (a*x+b*y+c*z) (e*x+f*y+g*z) (i*x+j*y+k*z)

(|*.) :: Matrix -> Point -> Point
(Matrix a b c d e f g h i j k l m n o p) |*. (Point x y z) =
  case m*x+n*y+o*z+p of 
    0 -> originPoint
    w -> Point ((a*x+b*y+c*z+d)/w) ((e*x+f*y+g*z+h)/w) ((i*x+j*y+k*z+l)/w)

(^*|) :: Vector -> Matrix -> Vector
(Vector x y z) ^*| (Matrix a b c _ e f g _ i j k _ _ _ _ _) =
  Vector (a*x+e*y+i*z) (b*x+f*y+j*z) (c*x+g*y+k*z)

(!*|) :: Normal -> Matrix -> Normal
(Normal x y z) !*| (Matrix a b c _ e f g _ i j k _ _ _ _ _) =
  Normal (a*x+e*y+i*z) (b*x+f*y+j*z) (c*x+g*y+k*z)

(.*|) :: Point -> Matrix -> Point
(Point x y z) .*| (Matrix a b c d e f g h i j k l m n o p) =
  case d*x+h*y+l*z+p of
    0 -> originPoint
    w -> Point ((a*x+e*y+i*z+m)/w) ((b*x+f*y+j*z+n)/w) ((c*x+g*y+k*z+o)/w)

scalingMatrix :: Vector -> Matrix
scalingMatrix (Vector x y z) = Matrix x 0 0 0
                                      0 y 0 0
                                      0 0 z 0
                                      0 0 0 1

translationMatrix :: Vector -> Matrix
translationMatrix (Vector x y z) = Matrix 1 0 0 x
                                          0 1 0 y
                                          0 0 1 z
                                          0 0 0 1

rotationMatrix :: Double -> Vector -> Matrix
rotationMatrix angle axis = Matrix (u*u*(1-c)+c)    (u*v*(1-c)-w*s)  (u*w*(1-c)+v*s)  0
                                   (v*u*(1-c)+w*s)  (v*v*(1-c)+c)    (v*w*(1-c)-u*s)  0
                                   (w*u*(1-c)-v*s)  (w*v*(1-c)+u*s)  (w*w*(1-c)+c)    0
                                    0                0                0               1
  where c = cos angle; s = sin angle; u = dx axis; v = dy axis; w = dz axis

frustumMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
frustumMatrix l r b t n f = Matrix ((2*n)/(r-l))              0    a    0
                                               0  ((2*n)/(t-b))    b'   0
                                               0              0    c    d
                                               0              0  (-1)   0
  where a = (r+l)/(r-l); b' = (t+b)/(t-b); c = -(f+n)/(f-n); d = -(2*f*n)/(f-n)

perspectiveMatrix :: Double -> Double -> Double -> Double -> Matrix
perspectiveMatrix fovY aspect near far = frustumMatrix (-fW) fW (-fH) fH near far
  where fH = tan (fovY / 2) * near; fW = fH * aspect

orthoMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
orthoMatrix l r b t n f = Matrix (2/(r-l))        0          0   tx
                                        0  (2/(t-b))         0   ty
                                        0         0  (-2*(f-n))  tz
                                        0         0          0   1
  where tx = -(r+l)/(r-l); ty = -(t+b)/(t-b); tz = -(f+n)/(f-n)
