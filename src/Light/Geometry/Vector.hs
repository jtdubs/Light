module Light.Geometry.Vector
  -- ADT
  ( Vector(..)

  -- Default Instances
  , zeroVector, unitXVector, unitYVector, unitZVector

  -- Arithmetic
  , (^+^), (^-^), (^.^), (*^), (^*), (^/)
  , negateV, magnitudeV, magnitudeSquaredV, normalizeV, cross
  , angleBetween
  )
where

data Vector = Vector { dx :: !Double, dy :: !Double, dz :: !Double } deriving (Show, Read)

instance Eq Vector where
  u == v = magnitudeSquaredV (u ^-^ v) < 0.000001

zeroVector, unitXVector, unitYVector, unitZVector :: Vector
zeroVector  = Vector 0 0 0
unitXVector = Vector 1 0 0
unitYVector = Vector 0 1 0
unitZVector = Vector 0 0 1
 
(^+^), (^-^) :: Vector -> Vector -> Vector
(Vector x y z) ^+^ (Vector a b c) = Vector (x+a) (y+b) (z+c)
(Vector x y z) ^-^ (Vector a b c) = Vector (x-a) (y-b) (z-c)

(^*), (^/) :: Vector -> Double -> Vector
(Vector x y z) ^* s = Vector (x*s) (y*s) (z*s)
(Vector x y z) ^/ s = Vector (x/s) (y/s) (z/s)

(*^) :: Double -> Vector -> Vector
s *^ (Vector x y z) = Vector (x*s) (y*s) (z*s)

(^.^) :: Vector -> Vector -> Double
(Vector x y z) ^.^ (Vector a b c) = (x*a) + (y*b) + (z*c)

infixl 6 ^+^, ^-^
infixl 7 ^.^, ^*, ^/, *^

negateV :: Vector -> Vector
negateV (Vector x y z) = Vector (-x) (-y) (-z)

magnitudeV, magnitudeSquaredV :: Vector -> Double
magnitudeV = sqrt . magnitudeSquaredV
magnitudeSquaredV v = v ^.^ v

normalizeV :: Vector -> Vector
normalizeV v
 | magnitudeSquaredV v == 0 = v
 | otherwise                = v ^/ magnitudeV v

cross :: Vector -> Vector -> Vector
cross (Vector ux uy uz) (Vector vx vy vz) =
  Vector (uy*vz - uz*vy)
         (uz*vx - ux*vz)
         (ux*vy - uy*vx)

angleBetween :: Vector -> Vector -> Double
angleBetween v w = acos $ (v ^.^ w) / (magnitudeV v * magnitudeV w)
