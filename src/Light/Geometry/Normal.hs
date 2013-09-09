module Light.Geometry.Normal
  -- ADT
  ( Normal(..), fromVector

  -- Arithmetic
  , (!-!), (!+!), (!*), (!/), (*!), (!.!), negateN
  , normalizeN, magnitudeN, magnitudeSquaredN, faceForward
  )
where

import Light.Geometry.Vector

data Normal = Normal { nx :: !Double
                     , ny :: !Double
                     , nz :: !Double }
            deriving (Show, Read)

instance Eq Normal where
  u == v = magnitudeSquaredN (u !-! v) < 0.000001

fromVector :: Vector -> Normal
fromVector v = Normal (dx v) (dy v) (dz v)

(!+!), (!-!) :: Normal -> Normal -> Normal
(Normal x y z) !+! (Normal a b c) = Normal (x+a) (y+b) (z+c)
(Normal x y z) !-! (Normal a b c) = Normal (x-a) (y-b) (z-c)

(!.!) :: Normal -> Normal -> Double
(Normal x y z) !.! (Normal a b c) = (x*a) + (y*b) + (z*c)

(!*), (!/) :: Normal -> Double -> Normal
(Normal x y z) !* s = Normal (x*s) (y*s) (z*s)
(Normal x y z) !/ s = Normal (x/s) (y/s) (z/s)

(*!) :: Double -> Normal -> Normal
s *! (Normal x y z) = Normal (x*s) (y*s) (z*s)

infixl 6 !+!, !-!
infixl 7 !.!, !*, !/, *!

negateN :: Normal -> Normal
negateN (Normal a b c) = Normal (-a) (-b) (-c)

magnitudeN, magnitudeSquaredN :: Normal -> Double
magnitudeN = sqrt . magnitudeSquaredN
magnitudeSquaredN n = n !.! n

normalizeN :: Normal -> Normal
normalizeN n
 | magnitudeSquaredN n == 0 = n
 | otherwise                = n !/ magnitudeN n

faceForward :: Normal -> Vector -> Normal
faceForward n v = if (n !.! fromVector v) < 0 then negateN n else n
