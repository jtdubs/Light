{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.Normal
  -- ADT
  ( Normal, normal, nx, ny, nz, fromVector, ns

  -- Arithmetic
  , (!-!), (!+!), (!*), (!/), (*!), (!.!), negateN
  , normalizeN, magnitudeN, magnitudeSquaredN, faceForward
  )
where

import Control.Lens

import Light.Geometry.Vector

data Normal = Normal { _nx :: Double, _ny :: Double, _nz :: Double } deriving (Show, Read)

makeLenses ''Normal

normal :: Double -> Double -> Double -> Normal
normal = Normal

ns :: Lens' Normal [Double]
ns = lens (\ (Normal x y z) -> [x, y, z, 0]) (\_ [x, y, z, 0] -> Normal x y z)

instance Eq Normal where
  u == v = magnitudeSquaredN (u !-! v) < 0.000001

fromVector :: Vector -> Normal
fromVector v = Normal (v^.dx) (v^.dy) (v^.dz)

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
