{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.Normal
    -- ADT
	( Normal, normal, nx, ny, nz, ns, fromVector

	-- Arithmetic
	, (!-!), (!+!), (!*), (!/), (*!), (!.!), negateNormal
    , normalizeN, magnitudeN, magnitudeSquaredN, faceForward
	)
where

import Control.Lens
import Control.Lens.TH

import Light.Geometry.Vector

data Normal = Normal { _nx :: Float, _ny :: Float, _nz :: Float }

makeLenses ''Normal

normal = Normal

ns :: Lens' Normal [Float]
ns = lens (\ (Normal x y z) -> [x, y, z, 0]) (\n [x, y, z, 0] -> Normal x y z)

instance Eq Normal where
  u == v = magnitudeSquaredN (u !-! v) < 0.00001

instance Show Normal where
  show (Normal x y z) = concat ["#N(", show x, ", ", show y, ", ", show z, ")"]

fromVector v = Normal (v^.dx) (v^.dy) (v^.dz)

(Normal x y z) !+! (Normal a b c) = Normal (x+a) (y+b) (z+c)
(Normal x y z) !-! (Normal a b c) = Normal (x-a) (y-b) (z-c)
(Normal x y z) !.! (Normal a b c) = (x*a) + (y*b) + (z*c)

(Normal x y z) !* s = Normal (x*s) (y*s) (z*s)
(Normal x y z) !/ s = Normal (x/s) (y/s) (z/s)
s *! (Normal x y z) = Normal (x*s) (y*s) (z*s)

negateNormal (Normal a b c) = Normal (-a) (-b) (-c)

magnitudeN = sqrt . magnitudeSquaredN
magnitudeSquaredN n = n !.! n

normalizeN n
 | magnitudeSquaredN n == 0 = n
 | otherwise                = n !/ magnitudeN n

faceForward n v = if (n !.! fromVector v) < 0 then negateNormal n else n
