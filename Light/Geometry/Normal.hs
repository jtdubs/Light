{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.Normal
    -- ADT
	( Normal, normal, nx, ny, nz, ns, fromVector

	-- Arithmetic
	, (!-!), (!+!), (!*), (!/), (*!), (!.!), negateNormal
    , normalize, magnitude, magnitudeSquared, faceForward
	)
where

import Control.Lens          ((^.), Lens', lens, traverse, (*~), (//~))
import Control.Lens.TH       (makeLenses)
import Light.Geometry.Vector (dx, dy, dz)

data Normal = Normal { _nx :: Float, _ny :: Float, _nz :: Float }

normal = Normal

makeLenses ''Normal

ns :: Lens' Normal [Float]
ns = lens (\ (Normal x y z) -> [x, y, z, 0]) (\n [x, y, z, 0] -> Normal x y z)

instance Eq Normal where
  u == v = magnitudeSquared (u !-! v) < 0.00001

instance Show Normal where
  show (Normal x y z) = concat ["#N(", show x, ", ", show y, ", ", show z, ")"]

fromVector v = Normal (v^.dx) (v^.dy) (v^.dz)

nn op (Normal a b c) (Normal x y z) = Normal (op a x) (op b y) (op c z)

(!-!) = nn (-)
(!+!) = nn (+)

n !* s = ns.traverse *~  s $ n
n !/ s = ns.traverse //~ s $ n
(*!) = flip (!*)

(Normal x y z) !.! (Normal a b c) = (x*a) + (y*b) + (z*c)

negateNormal (Normal a b c) = Normal (-a) (-b) (-c)

magnitude = sqrt . magnitudeSquared
magnitudeSquared n = n !.! n

normalize n
 | magnitudeSquared n == 0 = n
 | otherwise               = n !/ magnitude n

faceForward n v = if (n !.! fromVector v) < 0 then negateNormal n else n
