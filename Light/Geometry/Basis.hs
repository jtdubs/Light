{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}

module Light.Math.Basis
	-- ADT
	( Basis, origin, rotation

	-- Default Instances
    , defaultBasis

	-- HasBasis
	, HasBasis(..)
	, modelMatrix, viewMatrix
	, translateL, translateG
	, rotateL, rotateG

	-- Orientable
	, Orientable(..)
	)
where

import Control.Lens          (Lens', (%~), (^.))
import Control.Lens.TH       (makeLenses)
import Light.Math.Vector     (Vector)
import Light.Math.Point      (Point, originPoint, (.-.), (.+^))
import Light.Math.Quaternion (Quaternion, identity, toMatrix, conjugate, (@*@), (@*^), (@*.))
import Light.Math.Matrix     ((|*|), translationMatrix)

data Basis = Basis { _origin :: Point, _rotation :: Quaternion } deriving (Eq)

makeLenses ''Basis

instance Show Basis where
  show cs = concat ["#CS(", show (cs^.origin), ", ", show (cs^.rotation), ")"]

defaultBasis = Basis originPoint identity


class HasBasis a where
  basis :: Lens' a Basis

modelMatrix c = (toMatrix $ conjugate (c^.basis.rotation)) |*| translationMatrix ((c^.basis.origin) .-. originPoint)
viewMatrix  c = (toMatrix $            c^.basis.rotation)  |*| translationMatrix ((c^.basis.origin) .-. originPoint)

translateL v c = (basis.origin %~ (.+^ ((c^.basis.rotation) @*^ v))) c
translateG v c = (basis.origin %~ (.+^ v)) c

rotateL q c = (basis.rotation %~ (@*@ q)) c
rotateG q c = (basis.rotation %~ (q @*@)) . (basis.origin %~ (q @*.)) $ c


class Orientable a where
  outOf :: Basis -> a -> a
  into  :: Basis -> a -> a

instance Orientable Vector where
  outOf c v = (c^.rotation) @*^ v
  into  c v = (conjugate $ c^.rotation) @*^ v

instance Orientable Point where
  outOf c p = (c^.origin) .+^ ((c^.rotation) @*^ (p .-. (c^.origin)))
  into  c p = originPoint .+^ (conjugate (c^.rotation) @*^ (p .-. originPoint))

instance Orientable Quaternion where
  outOf c q = (c^.rotation) @*@ q
  into  c q = q @*@ conjugate (c^.rotation)

instance Orientable Basis where
  outOf c h = Basis (outOf c (h^.origin)) (outOf c (h^.rotation))
  into  c h = Basis (into  c (h^.origin)) (into  c (h^.rotation))
