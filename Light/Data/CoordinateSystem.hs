{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.CoordinateSystem
	-- ADT
	( CoordinateSystem, coordinateSystem, origin, rotation

	-- Default Instances
    , defaultCoordinateSystem

	-- Arithmetic
	, modelMatrix, viewMatrix
	, translateLocal, translateGlobal
	, rotateLocal, rotateGlobal

	-- Orientation
	, Orientable(..)
	)
where

import Control.Lens          (Lens', (%~), (^.))
import Control.Lens.TH       (makeLenses)
import Light.Data.Vector     (Vector)
import Light.Data.Point      (Point, (.-.), (.+^))
import Light.Data.Quaternion (Quaternion, identity, toMatrix, conjugate, (@*@), (@*^), (@*.))
import Light.Data.Matrix     ((|*|), translate)

import qualified Light.Data.Point      as P
import qualified Light.Data.Quaternion as Q

data CoordinateSystem = CoordinateSystem { _origin :: Point, _rotation :: Quaternion } deriving (Eq)

makeLenses ''CoordinateSystem

coordinateSystem = CoordinateSystem

defaultCoordinateSystem = coordinateSystem P.origin identity

modelMatrix c = (toMatrix $ conjugate (c^.rotation)) |*| translate ((c^.origin) .-. P.origin)

viewMatrix c = (toMatrix $ c^.rotation) |*| translate ((c^.origin) .-. P.origin)

translateLocal v c = (origin %~ (.+^ ((c^.rotation) @*^ v))) c
translateGlobal v c = (origin %~ (.+^ v)) c

rotateLocal q c = (rotation %~ (@*@ q)) c
rotateGlobal q c = (rotation %~ (q @*@)) . (origin %~ (q @*.)) $ c

class Orientable a where
  toGlobal :: CoordinateSystem -> a -> a
  toLocal  :: CoordinateSystem -> a -> a

instance Orientable Vector where
  toGlobal c v = (c^.rotation) @*^ v
  toLocal  c v = (conjugate $ c^.rotation) @*^ v

instance Orientable Point where
  toGlobal c p = (c^.origin) .+^ ((c^.rotation) @*^ (p .-. (c^.origin)))
  toLocal  c p = P.origin .+^ (conjugate (c^.rotation) @*^ (p .-. P.origin))

instance Orientable Quaternion where
  toGlobal c q = (c^.rotation) @*@ q
  toLocal  c q = q @*@ conjugate (c^.rotation)

instance Orientable CoordinateSystem where
  toGlobal c h = CoordinateSystem (toGlobal c (h^.origin)) (toGlobal c (h^.rotation))
  toLocal  c h = CoordinateSystem (toLocal  c (h^.origin)) (toLocal  c (h^.rotation))

instance Show CoordinateSystem where
  show cs = concat ["#CS(", show (cs^.origin), ", ", show (cs^.rotation), ")"]
