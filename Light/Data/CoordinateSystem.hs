{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.CoordinateSystem
	-- ADT
	( CoordinateSystem, coordinateSystem, origin, rotation

	-- Default Instances
    , defaultCoordinateSystem
	)
where

import Control.Lens          (Lens', traverse, (*~), (//~), (^.), lens, traversed)
import Control.Lens.TH       (makeLenses)
import Light.Data.Point      (Point)
import Light.Data.Quaternion (Quaternion, identity)

import qualified Light.Data.Point      as P
import qualified Light.Data.Quaternion as Q

data CoordinateSystem = CoordinateSystem { _origin :: Point, _rotation :: Quaternion } deriving (Eq)

makeLenses ''CoordinateSystem

coordinateSystem = CoordinateSystem

defaultCoordinateSystem = coordinateSystem P.origin identity

instance Show CoordinateSystem where
  show cs = concat ["#CS(", show (cs^.origin), ", ", show (cs^.rotation), ")"]
