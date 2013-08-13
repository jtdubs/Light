{-# LANGUAGE TypeFamilies #-}

module Light.Data.CoordinateSystem
	-- ADT
	( CoordinateSystem

	-- Construction
	, coordinateSystem, origin, rotation

	-- Default Instances
    , defaultCoordinateSystem
	)
where

import Light.Data.Point      (Point(..), origin)
import Light.Data.Quaternion (Quaternion(..), identity)

data CoordinateSystem = CoordinateSystem !Point !Quaternion } deriving (Eq)

coordinateSystem :: Point -> Quaternion -> CoordinateSystem
coordinateSystem = CoordinateSystem

origin :: CoordinateSystem -> Point
origin   (CoordinateSystem o r) = o

rotation :: CoordinateSystem -> Quaternion
rotation (CoordinateSystem o r) = r

defaultCoordinateSystem :: CoordinateSystem
defaultCoordinateSystem = coordinateSystem origin identity

instance Show CoordinateSystem where
  show cs = concat ["#CS(", show (origin cs), ", ", show (rotation cs), ")"]
