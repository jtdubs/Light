{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.CoordinateSystem
	-- ADT
	( CoordinateSystem

	-- Construction
	, coordinateSystem, origin, rotation

	-- Default Instances
    , defaultCoordinateSystem
	)
where

import Data.Lens.Template    (makeLens)
import Data.Lens.Common      ((^.))
import Light.Data.Point      (Point)
import Light.Data.Quaternion (Quaternion)

import qualified Light.Data.Point      as P
import qualified Light.Data.Quaternion as Q

data CoordinateSystem = CoordinateSystem { _origin :: Point, _rotation :: Quaternion } deriving (Eq)

$(makeLens ''CoordinateSystem)

coordinateSystem :: Point -> Quaternion -> CoordinateSystem
coordinateSystem = CoordinateSystem

defaultCoordinateSystem :: CoordinateSystem
defaultCoordinateSystem = coordinateSystem P.origin Q.identity

instance Show CoordinateSystem where
  show cs = concat ["#CS(", show (cs^.origin), ", ", show (cs^.rotation), ")"]
