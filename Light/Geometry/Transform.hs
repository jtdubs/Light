{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}

module Light.Geometry.Transform
	-- ADT
	( Transform

	-- Default Instances
  , identityTransform

	-- Arithmetic
	, inverse, compose, composeAll
  , translation, scale, rotationQ, rotation, rotation3

  -- Transformable(..)
  , Transformable(..)
	)
where

import Control.Lens hiding (transform)

import Light.Geometry.AABB
import Light.Geometry.Ray
import Light.Geometry.Point
import Light.Geometry.Normal
import Light.Geometry.Vector
import Light.Geometry.Matrix
import Light.Geometry.Quaternion

data Transform = Transform { _m :: Matrix, _mInv :: Matrix } deriving (Eq)

instance Show Transform where
  show = show . _m

identityTransform = Transform identityMatrix identityMatrix

inverse (Transform m m') = Transform m' m

compose (Transform m m') (Transform n n') = Transform (m |*| n) (n' |*| m')

composeAll ts = foldl compose identityTransform ts

translation v = Transform m m'
  where m  = translationMatrix v
        m' = translationMatrix (negateVector v)

scale v = Transform m m'
  where m  = scaleMatrix v
        m' = scaleMatrix (vector (1/v^.dx) (1/v^.dy) (1/v^.dz))

rotationQ q = Transform m m'
  where m  = toRotationMatrix q
        m' = toRotationMatrix $ conjugate q

rotation  angle axis     = rotationQ $ rotationQuaternion angle axis
rotation3 pitch yaw roll = rotationQ $ rotationQuaternion3 pitch yaw roll

class Transformable a where
  transform :: Transform -> a -> a

instance Transformable Point where
  transform t p = (_m t) |*. p

instance Transformable Vector where
  transform t v = (_m t) |*^ v

instance Transformable Normal where
  transform t n = transpose (_mInv t) |*! n

instance Transformable Ray where
  transform t r = ray (transform t $ r^.origin) (transform t $ r^.direction)

instance Transformable AABB where
  transform t b = fromPoints $ map (transform t) (corners b)
