{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Light.Geometry.Transform
  -- ADT
  ( Transform

  -- Default Instances
  , identityTransform

  -- Arithmetic
  , inverse, compose, composeAll
  , translation, scaling, rotationQ, rotation, rotation3
  , translate, scale, rotate, rotate3, rotateQ

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

data Transform = Transform { _m :: Matrix, _mInv :: Matrix } deriving (Eq, Show, Read)

identityTransform :: Transform
identityTransform = Transform identityMatrix identityMatrix

inverse :: Transform -> Transform
inverse (Transform m m') = Transform m' m

compose :: Transform -> Transform -> Transform
compose (Transform m m') (Transform n n') = Transform (m |*| n) (n' |*| m')

composeAll :: [Transform] -> Transform
composeAll = foldl compose identityTransform

translation :: Vector -> Transform
translation v = Transform m m'
  where m  = translationMatrix v
        m' = translationMatrix (negateV v)

scaling :: Vector -> Transform
scaling v = Transform m m'
  where m  = scalingMatrix v
        m' = scalingMatrix (vector (1/v^.dx) (1/v^.dy) (1/v^.dz))

rotationQ :: Quaternion -> Transform
rotationQ q = Transform m m'
  where m  = toRotationMatrix q
        m' = toRotationMatrix $ conjugate q

rotation :: Float -> Vector -> Transform
rotation  angle axis = rotationQ $ rotationQuaternion angle axis

rotation3 :: Float -> Float -> Float -> Transform
rotation3 pitch yaw roll = rotationQ $ rotationQuaternion3 pitch yaw roll

translate :: (Transformable t) => t -> Vector -> t
translate t v = transform (translation v) t

scale :: (Transformable t) => t -> Vector -> t
scale t s = transform (scaling s) t

rotate :: (Transformable t) => t -> Float -> Vector -> t
rotate t a x = transform (rotation a x) t

rotate3 :: (Transformable t) => t -> Float -> Float -> Float -> t
rotate3 t p y r = transform (rotation3 p y r) t

rotateQ :: (Transformable t) => t -> Quaternion -> t
rotateQ t q = transform (rotationQ q) t

class Transformable a where
  transform :: Transform -> a -> a

instance Transformable Point where
  transform t p = _m t |*. p

instance Transformable Vector where
  transform t v = _m t |*^ v

instance Transformable Normal where
  transform t n = transpose (_mInv t) |*! n

instance Transformable Ray where
  transform t r = ray (transform t $ r^.rayOrigin) (transform t $ r^.rayDirection)

instance Transformable AABB where
  transform t b = fromPoints $ map (transform t) (corners b)
