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

import Light.Geometry.AABB
import Light.Geometry.Ray
import Light.Geometry.Point
import Light.Geometry.Normal
import Light.Geometry.Vector
import Light.Geometry.Matrix
import Light.Geometry.Quaternion

data Transform = Transform { transformMatrix        :: !Matrix
                           , transformInverseMatrix :: !Matrix
                           }
               deriving (Eq, Show, Read)

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
        m' = scalingMatrix (Vector (1/dx v) (1/dy v) (1/dz v))

rotationQ :: Quaternion -> Transform
rotationQ q = Transform m m'
  where m  = toRotationMatrix q
        m' = toRotationMatrix $ conjugate q

rotation :: Double -> Vector -> Transform
rotation  angle axis = rotationQ $ rotationQuaternion angle axis

rotation3 :: Double -> Double -> Double -> Transform
rotation3 pitch yaw roll = rotationQ $ rotationQuaternion3 pitch yaw roll

translate :: (Transformable t) => t -> Vector -> t
translate t v = transform (translation v) t

scale :: (Transformable t) => t -> Vector -> t
scale t s = transform (scaling s) t

rotate :: (Transformable t) => t -> (Double, Vector) -> t
rotate t (a, x) = transform (rotation a x) t

rotate3 :: (Transformable t) => t -> (Double, Double, Double) -> t
rotate3 t (p, y, r) = transform (rotation3 p y r) t

rotateQ :: (Transformable t) => t -> Quaternion -> t
rotateQ t q = transform (rotationQ q) t

class Transformable a where
  transform :: Transform -> a -> a

instance Transformable Point where
  transform t p = transformMatrix t |*. p

instance Transformable Vector where
  transform t v = transformMatrix t |*^ v

instance Transformable Normal where
  transform t n = transpose (transformInverseMatrix t) |*! n

instance Transformable Ray where
  transform t r = Ray (transform t $ rayOrigin r) (transform t $ rayDirection r)

instance Transformable AABB where
  transform t b = fromPoints $ map (transform t) (corners b)
