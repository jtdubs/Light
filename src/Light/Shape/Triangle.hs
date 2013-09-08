{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Triangle
  -- ADT
  ( Triangle, triangle, triangleVertices

  -- Default Instances
  , unitTriangle
  )
where

import Control.Monad
import Control.Lens hiding (transform)

import Light.Geometry
import Light.Shape

data Triangle = Triangle { _triangleTransform :: Transform, _triangleVertices :: [Point] } deriving (Show, Read)

triangle :: [Point] -> Triangle
triangle [a, b, c] = Triangle identityTransform [a, b, c]
triangle _         = error "triangles must have three vertices"

makeLenses ''Triangle

unitTriangle :: Triangle
unitTriangle = triangle [ originPoint .-^ unitXVector .-^ unitYVector
                        , originPoint .+^ unitXVector .-^ unitYVector
                        , originPoint .+^ unitYVector
                        ]

instance Transformable Triangle where
  transform t' (Triangle t points) = Triangle (compose t' t) points

instance Shape Triangle where
  shapeTransform = triangleTransform

  bound (Triangle _ points) = fromPoints points

  surfaceArea (Triangle _ [a, b, c]) = 0.5 * magnitudeV (cross (b .-. a) (c .-. a))
  surfaceArea (Triangle _ _) = error "triangles must have three vertices"

  intersect theRay (Triangle t [v0, v1, v2]) = do
    guard $ a /= 0
    guard $ u >= 0 && u <= 1
    guard $ v >= 0 && (u + v) <= 1
    guard $ t' >= 0
    return t'
    where r' = transform (inverse t) theRay
          p  = rayOrigin r'
          d  = rayDirection r'
          e1 = v1 .-. v0
          e2 = v2 .-. v0
          h  = cross d e2
          a  = e1 ^.^ h
          f  = 1/a
          s  = p .-. v0
          u  = f * (s ^.^ h)
          q  = cross s e1
          v  = f * (d ^.^ q)
          t' = f * (e2 ^.^ q)
  intersect _ (Triangle _ _) = error "triangles must have three vertices"
