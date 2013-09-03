{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.DisjunctionShape
  -- ADT
  ( DisjunctionShape, disjunctionShape, positiveShape, negativeShape
  )
where

import Control.Lens hiding (transform)
import Data.List
import Data.Ord

import Light.Geometry.AABB
import Light.Geometry.Transform
import Light.Shape.Shape
import Light.Shape.IntersectionShape

data DisjunctionShape = DisjunctionShape
                       { _disjunctionTransform :: Transform
                       , _positiveShape        :: ShapeBox
                       , _negativeShape        :: ShapeBox
                       }
                       deriving (Show)

disjunctionShape :: [ShapeBox] -> [ShapeBox] -> DisjunctionShape
disjunctionShape ps ns = DisjunctionShape identityTransform (shapeBox $ intersectionShape ps) (shapeBox $ intersectionShape ns)

makeLenses ''DisjunctionShape

data State = Outside | InsideP | InsideN | InsidePN

instance Transformable DisjunctionShape where
  transform t' (DisjunctionShape t p n) = DisjunctionShape (compose t' t) p n

instance Shape DisjunctionShape where
  shapeTransform = disjunctionTransform

  bound (DisjunctionShape _ p n) = bound p

  surfaceArea (DisjunctionShape _ p n) = surfaceArea p

  intersections theRay (DisjunctionShape tr p n) = if null pts
                                                   then []
                                                   else helper Outside pts nts
    where helper _        []     []     = []
          helper _        []     _      = []
          helper _        ps     []     = ps
          helper Outside  (p:ps) (n:ns)
            | p <= n    = p : helper InsideP  (  ps) (n:ns)
            | otherwise =     helper InsideN  (p:ps) (  ns)
          helper InsideN  (p:ps) (n:ns)
            | p <= n    =     helper InsidePN (  ps) (n:ns)
            | otherwise =     helper Outside  (p:ps) (  ns)
          helper InsideP  (p:ps) (n:ns)
            | p <= n    = p : helper Outside  (  ps) (n:ns)
            | otherwise = p : helper InsidePN (p:ps) (  ns)
          helper InsidePN (p:ps) (n:ns)
            | p <= n    =     helper InsideN  (  ps) (n:ns)
            | otherwise = n : helper InsideP  (p:ps) (  ns)

          pts  = intersections (transform (inverse tr) theRay) p
          nts  = intersections (transform (inverse tr) theRay) n
