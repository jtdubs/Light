{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.IntersectionShape
  -- ADT
  ( IntersectionShape, intersectionShape, intersectionShapes
  )
where

import Control.Lens hiding (transform)
import Data.List
import Data.Ord

import Light.Geometry.AABB
import Light.Geometry.Transform
import Light.Shape.Shape

data IntersectionShape = IntersectionShape { _intersectionTransform :: Transform, _intersectionShapes :: [ShapeBox] }
                deriving (Show)

intersectionShape :: [ShapeBox] -> IntersectionShape
intersectionShape = IntersectionShape identityTransform

makeLenses ''IntersectionShape

instance Transformable IntersectionShape where
  transform t' (IntersectionShape t s) = IntersectionShape (compose t' t) s

instance Shape IntersectionShape where
  shapeTransform = intersectionTransform

  bound (IntersectionShape _ s) = foldl aabbUnion EmptyAABB (map bound s)

  surfaceArea (IntersectionShape _ s) = sum $ map surfaceArea s

  intersections theRay (IntersectionShape tr s) = if (any null hits)
                                                  then []
                                                  else helper [] (order hits)
    where helper []         []          = []
          helper []         ((t:ts):[]) = t : helper [ts] []   -- on entering the last object, we enter the union
          helper []         ((_:ts):r)  = helper [ts] r
          helper ((t:[]):_) []          = t : []               -- on completing the first object, we are done with the union
          helper ((t:ts):r) []          = t : helper r [ts]    -- on exiting the first object, we exit the union
          helper ((t:[]):_) ((q:_):_)                          -- on completing the first object, we are done with the union
            | t <= q    = t : []
          helper ((t:ts):r) ((q:qs):f)
            | t <= q    =     helper r (order $ ts:(q:qs):f)
            | null f    = q : helper (order $ qs:(t:ts):r) f   -- on entering the last object, we enter the union
            | otherwise =     helper (order $ qs:(t:ts):r) f
          helper _ _ = error "shape intersection failed"

          hits  = map (intersections (transform (inverse tr) theRay)) s
          order = sortBy (comparing head) . filter (not . null)

