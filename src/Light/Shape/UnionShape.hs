{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.UnionShape
  -- ADT
  ( UnionShape, unionShape, unionShapes
  )
where

import Control.Lens hiding (transform)
import Data.List
import Data.Ord

import Light.Geometry.AABB
import Light.Geometry.Transform
import Light.Shape.Shape

data UnionShape = UnionShape { _unionTransform :: Transform, _unionShapes :: [ShapeBox] }
                deriving (Show)

unionShape :: [ShapeBox] -> UnionShape
unionShape = UnionShape identityTransform

makeLenses ''UnionShape

instance Transformable UnionShape where
  transform t' (UnionShape t s) = UnionShape (compose t' t) s

instance Shape UnionShape where
  shapeTransform = unionTransform

  bound (UnionShape _ s) = foldl aabbUnion EmptyAABB (map bound s)

  surfaceArea (UnionShape _ s) = sum $ map surfaceArea s

  intersections theRay (UnionShape tr s) =
    helper [] $ (order . map (intersections (transform (inverse tr) theRay))) s
    where helper []         []         = []
          helper []         ((t:ts):r) = t : helper [ts] r
          helper ((_:ts):r) []         = helper r [ts]
          helper ((t:ts):r) ((q:qs):f)
            | t <= q    = helper r                     (order $ ts:(q:qs):f)
            | otherwise = helper (order $ qs:(t:ts):r) f
          order = sortBy (comparing head) . filter (not . null)
