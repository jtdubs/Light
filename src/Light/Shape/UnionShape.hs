module Light.Shape.UnionShape
  -- ADT
  ( UnionShape, unionShape, unionShapes
  )
where

import Data.List
import Data.Ord

import Light.Geometry
import Light.Shape

data UnionShape = UnionShape { unionTransform :: Transform
                             , unionShapes    :: [ShapeBox]
                             }
                deriving (Show)

unionShape :: [ShapeBox] -> UnionShape
unionShape = UnionShape identityTransform

instance Transformable UnionShape where
  transform t' (UnionShape t s) = UnionShape (compose t' t) s

instance Shape UnionShape where
  shapeTransform = unionTransform

  bound (UnionShape _ s) = foldl aabbUnion EmptyAABB (map bound s)

  surfaceArea (UnionShape _ s) = sum $ map surfaceArea s

  -- TODO: missing time for exit from last element of union
  intersections theRay (UnionShape tr s) =
    helper [] $ (order . map (intersections (transform (inverse tr) theRay))) s
    where helper []         []         = []
          helper []         ((t:ts):r) = t : helper [ts] r
          helper ((_:ts):r) []         = helper r [ts]
          helper ((t:ts):r) ((q:qs):f)
            | q <= t    =     helper (order $ qs:(t:ts):r) f
            | null r    = t : helper r (order $ ts:(q:qs):f)
            | otherwise =     helper r (order $ ts:(q:qs):f)
          order = sortBy (comparing head) . filter (not . null)
