{-# LANGUAGE TemplateHaskell #-}

module Light.Data.Sphere
    -- ADT
    ( Sphere, sphere, center, radius

    -- Default Instances
    , unitSphere
    )
where

import Control.Lens.TH             (makeLenses)
import Light.Data.Point            (Point)
import Light.Data.CoordinateSystem (Orientable(..))

import qualified Light.Data.Point as P

data Sphere = Sphere { _center :: Point, _radius :: Float }

sphere = Sphere

makeLenses ''Sphere

instance Eq Sphere where
  (Sphere c r) == (Sphere d s) = c == d && r == s

instance Show Sphere where
  show (Sphere c r) = concat ["#S(", show c, ", ", show r, ")"]

instance Orientable Sphere where
  toGlobal c (Sphere x r) = Sphere (toGlobal c x) r
  toLocal  c (Sphere x r) = Sphere (toLocal  c x) r

unitSphere = sphere P.origin 1
