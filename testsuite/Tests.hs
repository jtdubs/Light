module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)

import qualified Light.Geometry.VectorTest as VT
import qualified Light.Geometry.PointTest  as PT
import qualified Light.Geometry.MatrixTest as MT

tests :: [TF.Test]
tests = [ testGroup "Vector" VT.tests
        , testGroup "Point"  PT.tests
        , testGroup "Matrix" MT.tests
        ] 

main :: IO ()
main = defaultMain tests
