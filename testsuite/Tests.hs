module Main where

import Control.Monad
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Light.Geometry.Vector
import Light.Geometry.Point
import Light.Geometry.Matrix

instance QC.Arbitrary Vector where
  arbitrary = QC.vector 3 >>= \ [a, b, c] -> return $ vector a b c

instance QC.Arbitrary Point where
  arbitrary = QC.vector 3 >>= \ [a, b, c] -> return $ point a b c

instance QC.Arbitrary Matrix where
  arbitrary = liftM matrix $ QC.vector 16

prop_VectorAdditiveIdentity :: Vector -> Bool
prop_VectorAdditiveIdentity v = v == (v ^+^ zeroVector)
                             && v == (v ^-^ zeroVector)

prop_CrossProductIsOrthogonal :: Vector -> Vector -> Bool
prop_CrossProductIsOrthogonal u v = u == zeroVector
                                 || v == zeroVector
                                 || ( abs ((cross (normalizeV u) (normalizeV v)) ^.^ u) < 0.0001
                                   && abs ((cross (normalizeV u) (normalizeV v)) ^.^ v) < 0.0001 )

prop_MatrixMultiplicitiveIdentity :: Matrix -> Bool
prop_MatrixMultiplicitiveIdentity m = m |*| identityMatrix == m
                                   && identityMatrix |*| m == m

prop_MatrixMultiplicitiveZero :: Matrix -> Bool
prop_MatrixMultiplicitiveZero m = m |*| zeroMatrix == zeroMatrix
                               && zeroMatrix |*| m == zeroMatrix

prop_MatrixAdditiveIdentity :: Matrix -> Bool
prop_MatrixAdditiveIdentity m = m |+| zeroMatrix == m
                             && zeroMatrix |+| m == m

prop_MatrixTranspose :: Matrix -> Bool
prop_MatrixTranspose m = transpose (transpose m) == m

prop_VectorTimesZero :: Vector -> Bool
prop_VectorTimesZero v = v ^*| zeroMatrix == zeroVector
                      && zeroMatrix |*^ v == zeroVector

prop_PointTimesZero :: Point -> Bool
prop_PointTimesZero p = p .*| zeroMatrix == originPoint
                     && zeroMatrix |*. p == originPoint

prop_VectorTimesIdentity :: Vector -> Bool
prop_VectorTimesIdentity v = v ^*| identityMatrix == v
                          && identityMatrix |*^ v == v

prop_PointTimesIdentity :: Point -> Bool
prop_PointTimesIdentity p = p .*| identityMatrix == p
                         && identityMatrix |*. p == p

tests :: [TF.Test]
tests = [ testGroup "QuickCheck Geometry"
            [ testProperty "VectorAdditiveIdentity" prop_VectorAdditiveIdentity
            , testProperty "CrossProductIsOrthogonal" prop_CrossProductIsOrthogonal
            , testProperty "MatrixMultiplicitiveIdentity"  prop_MatrixMultiplicitiveIdentity 
            , testProperty "MatrixMultiplicitiveZero"  prop_MatrixMultiplicitiveZero 
            , testProperty "MatrixAdditiveIdentity"  prop_MatrixAdditiveIdentity 
            , testProperty "MatrixTranspose" prop_MatrixTranspose
            , testProperty "VectorTimesZero" prop_VectorTimesZero
            , testProperty "PointTimesZero" prop_PointTimesZero
            , testProperty "VectorTimesIdentity" prop_VectorTimesIdentity
            , testProperty "PointTimesIdentity" prop_PointTimesIdentity
            , testProperty "MatrixTranspose" prop_MatrixTranspose
            ] ] 

main :: IO ()
main = defaultMain tests
