module Light.Data.Tests (main) where

import qualified Test.QuickCheck as QC

import Light.Data.Vector
import Light.Data.Point
import Light.Data.Matrix

instance QC.Arbitrary Vector where
  arbitrary = QC.vector 3 >>= return . vector

instance QC.Arbitrary Point where
  arbitrary = QC.vector 3 >>= return . point

instance QC.Arbitrary Matrix where
  arbitrary = QC.vector 16 >>= return . matrix

prop_VectorAdditiveIdentity :: Vector -> Bool
prop_VectorAdditiveIdentity v = v == (v ^+^ zeroV)
                             && v == (v ^-^ zeroV)

prop_CrossProductIsOrthogonal :: Vector -> Vector -> Bool
prop_CrossProductIsOrthogonal u v = u == zeroV
                                 || v == zeroV
                                 || ( abs ((cross3 (normalized u) (normalized v)) <.> u) < 0.0001
                                   && abs ((cross3 (normalized u) (normalized v)) <.> v) < 0.0001 )

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
prop_VectorTimesZero v = v ^*| zeroMatrix == zeroV
                      && zeroMatrix |*^ v == zeroV

prop_PointTimesZero :: Point -> Bool
prop_PointTimesZero p = p .*| zeroMatrix == origin
                     && zeroMatrix |*. p == origin

prop_VectorTimesIdentity :: Vector -> Bool
prop_VectorTimesIdentity v = v ^*| identityMatrix == v
                          && identityMatrix |*^ v == v

prop_PointTimesIdentity :: Point -> Bool
prop_PointTimesIdentity p = p .*| identityMatrix == p
                         && identityMatrix |*. p == p

main = do
  QC.quickCheck prop_VectorAdditiveIdentity
  QC.quickCheck prop_CrossProductIsOrthogonal
  QC.quickCheck prop_MatrixMultiplicitiveIdentity 
  QC.quickCheck prop_MatrixMultiplicitiveZero 
  QC.quickCheck prop_MatrixAdditiveIdentity 
  QC.quickCheck prop_MatrixTranspose
  QC.quickCheck prop_VectorTimesZero
  QC.quickCheck prop_PointTimesZero
  QC.quickCheck prop_VectorTimesIdentity
  QC.quickCheck prop_PointTimesIdentity
  QC.quickCheck prop_MatrixTranspose
