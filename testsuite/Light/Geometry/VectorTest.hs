module Light.Geometry.VectorTest (tests) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

import Light.Geometry.Vector

instance QC.Arbitrary Vector where
  arbitrary = QC.vector 3 >>= \ [a, b, c] -> return $ Vector a b c

prop_VectorAdditiveIdentity :: Vector -> Bool
prop_VectorAdditiveIdentity v = v == (v ^+^ zeroVector)
                             && v == (v ^-^ zeroVector)

prop_MultiplicativeIdentity :: Vector -> Bool
prop_MultiplicativeIdentity v = (v ^* 1) == v
                             && (1 *^ v) == v
                             && (v ^/ 1) == v

prop_MultiplicativeCommutivity :: Vector -> Double -> Bool
prop_MultiplicativeCommutivity v s = (v ^* s) == (s *^ v)

prop_MultiplicationIsRepeatedAddition :: Vector -> Bool
prop_MultiplicationIsRepeatedAddition v = (v ^* 2) == (v ^+^ v)
                                       && (v ^* 3) == (v ^+^ v ^+^ v)

prop_DotProductCommutivity :: Vector -> Vector -> Bool
prop_DotProductCommutivity v w = v ^.^ w == w ^.^ v

prop_DoubleNegation :: Vector -> Bool
prop_DoubleNegation v = negateV (negateV v) == v

prop_NormalizeIsUnitLength :: Vector -> Bool
prop_NormalizeIsUnitLength v = v == zeroVector || abs (magnitudeV (normalizeV v) - 1) < 0.000001

prop_ScalarMultiplicationIsMagnitude :: Double -> Bool
prop_ScalarMultiplicationIsMagnitude s = abs (magnitudeV (Vector 1 0 0 ^* s) - abs s) < 0.000001

prop_CrossProductIsOrthogonal :: Vector -> Vector -> Bool
prop_CrossProductIsOrthogonal u v = u == zeroVector
                                 || v == zeroVector
                                 || ( abs (cross (normalizeV u) (normalizeV v) ^.^ u) < 0.000001
                                   && abs (cross (normalizeV u) (normalizeV v) ^.^ v) < 0.000001 )

tests :: [Test]
tests = [ testProperty "VectorAdditiveIdentity" prop_VectorAdditiveIdentity
        , testProperty "MultiplicativeIdentity" prop_MultiplicativeIdentity
        , testProperty "MultiplicativeCommutivity" prop_MultiplicativeCommutivity
        , testProperty "MultiplicationIsRepeatedAddition" prop_MultiplicationIsRepeatedAddition
        , testProperty "DotProductCommutivity" prop_DotProductCommutivity
        , testProperty "DoubleNegation" prop_DoubleNegation
        , testProperty "NormalizeIsUnitLength" prop_NormalizeIsUnitLength
        , testProperty "ScalarMultiplicationIsMagnitude" prop_ScalarMultiplicationIsMagnitude
        , testProperty "CrossProductIsOrthogonal" prop_CrossProductIsOrthogonal
        ]
