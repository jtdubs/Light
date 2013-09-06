module Light.Math
  ( quadratic, radicalInverse, vanDerCorputSequence
  )
where

import Data.List

quadratic :: Double -> Double -> Double -> [Double]
quadratic a b c = if d < 0
                  then []
                  else sort [ q/a, c/q ]
  where d = b*b - 4*a*c
        q = if b < 0
            then -(b - sqrt d)/2
            else -(b + sqrt d)/2

radicalInverse :: Int -> Int -> Double
radicalInverse n b = helper 0 n (1 / fromIntegral b) (1 / fromIntegral b)
  where helper :: Double -> Int -> Double -> Double -> Double
        helper r 0 _     _       = r
        helper r i invBi invBase = let di = i `mod` b
                                   in helper (r + (fromIntegral di) * invBi) (truncate $ (fromIntegral i) * invBase) (invBi * invBase) invBase

vanDerCorputSequence :: Int -> [Double]
vanDerCorputSequence b = map (flip radicalInverse b) [1..]
