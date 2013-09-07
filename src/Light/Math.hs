module Light.Math
  ( quadratic, radicalInverse
  , vanDerCorput, sobol, reverseBits
  )
where

import Data.List
import Data.Bits
import Data.Word

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

vanDerCorput :: Word32 -> Word32 -> Double
vanDerCorput n scramble = fromIntegral num / fromIntegral denom
  where num   = ((reverseBits n `xor` scramble) `shiftR` 8) .&. 0xFFFFFF
        denom = (1 :: Word32) `shiftL` 24

sobol :: Word32 -> Word32 -> Double
sobol n scramble = fromIntegral ((scramble' `shiftR` 8) .&. 0xFFFFFF) / fromIntegral ((1 :: Word32) `shiftL` 24)
  where vs = take 32 $ iterate (\x -> x `xor` (x `shiftR` 1)) (1 `shiftL` 31)
        ns = (map (.&. 1) . take 32 . iterate (`shiftR` 1)) n
        scramble' = foldl xor scramble $ zipWith (*) vs ns

reverseBits :: Word32 -> Word32
reverseBits = r1 . r2 . r4 . r8 . r16
  where r16 n = (n `shiftL` 16) .|. (n `shiftR` 16)
        r8  n = ((n .&. 0x00FF00FF) `shiftL` 8 .|. (n .&. 0xFF00FF00) `shiftR` 8)
        r4  n = ((n .&. 0x0F0F0F0F) `shiftL` 4 .|. (n .&. 0xF0F0F0F0) `shiftR` 4)
        r2  n = ((n .&. 0x33333333) `shiftL` 2 .|. (n .&. 0xCCCCCCCC) `shiftR` 2)
        r1  n = ((n .&. 0x55555555) `shiftL` 1 .|. (n .&. 0xAAAAAAAA) `shiftR` 1)
