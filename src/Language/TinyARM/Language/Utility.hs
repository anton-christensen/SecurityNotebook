module TinyARM.Language.Utility where

import Data.Bits
import Data.Int
import Data.Word

type MachineInt = Int64


bitSlice :: (Bits a, Num a) => a -> Int -> Int -> a
bitSlice x start end = (shiftR x start) .&. (complement $ shiftL (-1) (end - start))
{-# SPECIALIZE bitSlice :: Word32 -> Int -> Int -> Word32 #-}
{-# INLINE bitSlice #-}


signExtend :: Int -> MachineInt -> MachineInt
signExtend l n = if testBit n (l-1)
                 then n-2^l
                 else n

machineIntToShamt :: MachineInt -> Int
machineIntToShamt = fromIntegral