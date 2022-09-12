module Data.Word.Encoding where

import Data.Word (Word8, Word32)
import Data.Bits ((.&.), shiftL, shiftR)

toWord32 :: [Word8] -> Maybe Word32
toWord32 [a,b,c,d] = Just $
  fromIntegral a `shiftL` 24 +
  fromIntegral b `shiftL` 16 +
  fromIntegral c `shiftL` 8 +
  fromIntegral d
toWord32 _ = Nothing

fromWord32 :: Word32 -> [Word8]
fromWord32 w =
  [ fromIntegral ((w .&. 0xFF000000) `shiftR` 24)
  , fromIntegral ((w .&. 0x00FF0000) `shiftR` 16)
  , fromIntegral ((w .&. 0x0000FF00) `shiftR` 8)
  , fromIntegral ((w .&. 0x000000FF) `shiftR` 0)
  ]
