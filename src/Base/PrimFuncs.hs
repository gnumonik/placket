{-# LANGUAGE BinaryLiterals #-}
module PrimFuncs where


import           Control.Exception (BlockedIndefinitelyOnMVar (BlockedIndefinitelyOnMVar),
                                    BlockedIndefinitelyOnSTM (BlockedIndefinitelyOnSTM),
                                    Handler (Handler), catches, throwIO)
import           Data.Binary       (encode)
import           Data.Binary.Get   (getWord32be, getWord8, runGet)
import           Data.Bits         (Bits (setBit, shiftL, shiftR, (.&.), (.|.)))
import           Data.Word         (Word16, Word32, Word64, Word8)
import           Say               (sayString)





setIf ::  Bool -> Int ->  Word16 -> Word16
setIf myBool toSet w16 = if myBool then setBit w16 toSet else w16

hasLocked :: String -> IO a -> IO a
hasLocked msg action =
  action `catches`
  [ Handler $ \exc@BlockedIndefinitelyOnMVar -> sayString ("[MVar]: " ++ msg) >> throwIO exc
  , Handler $ \exc@BlockedIndefinitelyOnSTM -> sayString ("[STM]: " ++ msg) >> throwIO exc
  ]

makeWord24 :: Word8 -> Word8 -> Word8 -> Word32
makeWord24 a b c = shiftL (toWord32 a) 16 .|. toWord32 (makeWord16 b c)

makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 w8one w8two = shiftL (to16 w8one :: Word16) 8 .|. to16 w8two
    where
        to16 w8 = fromIntegral w8 :: Word16

makeWord32 :: Word16 -> Word16 -> Word32
makeWord32 w16one w16two = shiftL (to32 w16one :: Word32) 16 .|. to32 w16two
    where
        to32 = toWord32

splitWord16 :: Word16 -> (Word8,Word8)
splitWord16 w16 = (to8 w16,to8' w16)
    where
        to8  w = (\x -> fromIntegral x :: Word8) $ shiftR (w .&. (65280 :: Word16)) 8
        to8' w = (\x -> fromIntegral x :: Word8) $  w .&. (255 :: Word16)

splitWord32 :: Word32 -> (Word16,Word16)
splitWord32 w32 = (to16 w32, to16' w32)
    where 
        to16  w = (\x -> fromIntegral x :: Word16) $ shiftR (w .&. (4294901760 :: Word32)) 16
        to16' w = (\x -> fromIntegral x :: Word16) $ w .&. (65535:: Word32)

toWord32 :: Integral a => a -> Word32
toWord32 x = fromIntegral x :: Word32

toWord16 :: Integral a => a -> Word16
toWord16 x = fromIntegral x :: Word16

toWord8 :: Integral a => a -> Word8
toWord8  x = fromIntegral x :: Word8

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

addComplement :: Word16 -> Word16 -> Word16
addComplement w16a w16b = if overflow > 0 then  result + overflow else result
   where
       a = toWord32 w16a
       b = toWord32 w16b
       (overflow, result) = splitWord32 (a + b)

toWord64 :: Word32 -> Word32 -> Word64
toWord64 w1 w2 =  (fromIntegral w2 :: Word64) .|.  (shiftL ((fromIntegral w1 :: Word64) .&. 4294967295) 32)


flipW32 :: Word32 -> Word32
flipW32 x = runGet getWord32be $ encode $ (x :: Word32)


w32ToIP :: Word32 -> String
w32ToIP w = runGet getIPStringFromW32  (encode w)
   where
       getIPStringFromW32 = do
           a <- getWord8
           b <- getWord8
           c <- getWord8
           d <- getWord8
           return $! show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

