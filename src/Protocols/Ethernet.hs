{-# LANGUAGE BangPatterns #-}
module Ethernet where

import           Classes
import           PrimTypes


import           Control.Lens    (set, view, (^.))
import qualified Data.ByteString as BS
import qualified Data.List       as DL (foldl')
import           Data.Serialize  (Get, Put, Serialize (..), getBytes,
                                  getWord16be, getWord32be, getWord8,
                                  putByteString, putWord16be, putWord32be,
                                  putWord8, remaining, runGet, runPut)
import           Data.Word       (Word16, Word32, Word8)
import           Numeric         (readHex)

instance Serialize ARPMessage where
  get = getARPMessage
  put = putARPMessage

instance Serialize EthernetFrame where
  get = getEthernetFrame
  put = putEthernetFrame



--Converts a string repsentation of a mac address
macToMacAddr :: String -> MacAddr
macToMacAddr macstring = macify $ map ((\x -> fromIntegral (x :: Int) :: Word8) . fst) (concatMap readHex $ go macstring "")
  where
    macify [a,b,c,d,e,f] = MacAddr a b c d e f
    macify _ = error "oops! Something went wrong with macToMacAddr"
    go [] [] = []
    go [] acc = go' [] acc
    go (y:ys) acc = 
      if (y /= ':') && (y/= '-') && (y /= ' ') then go ys (acc ++ [y]) else go' ys acc
    go' [] acc' = acc' : go [] []
    go' as acc' = acc' : go as ""

--Take an IP address (string, dotted decimal) and spits out a word32 that corresponds to that string
ipToWord32 :: String -> Word32
ipToWord32 ip = DL.foldl' (\acc oct -> 256*acc + fromIntegral oct) 0 (octets ip) -- Found this one stackexchange, more efficient than what I originally wrote.
  where
    octets :: String -> [Int]
    octets ipstr = map (read :: String -> Int) (go ipstr "")
      where
        --Mutual recursion. go eats up the IP string and stores characters in acc until it runs into a period, whereupon it calls go', which appends each acc that was fed to it by go onto acc', skips one character, then calls go again. Isn't recursion fun?
        go [] []      = []
        go [] acc     = go' [] acc
        go (y:ys) acc = if y /= '.' then go ys (acc ++ [y]) else go' ys acc
        go' [] acc' = acc' : go [] []
        go' as acc' = acc' : go as ""

--Serializer/Encoder for MacAddr. Spits out the octets of the MAC address in order.
putMac :: MacAddr -> Put
putMac x = do
  putWord8 $! view first8 x
  putWord8 $! view second8 x
  putWord8 $! view third8 x
  putWord8 $! view fourth8 x
  putWord8 $! view fifth8 x
  putWord8 $! view last8 x

runPutMac :: MacAddr -> BS.ByteString
runPutMac = runPut . putMac

getMac :: Get MacAddr
getMac = do
  !f8 <- getWord8
  !s8 <- getWord8
  !t8 <- getWord8
  !fo8 <- getWord8
  !fi8 <- getWord8
  !l8 <- getWord8
  return $! MacAddr f8 s8 t8 fo8 fi8 l8
{-# INLINE getMac #-}

runGetMac :: BS.ByteString -> Either String MacAddr
runGetMac = runGet getMac

-- Encoder/serializer for arp messages.
putARPMessage :: ARPMessage -> Put
putARPMessage arp = do
  putWord16be $! view aHrd arp
  putWord16be $! view aPro arp
  putWord8    $! view aHln arp
  putWord8    $! view aPln arp
  putWord16be $! view aOp  arp
  putMac      $! view aSha arp
  putWord32be $! unIP4 $ view aSpa arp
  putMac      $! view aTha arp
  putWord32be $! unIP4 $ view aTpa arp

--Executes the previous encoder/serializer
runPutARPMessage :: ARPMessage -> BS.ByteString
runPutARPMessage = runPut . putARPMessage

--Decoder/deserializer (what's the right word here?) for an ARP message.
getARPMessage :: Get ARPMessage
getARPMessage  = do
  !aHrd' <- getWord16be
  !aPro' <- getWord16be
  !aHln' <- getWord8
  !pln' <- getWord8
  !op'  <- getWord16be
  !aSha' <- getMac
  !aSpa' <- getWord32be
  !aTha' <- getMac
  !aTpa' <- getWord32be
  return $! 
      ARPMessage  aHrd'  aPro' aHln' pln' op' aSha' (mkIP4 aSpa') aTha' (mkIP4 aTpa')

--Runs the previous decoder/deserializer.
runGetARPMessage :: BS.ByteString -> Either String ARPMessage
runGetARPMessage = runGet getARPMessage


getEthernetFrame :: Get EthernetFrame
getEthernetFrame = do
  !ethdest   <- getMac
  !ethsource <- getMac
  !ethtype   <- getWord16be
  return $! EthernetFrame  ethdest ethsource ethtype 
{-# INLINE getEthernetFrame #-}

runGetEthernetFrame :: BS.ByteString -> Either String EthernetFrame
runGetEthernetFrame = runGet getEthernetFrame

putEthernetFrame :: EthernetFrame -> Put
putEthernetFrame frame = do
  putMac            $  frame ^. eDst
  putMac            $  frame ^. eSrc
  putWord16be       $  frame ^. eEtherType

runPutEthernetFrame :: EthernetFrame -> BS.ByteString
runPutEthernetFrame = runPut . putEthernetFrame

