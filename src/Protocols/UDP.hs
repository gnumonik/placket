module UDP where

import           Classes
import           Control.Lens                   ((^.))
import qualified Data.ByteString                as BS
import           Data.Serialize                 (Get, Put, Serialize (..),
                                                 getBytes, getWord16be,
                                                 putByteString, putWord16be,
                                                 remaining, runGet, runPut)
import           Data.Word                      (Word16, Word8)
import           PrimTypes

import           Control.Monad.Identity         (Identity (runIdentity))
import qualified Control.Monad.Trans.State.Lazy as ST
import           Data.Bits                      (Bits (complement))
import           Data.Maybe                     (fromJust, isJust)
import           PrettyPrint
import           PrimFuncs                      (addComplement, makeWord16)

instance Serialize UDPMessage where
    get = getUDPMessage
    put = putUDPMessage


getUDPMessage :: Get UDPMessage
getUDPMessage = do
    sourceport  <- getWord16be
    destport    <- getWord16be
    udplength   <- getWord16be
    udpchecksum <- getWord16be
    return $! UDPMessage sourceport destport udplength udpchecksum 

runGetUDPMessage :: BS.ByteString -> Either String UDPMessage
runGetUDPMessage = runGet getUDPMessage

putUDPMessage :: UDPMessage -> Put
putUDPMessage udp = do
    putWord16be       $ udp ^. uSrc
    putWord16be       $ udp ^. uDst
    putWord16be       $ udp ^. uLen
    putWord16be       $ udp ^. uChecksum


runPutUDPMessage :: UDPMessage -> BS.ByteString
runPutUDPMessage x = runPut $ putUDPMessage x

calcL4Checksum :: BS.ByteString -> Word16
calcL4Checksum bstring = complement . foldr1 addComplement $ toWord16s  $ BS.unpack bstring
  where
        toWord16s xs = runIdentity $ ST.evalStateT (toWord16st xs) (0,Nothing,[])
        toWord16st :: [Word8] -> ST.StateT (Word16,Maybe Word8, [Word16]) Identity [Word16]
        toWord16st []       = do
            s <- ST.get
            let myCount = (\(a,_,_) -> a) s
            let w8 = (\(_,b,_) -> b) s
            let w16s = (\(_,_,c) -> c)  s
            if myCount `mod` 2 /= 0 && isJust w8
               then return $! w16s ++ [makeWord16 (fromJust w8) 0]
               else return w16s
        toWord16st (w8:w8s) = do
            s' <- ST.get
            ST.modify $ \(x,y,z) -> (x+1,y,z)
            let s = (\(_,b,_) -> b) s'
            if isJust s
                then do
                    let w16 = makeWord16 (fromJust s) w8
                    ST.modify $ \(a,_,x) -> (a,Nothing, x ++ [w16])
                else ST.modify $ \(a,_,x) -> (a,Just w8,x)
            toWord16st w8s

prettyUDP mode udp 
    = let src = ("src",udp ^. uSrc)
          dst = ("dst",udp ^. uDst)
          len = ("len",udp ^. uLen)
          chk = ("chkSum",udp ^. uChecksum)
      in undefined 