{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module PacketIO where

import           Control.Concurrent.STM       (TChan, atomically)
import           Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import           Control.Monad                (void)
import qualified Data.ByteString              as BS
import           Data.Word                    (Word32, Word16, Word64)
import           Network.Pcap                 
import           PrimFuncs                    ((|>), toWord64)
import           Data.Time.Clock.System
import           Data.Serialize 
import qualified Data.Text as T
import System.IO
import Serializer
import FieldClasses (pprint, PrintMode(..))
import Staging
import Network.Socket.Internal 
import qualified Data.Vector as V
import PrettyPrint
import Classes
import Network.Info 

convIP4 (IPv4 a) = case decode . BS.reverse . encode $ a of 
  Right ip -> IP4Address ip 
  Left err -> error "Could not decode IP address"
convIP6 (IPv6 a b c d) = encode d <> encode c <> encode b <> encode a 
convMAC (MAC a b c d e f) = MacAddr a b c d e f 

prettyInterface :: NetworkInterface -> T.Text 
prettyInterface (NetworkInterface nm i4 i6 mac) 
    = spaceRow'  
    <>  makeLabelRowLJ (T.pack nm)
    <>  makeDataRowLJ  ["  |-IP4: " <> (pprint Dflt $ convIP4 i4)]
    <>  makeDataRowLJ  ["  |-IP6: " <> (pprint Hex $ convIP6 i6)]
    <>  makeDataRowLJ  ["  |-MAC: "  <>(pprint Dflt $ convMAC mac)]
    <>  dashRow 
    <>  spaceRow'

data PcapGlobalHdr 
    = PcapGlobalHdr {_magicNumber   :: !Word32
                    ,_versionMajor :: !Word16
                    ,_versionMinor :: !Word16
                    ,_gmtOffset    :: !Word32
                    ,_sigfigs      :: !Word32
                    ,_snaplen      :: !Word32
                    ,_l2Type       :: !Word32}

putGlobalHeader :: PcapGlobalHdr -> PutM ()
putGlobalHeader (PcapGlobalHdr m vmj vmn off sf sl l2)
    = putWord32be m 
    *> putWord16be vmj 
    *> putWord16be vmn
    *> putWord32be off 
    *> putWord32be sf
    *> putWord32be sl
    *> putWord32be l2

-- 0010 0001 , 0011 0011, 0110 0011 ,0000 1101,01001
devinfo :: IO T.Text
devinfo = do
  T.concat . map prettyInterface <$> getNetworkInterfaces
  
        



   

mkPcapHdr :: Word32 -- snapLen
          -> Word32 -- Data Link Type (Eth = 1)
          -> PcapGlobalHdr
mkPcapHdr  snaplen network 
    = PcapGlobalHdr 0xa1b2c3d4 2 4 0 0 snaplen network 

mkPacketHeader :: IO PktHdr
mkPacketHeader = do
    MkSystemTime secs nsecs <- getSystemTime
    let newSecs  = fromIntegral secs :: Word32
    return $! PktHdr newSecs nsecs 0 0
      
putPktHdr :: PktHdr -> PutM ()
putPktHdr (PktHdr w1 w2 w3 w4) = mapM_ putWord32be [w1,w2,w3,w4]

openDumpFile :: FilePath -> IOMode -> IO Handle
openDumpFile fPath mode = openBinaryFile fPath mode

initDumpFile :: FilePath -> IO Handle
initDumpFile fPath = do
    hdl <- openBinaryFile fPath AppendMode 
    let eth65535 = mkPcapHdr 65535 1
    let myGlobalHeader = runPut . putGlobalHeader $ eth65535
    BS.hPut hdl myGlobalHeader
    return hdl 

type SizeError = T.Text 

prettyStats :: PcapHandle -> IO T.Text
prettyStats hdl = do
    (Statistics rcvd dropd ifdropd) <- statistics hdl
    let myStats = "Pcap Statistics:\n Received: " 
                 <> (T.pack . show $  rcvd) 
                 <> "\nDropped: " 
                 <> (T.pack . show $ dropd)
                 <> "\nDropped by interface: " 
                 <> (T.pack . show $ ifdropd)
    return myStats 

dumpPacket :: Handle -> (PktHdr, V.Vector ProtocolMessage) -> IO (Maybe SizeError)
dumpPacket hdl ((PktHdr a b _ _),pkt) = do
    let !bs = serializeMessage pkt
    let len = fromIntegral $ BS.length bs
    if len > 65535
        then return $ Just $ "Error in dumpPacket! Packet size exceeds 65535." 
                    <> " Cannot write to file."
        else do 
            let newHdrBS = runPut . putPktHdr $ PktHdr a b len len
            BS.hPut hdl $! newHdrBS <> bs
            return Nothing 





atomicSendBS :: TMVar () -> PcapHandle -> BS.ByteString -> IO ()
atomicSendBS lock handle bstring = do
    void $ atomically $ takeTMVar lock
    sendPacketBS handle (bstring)
    void $ atomically $ putTMVar lock ()

atomicNextBS :: PcapHandle -> TMVar () -> IO (Word64,BS.ByteString)
atomicNextBS phandle lock = do
    void $ atomically $ takeTMVar lock
    p <- nextBS phandle
    void $ atomically $ putTMVar lock ()
    let timestamp = (\x -> toWord64 (hdrSeconds x) (hdrUseconds x)) . fst $  p
    let lazyBS = snd $ p
    return (timestamp,lazyBS)

atomicNextBS' :: PcapHandle -> TMVar () -> IO (PktHdr,BS.ByteString)
atomicNextBS' phandle lock = do
    void $ atomically $ takeTMVar lock
    p <- nextBS phandle
    void $ atomically $ putTMVar lock ()
    return p

initOffline :: String -> IO PcapHandle
initOffline fPath = openOffline fPath 

initPCAP :: IO PcapHandle
initPCAP = do
    device <- lookupDev
    hdl <- pcapCreate device
    setImmediateMode hdl True
    setSnapLen hdl 65535
    pcapActivate hdl
    return hdl 

initPCAPDefault :: IO PcapHandle
initPCAPDefault = do
    device <- lookupDev
    hdl <- pcapCreate device
    setImmediateMode hdl True
    setSnapLen hdl 65535
    pcapActivate hdl
    return hdl


initPCAPWithOptions :: String -> Bool -> Int -> IO PcapHandle
initPCAPWithOptions device immediate snaplen = do
    hdl <- pcapCreate device
    setImmediateMode hdl immediate
    setSnapLen hdl snaplen
    pcapActivate hdl
    return hdl




