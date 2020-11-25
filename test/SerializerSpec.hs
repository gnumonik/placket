module SerializerSpec where

import Serializer
import FieldClasses 
import UserAgent
import PacketIO 
import Network.Pcap 
import Data.Proxy
import PrimTypes
import qualified Data.Vector as V
import Control.Monad
import qualified Data.Text as T 




testSerializer :: IO () 
testSerializer = do
  hdl <- initPCAP
  go 100 hdl
 where
   go :: Int -> PcapHandle -> IO () 
   go n' hdl' = do
     when (n' > 0) $ do  
      (hdr,bs) <- nextBS hdl' 
      case incrementalDeserialize (Proxy :: Proxy EthernetFrame) bs (V.empty) of 
        Left err -> do
          putStr . T.unpack $ err 
          putStr . T.unpack . pprint Hex $ bs 
          go (n'-1) hdl'
        Right deserialized -> do
          putStr $ "Successfully deserialized packet #" ++ show n' ++ "\n"
          putStr $ "Checking if reserialize matches\n" 
          if serializeMessage deserialized == bs 
            then do 
              putStr $ "Success! ByteStrings Match!\n"
              go (n'-1) hdl'
            else do 
              putStr $ "Packets do not match!\n\n"
              putStr $ "Original ByteString:\n" ++ T.unpack (pprint Hex bs) ++ "\n\n"
              putStr $ "Reserialized ByteString:\n" ++ T.unpack (pprint Hex $ serializeMessage deserialized)
              go (n' - 1) hdl' 

