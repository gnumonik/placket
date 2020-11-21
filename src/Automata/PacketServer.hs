{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module PacketServer where
    
import Network.Pcap 
import Control.Concurrent.STM

import Control.Monad.Trans.State.Strict
import qualified Data.Vector as V
import PrimTypes
import Data.Proxy 
import Control.Monad 
import Data.Maybe 
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import Data.Machine
import PacketIO
import Serializer
import FactoryTypes
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Staging
import qualified Data.Text as T
import UtilityMachines (writeChan)


packetServer :: DisplayChan -> PcapHandle -> TMVar () -> TBQueue (ToServer) -> StateT (Map Int [TBQueue Message]) IO () 
packetServer dChan hdl lock commandQueue = forever $ do

         nextCmd <- liftIO $ atomically $ tryReadTBQueue commandQueue  
         when (isJust nextCmd) $ do
             runServerCommand (fromJust nextCmd)

         !(hdr,bs) <- liftIO $ atomicNextBS' hdl lock  

         let !next' = V.force <$> incrementalDeserialize (Proxy @EthernetFrame) bs V.empty

         case next' of
             Right !msg -> do
                 s <- get
                 liftIO  $ dispatchPacket (hdr,msg) s
                 --liftIO . atomically . writeTChan dChan $ "SERVER DID PACKET"
             Left  str   -> return ()
             
         liftIO $ threadDelay 500
   where 
    dispatchPacket :: Message  -> Map Int ([TBQueue Message]) -> IO () 
    dispatchPacket !msg !s 
        = let queues = concatMap snd . Map.toList $ s
          in mapM_ (\x -> liftIO . atomically . writeTBQueue x $ msg ) queues  
          --liftIO $ atomically $ writeTChan chan $ "dispatch packet's doing it's thing"

    runServerCommand ::  ToServer -> StateT (Map Int [TBQueue Message]) IO ()
    runServerCommand  com  = do 
        case com of
            GIMMEPACKETS tag queue -> do
                s <- get 
                case Map.lookup tag s of
                    Just _ -> return ()
                    Nothing -> do
                        modify $ Map.insert tag queue

            NOMOREPACKETS tag -> do
                s <- get
                case Map.lookup tag s of
                    Just qs -> do 
                        modify $ Map.delete tag
                        liftIO $ mapM_ (atomically . flushTBQueue) qs 
                    Nothing -> return () 

            SHOWACTIVE -> do
                s <- get
                writeChan dChan $ T.pack $ show $ map fst $ Map.toList s 

            
        --liftIO $ atomically $ writeTChan chan $ "RunServerCommand got a command!"

queueReaderM :: MonadIO m => TBQueue o -> MachineT m k (Maybe o)
queueReaderM queue = plug $ repeatedly $ do
    !nextMsg <- liftIO . atomically . tryReadTBQueue $ queue 
    yield nextMsg