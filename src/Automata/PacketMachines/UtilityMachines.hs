{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module UtilityMachines where

-- Utility machine.
import FactoryTypes
import Control.Concurrent.STM
import Network.Pcap (sendPacketBS, PcapHandle)
import qualified Data.Vector as V
import Control.Monad.IO.Class
import LibTypes (ProtocolMessage)
import Data.Machine
import Control.Category (Category)
import Control.Monad.Trans.State.Strict
import Control.Concurrent (threadDelay)
import PacketIO (mkPacketHeader)
import Control.Monad (when, forever)
import Serializer (serializeMessage)
import Data.Machine.Lift (execStateM, runReaderM)
import Data.Maybe

queueReader :: TBQueue a -> SourceT IO a
queueReader q = plug $ repeatedly $ do
    nextMsg <- liftIO $ atomically $ readTBQueue q
    yield nextMsg

-- Utility machine to facilitate the fanout syntax. 
semigrouper :: (Monad m, Category k) => MachineT m (k a) [a]
semigrouper = repeatedly $ do
    nextMsg <- await
    yield [nextMsg]

------ 
-- Delay. Takes an Int, waits that many microseconds, then yields the message it was 
--   passed as input. Technically an effectful machine but needs to go here to prevent
--   cyclic imports.
------

delay :: (Category k, MonadIO m) => Int -> MachineT m (k o) o
delay n = repeatedly $ do
    nextMsg <- await
    liftIO $ threadDelay n
    yield nextMsg 


--Blackhole. Accepts packets, never yields them. 
------
blackHole :: PacketMachine
blackHole = repeatedly $! do
    _ <- await
    liftIO $ return () 



-- IMPORTANT: Length for generated packets is set to 0. 
-- The only context in which the length matters is when packets are being
-- written to a dump file, and the dump machine recalculates the length and 
-- checks that packets do not exceed 65535 bytes (Todo: Allow for variable length 
-- max dump size). 
addHdr :: MachineT IO (Is (V.Vector ProtocolMessage)) Message
addHdr = repeatedly $ do
    nextMsg <- await
    hdr <- liftIO  mkPacketHeader
    return $! (hdr,nextMsg)


-- Utility functions necessary for the operation of particular packet machines 

packetSender :: DisplayChan -> TMVar () -> SendChan-> PcapHandle -> IO ()
packetSender dChan lock chan hdl = forever $ do
    (_,nextMsg) <- atomically . readTChan $ chan
    let !serialized = serializeMessage $! V.force nextMsg
    sendPacketBS hdl serialized
  --  atomically . writeTChan dChan $ "PacketSender sent packet: " <> T.pack (show nextMsg)
    threadDelay 250

writeChan :: MonadIO m => TChan a -> a -> m ()
writeChan ch x = liftIO . atomically . writeTChan ch $ x 

getID' :: Num b => TVar b -> IO b
getID' myTVar = do
    myRID <- readTVarIO myTVar
    atomically $ modifyTVar' myTVar (+1)
    return myRID 

liftMachineR :: Env -> MachineT (MyReader) (Is Message) Message -> PacketMachine
liftMachineR e mach = runReaderM e mach

liftMachineS :: s -> MachineT (StateT s IO) (Is Message) Message -> PacketMachine
liftMachineS s mach = execStateM s mach

type PacketOperation = Message -> IO (Maybe Message)

apEffect :: IO () -> PacketOperation
apEffect fx m = liftIO fx >> return (Just m)

liftF :: (V.Vector ProtocolMessage -> Maybe (V.Vector ProtocolMessage)) -> PacketOperation
liftF f x =  do
    let y = mapM f x
    return y

pMach :: PacketOperation -> PacketMachine
pMach f = repeatedly $ do
    !nextPacket <- await
    !appliedF <- liftIO $ f nextPacket
    when (isJust $ appliedF) $ do
        yield $ fromJust appliedF

liftMachine :: (ProtocolMessage -> ProtocolMessage) -> PacketMachine
liftMachine f = repeatedly $ do
    nextMsg <- await
    yield $ fmap (V.map f) nextMsg 
