{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module PacketSources where

import qualified Data.Vector as V
import LibTypes (ProtocolMessage)
import FactoryTypes
import Data.Machine
import Control.Monad.Trans.State.Strict
import Data.Machine.Lift (execStateM)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import PacketIO (atomicNextBS', atomicNextBS, mkPacketHeader)
import Control.Lens
import Data.Proxy 
import Control.Concurrent.STM (TMVar, newTBQueueIO)
import Serializer
import UtilityMachines (queueReader, delay)
import PrimTypes (EthernetFrame(EthernetFrame))
import THWrappers
import NextProtocol
import Data.Serialize (Serialize)
import Network.Pcap




pump :: SourceT IO ()
pump = repeatedly $ do
    yield ()


generator :: [V.Vector ProtocolMessage] -> Repeats -> Delay -> SourceT IO Message 
generator myMsgs myRepeats myDelay 
    = let myRegulator = pump ~> execStateM (myRepeats,Just 0) (regulator . V.force $ V.fromList myMsgs)  
      in case myDelay of
          Just n  -> myRegulator ~> delay n 
          Nothing -> myRegulator 
   where
    regulator :: V.Vector (V.Vector ProtocolMessage) 
                -> MachineT
                (StateT (Maybe Int, Maybe Int) IO )
                (Is ()) 
                Message
    regulator ms = repeatedly $ do
        _ <- await 
        count <- snd <$> lift get
        case count of
            Just n ->
                case  (V.force ms) V.!? n of
                    Just m -> do
                        hdr <- liftIO mkPacketHeader
                        yield $ (hdr,m)
                        lift . modify $ over _2 $ fmap (+1)
                    Nothing -> do
                        lift . modify $ over _1 (fmap $ \x -> x - 1)
                        lift . modify $ over _2 (fmap $ const 0)
                        rpts <- fst <$> lift get
                        case rpts of
                            Just n -> 
                                if n > 0
                                    then return ()
                                    else do
                                    lift . modify $ over _2 $ const Nothing
                                    lift . modify $ over _1 $ const Nothing
                                    stop
                            Nothing -> stop
            Nothing -> return ()

readPcap :: (Possibly a ProtocolMessage, WrapProtocol a, NextProtocol a, Serialize a)  
         => PcapHandle 
         -> TMVar () 
         -> Proxy a 
         -> MachineT IO (Is ()) (PktHdr, V.Vector ProtocolMessage)
readPcap hdl lock prox = pump ~> go 
   where 
    go = repeatedly $ do
        _     <- await
        (h,b) <- liftIO $ atomicNextBS' hdl lock
        case incrementalDeserialize prox b V.empty of
            Left err -> return () 
            Right m  -> yield (h,m)



capSource :: PacketSrc -> MyReader CappedSrc
capSource s = reduceSource s >>= \(Plugged m qs) -> return $ CappedSrc m qs 

reduceSource :: PacketSrc -> MyReader PacketSrc
reduceSource pSrc = case pSrc of
    Plugged m qs -> return $ Plugged m qs 
    Generator m -> return $ Plugged (pump ~> m) [] 
    Listener    -> do
        msgQueue <- liftIO $ newTBQueueIO 5000
        return $ Plugged (plug $ queueReader msgQueue) [msgQueue]
    Why m1 m2 -> do
        (Plugged m1' qs1) <- reduceSource m1
        (Plugged m2' qs2) <- reduceSource m2
        return $ Plugged (why m1' m2') (qs1 <> qs2)
    Tea m1 m2 -> do
        (Plugged m1' qs1) <- reduceSource m1
        (Plugged m2' qs2) <- reduceSource m2
        return $ Plugged (why m1' m2') (qs1 <> qs2)

why :: MachineT IO (Is ()) Message 
    -> MachineT IO (Is ()) Message 
    -> MachineT IO (Is ()) Message
why s1 s2 = capWye (plug s1) (plug s2) go
    where
        go :: WyeT IO Message Message Message
        go = repeatedly $ do
            x <- awaits X
            yield x 
            y <- awaits Y
            yield y

tea :: MachineT IO (Is ()) Message 
    -> MachineT IO (Is ()) Message 
    -> MachineT IO (Is ()) Message
tea s1 s2 = capT (plug s1) (plug s2) go
    where
        go :: TeeT IO Message Message Message
        go = repeatedly $ do
            l <- awaits L
            yield l
            r <- awaits R
            yield r 