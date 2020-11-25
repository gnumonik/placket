{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module SourceManager where

import Control.Concurrent.STM

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class 
import Data.Machine
import Data.Machine.Lift 
import qualified Data.Text as T 
import Control.Monad.IO.Class 
import Data.Word 
import Control.Monad
import Data.Maybe
import FactoryTypes
import PacketSources 
import MyReaderT
import UtilityMachines (writeChan)

pump :: SourceT IO ()
pump = repeatedly $ do
    yield ()


sinkManager :: TBQueue ToSink 
            -> TVar Int
            -> PacketMachine
sinkManager toSinkQueue outCount = execStateM Disconnected $ go toSinkQueue outCount
    where
        go :: TBQueue ToSink -> TVar Int -> MachineT (StateT SinkState IO) (Is Message) Message
        go sinkQ oCount = repeatedly $ do

            maybeCommand <- liftIO . atomically . tryReadTBQueue $ sinkQ

            lift $ runCommand maybeCommand 

            nextMsg <- await 

            liftIO . atomically . modifyTVar' oCount $ (+1)

            s <- lift get

            case s of 

                ConnectedTo someQ -> do
                    liftIO . atomically . writeTBQueue someQ $ nextMsg 

                Disconnected -> return () 

        runCommand :: (Maybe ToSink) -> StateT SinkState IO () 
        runCommand Nothing = return () 
        runCommand (Just cmd) = case cmd of

            ConnectTo newQ -> do
                modify $ \x -> ConnectedTo newQ

            Disconnect -> do
                modify $ \x -> Disconnected 


sourceManager :: DisplayChan 
              -> TBQueue ToServer
              -> TBQueue ToSrc
              -> [TBQueue Message] 
              -> MachineT (StateT SrcState IO) (Is Message) Message 
sourceManager dChan toServer toSrc msgQueue = repeatedly $ do

    nextCommand <- liftIO $ atomically $ tryReadTBQueue toSrc 
    when (isJust nextCommand) $ do 
        lift $ runSourceCommand (fromJust nextCommand) toServer msgQueue

    nextMsg        <- await
 
    (SrcState _ b cnt) <- lift get
    liftIO  $ atomically $  modifyTVar' cnt (+1)

    when (b == SRC_ACTIVE) $ do 
        yield nextMsg



        --liftIO . atomically . writeTChan dChan $ "SourceManager got a packet: " <> T.pack (show nextMsg)

   where
       runSourceCommand :: ToSrc 
                        -> TBQueue ToServer 
                        -> [TBQueue Message ]
                        -> StateT SrcState IO ()
       runSourceCommand cmd toServer' qs
            = case cmd of

                STOP -> do

                    (SrcState myTag myState cnt) <- get

                    writeQ toServer' (NOMOREPACKETS . fromIntegral $ myTag)

                    modify $ \(SrcState t _ cnt) -> SrcState t SRC_INACTIVE cnt



mkFactoryV2 :: Maybe T.Text -> MachineData -> SourceData -> MyReader (Word16,Factory)
mkFactoryV2 fName mData sData = do
    dChan <- askForDisplayChan

    MachineData mch mSch <- pure mData
    SourceData  src sSch <- pure sData

    serverQ <- askForServerQueue

    CappedSrc cappedSrc msgQueues <- capSource src

    facID <- mkRandomFacID

    let myFName = case fName of
          Nothing -> T.pack . show $ facID
          Just aName -> aName

    srcQueue <- liftIO $ newTBQueueIO 1000

    snkQueue <- liftIO $ newTBQueueIO 1000 

    inCount  <- liftIO $ newTVarIO 0

    outCount <- liftIO $ newTVarIO 0 

    let startTime = Nothing

    let fThread = Nothing

    let mySrcManager = plug $ cappedSrc ~> (execStateM (SrcState facID SRC_ACTIVE inCount) $ 
                                 sourceManager dChan serverQ srcQueue msgQueues)

    let mySinkManager = sinkManager snkQueue outCount 

    

    let theMachine = mySrcManager ~> mch ~> mySinkManager 

    let myFactory = Factory 
                        theMachine
                        myFName 
                        sData 
                        mData 
                        fThread 
                        srcQueue
                        snkQueue 
                        False 
                        inCount 
                        outCount 
                        startTime
                        msgQueues

    return (facID,myFactory) 




