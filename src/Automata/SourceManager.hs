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
import Control.Monad
import Data.Maybe
import FactoryTypes
import MyReaderT
import UtilityMachines (writeChan)

pump :: SourceT IO ()
pump = repeatedly $ do
    yield ()


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
 
    (SrcState _ b _) <- lift get

    when (b == SRC_ACTIVE) $ do 
        yield nextMsg
        (SrcState _ _ cnt) <- lift get 
        liftIO  $ atomically $  modifyTVar' cnt (+1)

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

                    liftIO $ atomically $ writeTBQueue toServer' (NOMOREPACKETS myTag)

                    modify $ \(SrcState t _ cnt) -> SrcState t SRC_INACTIVE cnt

mkFactory :: MachineData -> PacketSrc -> MyReader (Maybe PacketMachine)
mkFactory mData pSrc = do

    serverQ  <- askForServerQueue

    MachineData mch nm commQ activ pCnt sch thrd msgQ <- pure mData 

    mIDs <- askForMachineIDs 

    let tagOfNm = foldr (\(a,b) acc -> if b == nm then Right a else acc ) (Left $ "Error! Machine has no ID!") mIDs

    dChan <- askForDisplayChan

    if not activ 

        then do

            case tagOfNm  of

                Left err -> do
                    chan <- askForDisplayChan
                    liftIO $ atomically $ writeTChan chan err
                    return Nothing 

                Right t -> do 
                    
                    (Plugged src qs) <- pure pSrc 

                    let mySrcManager = plug $ src ~> 
                            (execStateM (SrcState t SRC_ACTIVE pCnt) $ 
                            sourceManager dChan serverQ commQ qs) 

                    return $ Just $ mySrcManager ~> mch

                
        else do
                writeChan dChan $
                    "\nError: Them machine named " 
                    <> (mchName nm)
                    <> "is already running."
                return Nothing    


