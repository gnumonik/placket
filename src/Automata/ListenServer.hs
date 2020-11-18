{-# LANGUAGE OverloadedStrings #-}
module ListenServer where

import FactoryTypes
import Control.Concurrent.STM
import Classes 
import Control.Monad 
import Control.Monad.IO.Class 
import Data.Maybe 
import Control.Concurrent 
import Data.Time.Clock 
import Control.Concurrent.Async
import Control.Lens 
import Data.Machine 
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T



listenServer :: DisplayChan 
             -> TChan Message 
             -> TChan ListenRequest
             -> TBQueue Message  
             -> StateT  (Map RequestID ListenData) IO ()
listenServer dChan sendCh listenReqChan  packetQueue
    = forever $ do
        
        maybeRequest <- liftIO $ atomically $ tryReadTChan listenReqChan 
        when (isJust maybeRequest) $ do
           -- liftIO . atomically . writeTChan dChan $ "ListenServer got a request!"
            processListenRequest (fromJust maybeRequest)


        maybePacket <- liftIO $ atomically $ tryReadTBQueue packetQueue
        when (isJust maybePacket) $ do
            processPacket (fromJust maybePacket) 
       --     liftIO . atomically . writeTChan dChan $ "ListenServer processed a packet"

        processTimeouts sendCh 

        liftIO $ threadDelay 500

   where
    processTimeouts :: TChan Message ->  StateT (Map RequestID ListenData) IO ()
    processTimeouts sndchan = do 
        s <- get
        mapM_ (go sndchan) (Map.toList s)
       where
           go :: TChan Message -> (RequestID,ListenData) -> StateT (Map RequestID ListenData) IO ()
           go sChan (rid, ldat) = do
               theTime <- liftIO $ getCurrentTime
               let diffTime = (\x -> fromRational x :: Double ) $! toRational $ diffUTCTime theTime (ldat ^. timeSent)
               when (diffTime > ldat ^. timeOut) $ do
                   liftIO $ atomically . writeTChan dChan $ "Timeout on request ID #" <> (T.pack . show $ rid)
                   modify $ Map.adjust (\x -> over timeOutCount (+1) x) rid
                   let f = ldat ^. retryF
                   case f (ldat ^. timeOutCount) diffTime of
                       Just r -> do
                           modify $ Map.adjust (\v -> set timeOut r v) rid
                           liftIO . atomically . writeTChan sChan $ ldat ^. packet
                       Nothing -> do
                           modify $ Map.delete rid
                         --  liftIO . atomically . writeTChan respChan $ 
                         --       TimeOut (ldat ^. listenerID) rid 
                           liftIO . atomically . writeTChan dChan $  "Timeout on request."

               

    processListenRequest :: ListenRequest -> StateT (Map RequestID ListenData) IO ()
    processListenRequest (ListenRequest lID lData) = do
        modify $ Map.insert lID lData
        --liftIO . atomically . writeTChan dChan $ "ListenServer Processed a request"

    processPacket :: Message
                  -> StateT (Map RequestID ListenData) IO ()
    processPacket msg   = do
        s <- get
        mapM_ (go msg) (Map.toList s)
       where
           go :: Message -> (RequestID,ListenData) -> StateT (Map RequestID ListenData) IO ()
           go msg (rid , ldata) = do
               (ListenData pkt pfil toCnt tSent to ret lID mach) <- pure ldata 
               when (pfil msg) $ do
                   void $ liftIO $ async $ runT (source [msg] ~> mach)
                   --liftIO . atomically . writeTChan dChan $ "\nListen Server processed a packet! woo!"
               modify $! Map.delete rid