{-# LANGUAGE TypeApplications, ScopedTypeVariables, RankNTypes, TemplateHaskell, FlexibleContexts, FunctionalDependencies, UndecidableInstances, MultiWayIf, OverloadedStrings, BangPatterns  #-}
module ARPCache where

import FactoryTypes
import PacketServer
import PrimTypes 
import Classes 
import Control.Monad
import Wrappers 
import Control.Monad.Trans.State.Strict 
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue 
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V 
import Data.Maybe 
import Control.Lens.TH 
import Control.Lens 
import Data.Word ()
import Data.Time.Clock
import qualified Data.Text as T 
import LibTypes
import IP4 
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)



arpServer :: TBQueue Message ->  DisplayChan -> StateT (TVar ARPCache) IO ()
arpServer msgQ dChan = forever $ do 
    !nextMsg <- liftIO . atomically $ tryReadTBQueue msgQ
    case snd <$> nextMsg of 
            Just msg ->  do
            let arped = V.force . V.map (as @ARPMessage ) . V.filter (is @ARPMessage) $  msg
            s <- get 
            liftIO $! V.mapM_ (updateARPCache s) (V.force arped)
       -- liftIO $! atomically $! writeTChan dChan $! "ARP DID A PACKET!"
    liftIO $ threadDelay 1000

   where

       updateARPCache :: forall t. TVar ARPCache -> Maybe ARPMessage -> IO ()
       updateARPCache cacheVar maybeMsg  = do
            when (isJust maybeMsg) $ do
                currentTime <- getCurrentTime
                atomically . modifyTVar' cacheVar $! \x ->  
                    goIP currentTime (fromJust maybeMsg) x

           where
                goIP :: UTCTime
                     -> ARPMessage 
                     -> Map IP4Address (MacAddr,UTCTime) 
                     -> Map IP4Address (MacAddr, UTCTime)
                goIP !theTime !msg !myMap 
                    = let targetAddrs   =  (msg ^. aTpa, msg ^. aTha)
                          newMap        = 
                              if msg ^. aOp == 2 
                                  then checkAndUpdate theTime targetAddrs myMap
                                  else myMap  
                      in newMap
                
                checkAndUpdate :: UTCTime
                               -> (IP4Address,MacAddr) 
                               -> Map IP4Address (MacAddr, UTCTime) 
                               -> (Map IP4Address (MacAddr, UTCTime)) 
                checkAndUpdate !time !(ip,mac) !myMap = 
                    case Map.lookup ip myMap of
                        Just (_,t) -> do
                            let diffTime = (\x -> fromRational x :: Double) . toRational $  diffUTCTime time t
                            if | (diffTime > 120) && (unIP4 ip/= 0) -> 
                                     Map.insert ip (mac,time) myMap 
                               | otherwise ->  myMap 
                        Nothing ->  Map.insert ip (mac,time) myMap 



prettyIPTable :: Map IP4Address (MacAddr,UTCTime) -> T.Text 
prettyIPTable myMap = "ARP Table (IP Keyed):\n"
                    <> (T.concat . map go $ Map.toList myMap)
    where
        go :: (IP4Address, (MacAddr,UTCTime)) -> T.Text 
        go (ip ,(mac,time)) = "IP: " 
                            <> prettifyIP (unIP4 ip) 
                            <> " | " 
                            <> "MAC: " <> prettyMac mac 
                            <> " | Time seen: "
                            <> (T.pack $ show time)
                            <> "\n"


