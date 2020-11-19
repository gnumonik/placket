{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module EffectfulMachines where




------
-- Send. Sends packets. (More accurately: Writes them to a TChan, where a send worker does the actual sending.)
------
import Control.Concurrent.STM
import qualified Data.Vector as V
import FactoryTypes
import qualified Data.Text as T
import RecordTypes (MsgSelectorExp, ProtocolSelectorExp(ProtocolSelectorExp))
import Data.Machine
import Control.Monad.Trans.State.Strict
import Data.Time.Clock
import System.IO
import LibTypes 
import Control.Monad.IO.Class
import UtilityMachines (getID', writeChan)
import Control.Monad
import Data.Machine.Lift
import Control.Monad.Trans
import PacketIO (atomicSendBS, dumpPacket)
import GPrettyPrint ()
import Network.Pcap
import Serializer 
import Data.Either
import RecordFuncs (evalMsgSelExpPlus, evalMsgSelectorExp, evalProtoSelectExp)


send :: DisplayChan -> TMVar () -> PcapHandle ->PacketMachine
send dchan lock hdl =  repeatedly $ do
    (hdr,nextMsg) <- await
    liftIO $ atomicSendBS lock hdl $! serializeMessage nextMsg
   -- liftIO . atomically . writeTChan dchan $ "'send' is writing a packet to the send channel"
    yield (hdr,nextMsg)



prettyPrint :: DisplayChan -> PrintMode -> PacketMachine
prettyPrint d m = repeatedly $ do
    (hdr,nextMsg) <- await
    let toPrint = T.concat . V.toList $ V.reverse $  V.map (pprint m) nextMsg 
    writeChan d $ toPrint
    yield (hdr,nextMsg)


------
-- Alert. Takes a predicate and a string within quotes, and displays that string when the predicate is satisfied. Can also take no predicate and display the string every time it receives a packet.
------
alert :: Maybe Predicate -> PrintThis -> DisplayChan -> PacketMachine
alert mPred str chan = repeatedly $ do
    (hdr,nextMsg) <- await
    case mPred of
        Just f -> do
            when (f nextMsg) $ do
                liftIO $ atomically $ writeTChan chan str
                yield (hdr,nextMsg)
        Nothing -> do
                liftIO $ atomically $ writeTChan chan str
                yield (hdr,nextMsg)


-- Write n packets in PCAP format to the designated filepath. Note: Uses pcap file format version 2.4.
------
dumpPkt :: Handle -> Int  -> DisplayChan -> FilePath ->  PacketMachine
dumpPkt hdl n dChan fPath = execStateM (Just n) $ repeatedly $ do
    nextMsg <- await
    s <- lift get
    case s of
        Just s' -> do 
            unless (s' <= 0) $ do
                liftIO $ dumpPacket hdl nextMsg
                lift . modify $ fmap (\x -> x - 1)
            when (s' <= 0) $ do
                writeChan dChan $ T.pack (show n) 
                                <> " packets successfully written to " 
                                <> T.pack fPath
                lift . modify $ const Nothing
        Nothing -> return ()
    yield nextMsg 

-- Debug. takes a string and ugly prints that string prepended to a packet. Used for testing, will replace with a "prettyPrint" machine once that is implemented.
------
debug :: T.Text -> DisplayChan -> PacketMachine
debug txt chan = repeatedly $ do 
    nextMsg <- await
    liftIO $ atomically $ writeTChan chan $ txt <> (T.pack $ show nextMsg)
    yield nextMsg 


-- Counter. Counts n packets, reports the time it took to consume that many, displays that time to the user, then resets itself. 
------
counter :: DisplayChan -> Int -> MachineT (StateT (UTCTime,Int) IO) (Is Message) Message
counter chan n = repeatedly $ do
    nextMsg <- await
    lift . modify $ \(x,y) -> (x,y-1)
    (time,count) <- lift get
    when (count == 0) $ do
        doneTime <- liftIO getCurrentTime
        let diff = diffUTCTime doneTime time
        liftIO . atomically . writeTChan chan $ 
            "\nRead " <> T.pack (show n) <> " packets " <> " in: " <> T.pack (show diff)
        lift . modify $ \x -> (doneTime, n)
    yield nextMsg  


------
-- Stash. Right now it doesn't do much except eat up memory! 
------
stash :: TVar (V.Vector Message) -> PacketMachine
stash t =  repeatedly $ do
    !nextMsg <- await
    !_ <- liftIO $ atomically $ modifyTVar' t $  (\ !x ->  V.force $! V.cons nextMsg  x) 
    yield nextMsg
    return () 



report :: T.Text -> DisplayChan -> PacketMachine
report txt chan = repeatedly $ do
    nextMsg <- await
    liftIO $ atomically $ writeTChan chan $ txt
    yield nextMsg 


vorM :: V.Vector (Maybe Bool) -> Bool
vorM vec = case V.mapM id vec of
    Just blah -> or blah
    Nothing   -> False 
    

listener ::  DisplayChan -> MsgSelectorExp -> TVar RequestID -> Int -> TChan ListenRequest -> Double -> (Int -> Double -> Maybe Double) -> PacketMachine -> PacketMachine
listener dChan lFor reqVar lid reqChan timeOut' retFunc myPMach
  = mkListener 
  where mkListener = repeatedly $ do
            (hdr,nextMsg) <- await
            
            --liftIO . atomically . writeTChan dChan $ "prepListen got a message! " <> T.pack (show nextMsg)
            case evalMsgSelExpPlus nextMsg lFor of
                Left err  -> return ()

                Right f -> do  
                    let g = f . snd 
                    myRID <- liftIO $ getID' reqVar
                    t <- liftIO $ getCurrentTime
                    let myLData = ListenData (hdr,nextMsg) g 0 t timeOut' retFunc lid myPMach
                    liftIO . atomically . writeTChan reqChan $ 
                        ListenRequest myRID myLData
                    --liftIO . atomically . writeTChan dChan $ "prepListen sending request w/ id: " <> T.pack (show myRID )
            yield (hdr,nextMsg) 