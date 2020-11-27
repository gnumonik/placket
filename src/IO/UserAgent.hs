{-# LANGUAGE BangPatterns #-}
module UserAgent where



import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async 
import Control.Concurrent.STM
import Control.Monad.Trans.State.Strict 
import Data.Machine
import ARPCache 
import Control.Concurrent.STM.TMVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader 
import qualified Data.Map.Strict as Map 
import qualified Data.Text as T 
import ListenServer 
import PacketIO 
import MachineController 
import System.Console.Haskeline
import PacketServer
import FactoryTypes
import System.Random.Mersenne.Pure64
import UtilityMachines (packetSender)
import Control.Lens (over)



initializeApp :: IO () 
initializeApp = do
-- Initialize the ReaderT Environment
    let tCount = 0

    let pktMachines  = Map.empty

    let pktSources   = Map.empty

    let pktFactories = Map.empty 

    pcLock <- newTMVarIO ()

    sChan  <- newTChanIO 

    dChan  <- newTChanIO

    serverQ <- newTBQueueIO 50000

    let stashMap = Map.empty

    let eLog = []

    let dumpFilePaths = Map.empty

    let readFilepaths = Map.empty  

    lReqChan <- newTChanIO

    lID <- newTVarIO 0

    cacheVar <- newTVarIO Map.empty 
  
    rID <- newTVarIO 0

    seed <- newPureMT 

    devs <- getDevices 

-- Initialize PCAP (NOTE: Switch to bounded channels so packets don't accrete forever)
    pcapHandle <- initPCAP --initOffline --initPCAP

    let initEnv = Environment  
                  devs 
                  tCount
                  pktFactories 
                  pktMachines 
                  pktSources 
                  pcLock
                  pcapHandle
                  serverQ
                  sChan
                  dChan
                  stashMap
                  eLog
                  lReqChan
                  lID
                  rID
                  cacheVar
                  dumpFilePaths
                  readFilepaths
                  seed
                  True


    env <- newTVarIO initEnv

-- Initialize the user input/output workers 
    userChannel <- newTChanIO 

    runIO <- async $  runInputT defaultSettings (userIO userChannel dChan)

-- Initializer the machine controller (which processes user input)
    let controller = machineController userChannel

    runController <- async $  runReaderT controller env



    let server = packetServer dChan pcapHandle pcLock serverQ

    _ <- async $ evalStateT server Map.empty

    listenQueue <- newTBQueueIO 200000

    let listenServer' = listenServer dChan sChan lReqChan listenQueue

    _ <- async $ evalStateT listenServer' Map.empty

    atomically $ writeTBQueue serverQ $ GIMMEPACKETS (-1) [listenQueue]

    arpMsgQ <- newTBQueueIO 100000

    let arpCacher = arpServer arpMsgQ dChan

    _ <- async $ evalStateT arpCacher cacheVar

    atomically $ writeTBQueue serverQ $ GIMMEPACKETS (-2) [arpMsgQ]

    _ <- async $ packetSender dChan pcLock sChan pcapHandle 

    _ <- mapM_ wait [runController]


    return ()  

 
userIO :: UserInputChan -> DisplayChan ->  InputT IO ()
userIO uchan dchan = do
    f <- getExternalPrint
    void $  liftIO . forkIO $ printer dchan f
    userAgent uchan -- testAgent uchan -- userAgent uchan 


userAgent :: UserInputChan -> InputT IO () 
userAgent chan = forever $ do
    input <- getInputLine "> "
    case input of
        Just i -> do 
            liftIO $ atomically $ writeTChan chan $ T.pack i
        Nothing -> return ()
    liftIO $ threadDelay 1000
{--
testAgent :: UserInputChan -> InputT IO () 
testAgent chan = forever $ do
    input <- getInputLine "> "
    case input of
        Just _ -> do 
            liftIO $ threadDelay 50000
            liftIO $ atomically $ writeTChan chan $ T.pack cmd1
            liftIO $ threadDelay 20000
            liftIO $ atomically $ writeTChan chan $ T.pack cmd2
            liftIO $ threadDelay 20000
            liftIO $ atomically $ writeTChan chan $ T.pack cmd3
        Nothing -> return () 

   -- liftIO $ threadDelay 1000
   where
       cmd1 = "s: testRand = genRandoms 10 wait=0 repeat=10000 [CONTENT ; IP4 ; ETH]"
       cmda = "m: cnt = countSwitch 100 (void :|) (report \"Counted 100 Packets\" :|) ~> prettyPrint default :| "
       cmd2 = "m: cnt = set IP4 (dst=255.255.255.255 src=192.168.0.151) ~> set ETH (dst=ff:ff:ff:ff:ff:ff etherType=2048) ~> send ~> count 50000 :|"
       cmd3 = "run: testRand >> cnt"
--}

printer :: DisplayChan -> (String -> IO ()) -> IO ()
printer chan f = forever $ do
    toPrint <- atomically $ readTChan chan 
    f $ "\n" <> T.unpack toPrint <> "\n" 
    threadDelay 2000



                
