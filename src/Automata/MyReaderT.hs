{-# LANGUAGE OverloadedStrings #-}

module MyReaderT where

import FactoryTypes
import Control.Monad 
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Lens 
import Control.Concurrent.STM 
import  qualified Data.Text as T 
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Machine (MachineT, SourceT)
import Data.Machine.Source
import System.Random.Mersenne.Pure64
import Control.Concurrent.Async (Async)


updateTagCount :: MyReader ()
updateTagCount = do
    e <- ask
    liftIO . atomically . modifyTVar' e $ \x -> over tagCount (+1) x


askForMachineData :: MyReader (Map Int MachineData)
askForMachineData = do 
    e <- ask 
    liftIO  $ view packetMachines <$>readTVarIO e 


askForActiveMachines :: MyReader [MachineName]
askForActiveMachines = do
    dat <- askForMachineData
    let dat' = map snd . Map.toList $ dat
    return $! foldr (\x y -> if x ^. isActive then x ^. machineNm : y else y) [] dat'  


askForServerQueue :: MyReader (TBQueue ToServer)
askForServerQueue = do
    e <- ask
    myServerQueue <- liftIO $ view serverQueue <$> readTVarIO e
    return myServerQueue 

askForSourceIDs :: MyReader (Map T.Text SourceData)
askForSourceIDs = do
    e <- ask
    mySourceIDs <- liftIO $ view sourceIDs <$> readTVarIO e 
    return mySourceIDs

askForEnvironment :: MyReader Environment
askForEnvironment = do
    e <- ask
    myEnv <- liftIO . readTVarIO $ e
    return myEnv 

askForTagCount :: MyReader TagCount
askForTagCount = do
    e <- ask
    myTagCount <- liftIO $  view tagCount <$> readTVarIO e
    liftIO $ atomically . modifyTVar' e $ over tagCount (+1) 
    return myTagCount

askForMachineIDs:: MyReader [(Int,MachineName)]
askForMachineIDs = do
    e <- ask
    dat <- liftIO $  view packetMachines <$> readTVarIO e
    return $ foldr (\(a,b) acc -> (a,b ^. machineNm) : acc) [] (Map.toList dat)


askForPcapLock:: MyReader (TMVar ())
askForPcapLock = do
    e <- ask
    myPcapLock <- liftIO $  view pcapLock <$> readTVarIO e
    return myPcapLock

askForSendChan :: MyReader SendChan
askForSendChan = do
    e <- ask
    mySendChan <- liftIO $  view sendChan <$> readTVarIO e
    return mySendChan

askForDisplayChan :: MyReader DisplayChan
askForDisplayChan = do
    e <- ask
    myDisplayChan <- liftIO $  view displayChan <$> readTVarIO e
    return myDisplayChan

askForARPCache :: MyReader (TVar ARPCache)
askForARPCache = do
    e <- ask
    cache <- liftIO $ view arpCache<$> readTVarIO e
    return $! cache 

askForErrorLog :: MyReader ErrorLog
askForErrorLog  = do
    e <- ask
    myErrorLog <- liftIO $ view errorLog <$> readTVarIO e
    return myErrorLog


getMachineTag :: MachineName -> MyReader (Maybe Int)
getMachineTag nm = do
    dat <- askForMachineData
    let mTag = foldr (\(a,b) y -> if b ^. machineNm == nm then Just a else y) Nothing $ Map.toList dat
    return mTag  

getMachineDataByName :: MachineName -> MyReader (Either T.Text MachineData)
getMachineDataByName nm = do
    t <- getMachineTag nm
    dat <- askForMachineData 
    let t' = t >>= \tag -> Map.lookup tag dat
    case t' of
        Just myData -> return . Right $ myData
        Nothing     -> 
            return . Left $ "Error! No machine data for machine named " <> (mchName nm)

getMachineByName :: MachineName -> MyReader (Either T.Text PacketMachine)
getMachineByName nm = do
    dat <- askForMachineData
    let mch = 
         foldr (\(_,b) y -> if b ^. machineNm == nm then Right (b ^. packetMch) else y) (Left $ "\nError: No machine named " <> (mchName nm) <> " exists.\n") $ Map.toList dat
    return mch 

getSourceByName :: T.Text -> MyReader (Either T.Text PacketSrc)
getSourceByName txt = do
    sIDs <- askForSourceIDs
    let mySourceID = Map.lookup txt sIDs
    case mySourceID of
        Nothing -> return . Left  $  "\nError: No source named " <> txt <> " exists.\n"
        Just s  -> return . Right $ s ^. pktSrc
{--
lookupMachine :: T.Text -> Environment -> Maybe PacketMachine
lookupMachine mName myMap = Map.lookup mName (view machineIDs myMap)  
--}
-- Error Handling

writeError :: ProgramError -> MyReader ()
writeError pErr = do
    e <- ask
    liftIO . atomically . modifyTVar' e $ over errorLog (pErr:)

reportError :: T.Text -> MyReader ()
reportError str = do
    dchan <- askForDisplayChan
    liftIO . atomically $ writeTChan dchan str 

atomicPutStr :: TMVar () -> T.Text -> IO ()
atomicPutStr lock str = do
    atomically $! takeTMVar lock
    putStr $! T.unpack str
    atomically $! putTMVar lock ()

askForSeed :: ReaderT (TVar Environment) IO (PureMT)
askForSeed = do
    e <- ask
    liftIO $ view randSeed <$> readTVarIO e


askForContinue :: ReaderT (TVar Environment) IO (Bool)
askForContinue = do
    e <- ask
    liftIO $ view cont <$> readTVarIO e 
    
    