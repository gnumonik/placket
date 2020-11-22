{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, TypeApplications, ScopedTypeVariables #-}

module CommandParser where

import qualified Data.Text as T
import Text.Megaparsec.Char
import qualified Data.Map.Strict as Map
import Text.Megaparsec
import PrimParsers
import Data.Char (isDigit, isLetter)
import Control.Monad.IO.Class
import Data.Char 
import Control.Concurrent.STM
import ARPCache (prettyIPTable)
import Control.Monad (void)
import MachineParser 
import SourceManager 
import Control.Concurrent.Async (uninterruptibleCancel, Async, async)
import Data.Machine (echo, plug, runT, (~>), runT_)
import Control.Monad.Trans.Reader
import Control.Lens
import FactoryTypes
import Control.Monad.Extra (when)
import MyReaderT
import RecordFuncs (formatList)
import Data.Maybe
import SourceParser 
import PacketSources (reduceSource)
import UtilityMachines (writeChan)
import qualified Data.Text.IO as TIO 
import Control.Monad.State.Strict (execStateT)
import Data.List (foldl')
import Control.Monad
import qualified Control.Exception as E 
import Data.Hashable 
import Data.Time.Clock.System 
import Data.Word (Word16, Word32)
import Numeric 


type Command = MyReader () 

commands :: Parser Command
commands = lexeme $ try $ do
    first <- lookAhead $ some (satisfy $ \x -> isDigit x || isLetter x)
    case first of
        "exit"           -> exit
        "save"           -> saveFile
        "load"           -> loadFile
        "showStats"      -> showStats
        "showArpCache"   -> showArpTables
        "showMachines"   -> showMachines
        "showSources"    -> showSources
        "debugServer"    -> debugServer
        "stop"           -> stopMachines
        "stopAll"        -> stopAll
        "run"            -> run'
        "clearMachines"  -> clearMachines
        "clearSources"   -> clearSources
        "deleteSource"   -> deleteSource
        "deleteMachine"  -> deleteMachine
        _                -> fail $ "Error: " <> first <> " is not a valid packet string, or perhaps you forgot to preface a machine definition with m:, or a source definition with s:"


exit :: Parser Command
exit = lexeme $ try $ do
    void . lexeme $ string "exit"
    return $! do
        e <- ask
        liftIO . atomically . modifyTVar' e $ over cont $ const False 


saveFile :: Parser Command
saveFile = lexeme $ try $ do
    void . lexeme $ string "save"
    void . lexeme $ string "mode="
    mode <- writeMode
    fPath <- filePath
    return $ do
        d <- askForDisplayChan
        dat <- map ((\x -> "{ m: " <> x <> " }\n") . view schema . snd ) 
             . Map.toList <$> askForMachineData
        sdat <- map ((\x -> "{ s: " <> x <> "} \n") . view srcSchema . snd) 
             . Map.toList <$> askForSourceIDs 
        let defs = dat <> sdat 
        case mode of
            Write -> do
                (a :: Either IOError ()) <- liftIO $ E.try (TIO.writeFile fPath "")
                (b :: Either IOError ()) <- liftIO $ 
                                          E.try (mapM_ (TIO.appendFile fPath) defs)
                case sequence [a,b] of
                    Right _ -> writeChan d $ "Success! Saved current definitions to" 
                                          <> (T.pack fPath )
                    Left err -> writeChan d . T.pack . show $ err 
            Append -> do
                liftIO $ mapM_ (TIO.appendFile fPath) defs


loadFile :: Parser Command
loadFile = lexeme $ try $ do
    void . lexeme $ string "load"
    fPath <- filePath
    return $ do
        d <- askForDisplayChan
        file <- liftIO $ E.try (TIO.readFile fPath)
        case file of
            Left (err :: IOError) -> writeChan d . ("Error! " <>) . T.pack . show $ err 
            Right myFile -> do
                let unlined = T.filter (/= '\n') myFile 
                case parseLex parseDefs unlined of
                    Left err -> do
                        writeChan d $ "Error! Unable to parse file " 
                                    <> (T.pack fPath)
                                    <> "\nParser error info: " 
                                    <>  err 
                    Right function -> do
                        function


showStats :: Parser Command
showStats = lexeme $ try $ do
    void . lexeme $ string "showStats"
    return $ do
        dat <- askForMachineData
        myStats        <- liftIO 
                        . mapM (\x -> readTVarIO $ x ^. pktCountIn) 
                        . map snd $ Map.toList dat 
        let myMachines = Map.foldr (\x acc -> (mchName $ x ^. machineNm) : acc ) [] dat
        let pretty = T.concat $ foldr 
                  (\(a,b) acc -> (a <> ": " <> (T.pack . show $ b) <> "\n") : acc ) [] 
                  $ zip myMachines myStats 
        d <- askForDisplayChan
        writeChan d pretty 


showArpTables :: Parser Command
showArpTables = lexeme $ try $ do
    void . lexeme $ string "showArpCache"
    return $! do
        dChan <- askForDisplayChan
        q <- askForARPCache
        c <- prettyIPTable <$> (liftIO $ readTVarIO q)
        liftIO . atomically . writeTChan dChan $ c 


showMachines :: Parser Command
showMachines = lexeme $ try $ do
    void . lexeme $ string "showMachines"
    return $! do
        ms <- map snd . Map.toList <$> askForMachineData
        let pretty = foldr (\x acc -> x ^. schema
                                 : acc) [] ms
        chan <- askForDisplayChan 
        let myMachineIDs = formatList "\nAvailable machines" 
                $ pretty
        liftIO $ atomically $ writeTChan chan myMachineIDs


showSources :: Parser Command
showSources = lexeme $ try $ do
    void . lexeme $ string "showSources"
    return $! do
        ss <- askForSourceIDs
        let pretty = foldr (\x acc -> fst x <> " :: " <> (snd x) ^. srcSchema : acc) [] $ Map.toList ss
        chan <- askForDisplayChan  
        let mySourceIDs = formatList "\nAvailable sources: " $ pretty
        liftIO . atomically . writeTChan chan $ mySourceIDs


--- Fix this to interact w/ the packet server and integrate the packet server into useragent
run' :: Parser Command
run'  = lexeme $ try $ do
    void $ lexeme $ string "run:"
    pSrc' <- sources 
    void $ lexeme (string ">>")
    machine <- some anySingle
    return $! do
        pSrc <- pSrc'
        dChan <- askForDisplayChan
        case pSrc of 
            Left err -> do 

                liftIO . atomically . writeTChan dChan $ err
            Right src -> do
                t <- askForTagCount
                newName <- mkRandomName t 
                let mySchema = newName <> " = "  <> (T.pack machine)
                machineBuilder mySchema 
                mach <- getMachineDataByName (MachineName $ newName)

                case mach of
                    Right mach' -> do
                        mData@(MachineData _ b _ _ _ _ _ _)  <- pure mach'
                        tag <- getMachineTag b 
                        when (isJust tag) $ do 

                            theSource@(Plugged _ sourceQs)  <- reduceSource src

                            theMachine <- mkFactory mData theSource 
                            when (isJust theMachine ) $ do                
                                async1 <- liftIO . async $! runT_ $ fromJust theMachine

                                sq <- askForServerQueue

                                liftIO . atomically . writeTBQueue sq $ GIMMEPACKETS (fromJust tag) sourceQs

                                writeChan dChan  $! 
                                    "Success! Activated machine: " <> newName 
                                
                                activateMachine (fromJust tag) async1 
                                

                    Left err -> writeChan dChan err 

activateMachine :: Int -> (Async ()) -> MyReader ()
activateMachine n asyn = do
    e <- ask
    liftIO . atomically . modifyTVar' e 
        $ over packetMachines 
            $ Map.adjust (f asyn) n
  where
      f :: (Async ()) -> MachineData -> MachineData
      f x m = over thread (const $ Just x) $ over isActive (const True) m

debugServer :: Parser Command
debugServer = lexeme $ try $ do
    void . lexeme $ string "debugServer"
    return $! do
        s <- askForServerQueue
        liftIO . atomically . writeTBQueue s $ SHOWACTIVE 

stopMachines :: Parser Command
stopMachines = lexeme $ try $ do
    void $ lexeme $ string "stop"
    machIDs <- some machineName 
    return $! do
        d <- askForDisplayChan
        
        myIDs <- sequence <$> mapM getMachineDataByName machIDs
        case  fmap (filter $ \x -> x ^. isActive) myIDs of
            Right toKill -> do 
                mapM_ kill toKill
                writeChan d $ "Kill signal sent to machine(s): "  
                           <> (T.concat $ map (\x -> mchName x <> " ") machIDs )

            Left err -> do 
                writeChan d err

stopAll :: Parser Command
stopAll = lexeme $ try $ do
    void . lexeme $ string "stopAll"
    return $! do
        d <- askForDisplayChan

        dat <- askForMachineData 

        mapM_ kill dat

        let killed = foldr (\x acc -> x ^. machineNm : acc) [] dat

        writeChan d $ "Kill signal sent to machines: " <> (T.concat $ map (\x -> mchName x <> " ") killed) 

kill :: MachineData -> MyReader ()
kill myData = do
    MachineData _ nm q _ cnt _ a mq <- pure myData
    case a of
        Just a' -> liftIO $ uninterruptibleCancel a'
        Nothing -> return ()

    mt <- getMachineTag nm

    s <- askForServerQueue

    case mt of
        Just t' -> do 
            e <- ask  
            liftIO . atomically . writeTBQueue s $ NOMOREPACKETS t'
            mapM_ (\x -> liftIO . atomically $ flushTBQueue x) mq 
            newQ <- liftIO $ newTBQueueIO 20000 
            liftIO $ atomically . modifyTVar' e $ (kill' t') 
            
        Nothing -> return () 
   where                
    kill' :: Int ->  Environment -> Environment
    kill' t e = 
        let f = \x -> set isActive False $ set thread Nothing  x
        in over packetMachines (Map.adjust f t) e


-- utility parsers
-- Some of this stuff should really be in another module, but here is the best place for avoiding  cyclinc imports. Will fix later. 

mkRandomName :: Int -> MyReader (T.Text)
mkRandomName n = do
    MkSystemTime _ w <- liftIO getSystemTime
    let mahHash = fromIntegral (hashWithSalt (fromIntegral w) n) :: Word16
    let mahHex = T.pack $ showHex mahHash $ ""
    return mahHex 

all' :: Parser T.Text
all' = lexeme $ try $ do
    myStr <- lexeme $ string "all"
    return $!  myStr

list :: Parser a -> Parser [a]
list p = lexeme $ try $ do
    parsed <- p
    return $! [parsed]

unbracket :: Parser a -> Parser a
unbracket p = lexeme $ try $ do
    option () space
    void . lexeme $ char '{'
    parsed <- p
    void . lexeme $ char '}'
    return parsed

clearSources :: Parser Command
clearSources = lexeme $ try $ do
    void . lexeme $ string "clearSources"
    return $ do
        e <- ask
        liftIO . atomically . modifyTVar' e $ set sourceIDs Map.empty 
        d <- askForDisplayChan
        writeChan d $ "Successfully erased all sources."

deleteSource :: Parser Command
deleteSource = lexeme $ try $ do
    void . lexeme $ string "deleteSource"
    s <- lexeme $ some $ satisfy (\x -> isLetter x || isDigit x)
    return $ do
        sids <- askForSourceIDs
        case Map.lookup (T.pack s) sids of
            Just x -> do
                e <- ask
                liftIO . atomically . modifyTVar' e $ over sourceIDs (Map.delete (T.pack s))
                d <- askForDisplayChan
                writeChan d $ "Successfully deleted source " <> (T.pack s)
            Nothing -> do
                d <- askForDisplayChan
                writeChan d $ "Error: Could not delete source " <> (T.pack s) <> " because it does not exist"
                 
deleteMachine :: Parser Command
deleteMachine = lexeme $ try $ do
    void . lexeme $ string "deleteMachine"   
    s <- machineName
    return $ do
        mData <- getMachineDataByName s 
        case mData of
            Left _ -> do
                d <- askForDisplayChan
                writeChan d $ "Error: Could not delete machine " <> mchName s <> "because it does not exist"
            Right mData -> do
                e <- ask
                t <- getMachineTag s
                d <- askForDisplayChan
                case t of
                    Just t' -> do  
                        kill mData
                        liftIO . atomically . modifyTVar' e $ over packetMachines (Map.delete t' )
                        writeChan d $ "Successfully stopped and deleted machine " <> (mchName s)
                    Nothing -> do
                        d <- askForDisplayChan
                        writeChan d $ "FATAL ERROR: MACHINE DATA EXISTS FOR " <> mchName s <> " BUT IT HAS NO INTEGER ID. EXITING PROGRAM."
                        e <- ask
                        liftIO . atomically . modifyTVar' e $ over cont (\x -> False)

clearMachines :: Parser Command
clearMachines = lexeme $ try $ do
    void . lexeme $ string "clearMachines"
    return $ do
        dat <- (map snd . Map.toList) <$> askForMachineData
        mapM_ kill dat
        e <- ask
        d <- askForDisplayChan
        liftIO . atomically . modifyTVar' e $ over packetMachines (\x -> Map.empty)
        writeChan d $ "Successfully deleted all machines. "



parseDefs :: Parser (MyReader ())
parseDefs = lexeme $ try $ do 
    option () space 
    def <- defs
    let p = sortInput
    case mapM (parseLex p) def of
        Left err -> return $ do
            d <- askForDisplayChan
            writeChan d $ err
        Right sorted ->  do
            let mchDefs = foldr (\x acc-> case x of
                    Command y -> acc 
                    MachineDef y -> y : acc
                    SourceDef y  -> acc ) [] sorted
            let srcDefs = foldr (\x acc-> case x of
                    Command y -> acc 
                    MachineDef y -> acc
                    SourceDef y  -> y : acc ) [] sorted
            return $ do
                d <- askForDisplayChan 
                mapM_ (\x -> writeChan d $ (x <> "\n"))    mchDefs 
                mapM_ machineBuilder mchDefs 
                mapM_ sourceBuilder srcDefs

sortInput :: Parser UserInput
sortInput = sourceDef <|> machineDef <|> userCommand 
   where 
    sourceDef :: Parser UserInput
    sourceDef = lexeme $ try $ do
        sourceString
        rest <- lexeme $ some anySingle
        return $! SourceDef . T.pack $ rest
       where
           sourceString :: Parser ()
           sourceString = lexeme $ try $ do
               option () space
               _ <- (lexeme . try $ string "source") <|> (lexeme $ string "s:")
               return ()

    machineDef :: Parser UserInput
    machineDef = lexeme $ try $ do
        void $ (lexeme . try $ string "machine:") <|> (lexeme $ string "m:")
        rest <- lexeme $ some anySingle
        return $! MachineDef . T.pack $ rest

    userCommand :: Parser UserInput
    userCommand = lexeme $ try $ do
        rest <- lexeme $ some anySingle
        return $! Command . T.pack $ rest 

data NamedSource = NamedSource T.Text PacketSource

defs :: Parser [T.Text]
defs = manyTill go eof  
    where 
        go = lexeme $ try $ do
            void . lexeme $ char '{'
            t <- some $ satisfy (/= '}')
            eof <|> (void $ many (satisfy (/= '{')) )
            return . T.pack $ t 

splitEq :: Parser (T.Text, T.Text)
splitEq = lexeme $ try $ do
    first <- lexeme $ some (satisfy $ \x -> isLetter x || isDigit x || x == '_')
    void . lexeme $ char '='
    rest <- lexeme $ some anySingle
    return $! (T.pack first, T.pack rest)

sourceBuilder :: T.Text -> MyReader ()
sourceBuilder txt 
    = case parseLex splitEq txt of
        Left _ -> do
            d <- askForDisplayChan
            writeChan d $ "Error. Looks liked you tried to define a "
                        <> "source, but forgot the '='"
        Right (nm,srcTxt) -> do
            sIDs <- askForSourceIDs
            unless (isJust $ Map.lookup nm sIDs) $ do 
                case parseLex sources srcTxt of
                    Left err -> do
                        d <- askForDisplayChan
                        writeChan d err
                    Right mySrc -> do
                        mySrc' <- mySrc 
                        case mySrc' of
                            Right mySrc' -> do 
                                e <- ask
                                let mySData = SourceData mySrc' txt
                                liftIO . atomically . modifyTVar' e $
                                    \x -> over sourceIDs (\m -> Map.insert nm mySData m) x
                                d <- askForDisplayChan
                                writeChan d $ "Success! Source " <> nm <> " has been successfully compiled."
                            Left err -> do
                                d <- askForDisplayChan
                                writeChan d err  
            when (isJust $ Map.lookup nm sIDs) $ do
                d <- askForDisplayChan
                writeChan d $ "Error! A source named " <> nm <> " already exists!" 
            
machineBuilder :: T.Text -> MyReader ()
machineBuilder inputString =
    case parseLex factory inputString of
        Left err -> do
            d <- askForDisplayChan
            liftIO $ atomically $ writeTChan d $ 
                "Error: Invalid expression in input string: \n\"" 
                <> inputString
                <> "\"\n Parser error information: \n" 
                <> err
        Right (NamedMachine nm mArr) -> do
            e <- askForEnvironment
            d <- askForDisplayChan
            liftIO . atomically . writeTChan d $ "Processing machine: " <> T.pack (show mArr)
            st <- liftIO $ execStateT (makeMachines mArr) (MyParserState (Right echo) [] e) 
            case st ^. machineAcc of
                Left errs -> do
                    d <- askForDisplayChan
                    liftIO $ atomically $ writeTChan d $
                        "Error: Unable to parse expression as a valid PacketMachine. "
                        <> "\nError info: \n" <> errs
                Right aMachine ->  do 
                            let fs = st ^. funcsOnSuccess
                            e' <- ask 
                            tag <- askForTagCount 
                            alreadyExists <- getMachineByName nm
                            case alreadyExists of 
                                Left _ -> do

                                    newCommQ <- liftIO $ newTBQueueIO 1000

                                    newCnt   <- liftIO $ newTVarIO 0

                                    let mchDat = MachineData 
                                                    aMachine
                                                    nm
                                                    newCommQ
                                                    False
                                                    newCnt
                                                    inputString
                                                    Nothing
                                                    [] 

                                    liftIO . atomically . modifyTVar' e' $ 
                                        over packetMachines $ Map.insert tag mchDat 

                                    liftIO $ atomically $ 
                                        modifyTVar' e' (updateEnv fs)

                                    liftIO . atomically . writeTChan d $ "Success! Machine " <> mchName nm <> " has been successfully compiled." 

                                Right _ ->  do
                                    liftIO . atomically . writeTChan d $ 
                                        "Error: Machine with name " <> mchName nm <> "already exists!"
   where
       updateEnv :: [Environment -> Environment] -> Environment -> Environment
       updateEnv fs en = foldl' (\x f-> f x) en fs 