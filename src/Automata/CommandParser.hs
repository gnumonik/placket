{-# LANGUAGE LambdaCase #-}
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
import PacketSources (capSource, reduceSource)
import UtilityMachines (writeChan)
import qualified Data.Text.IO as TIO 
import Control.Monad.State.Strict (execStateT)
import Data.List (foldl')
import Control.Monad
import qualified Control.Exception as E 
import Data.Hashable 
import PacketIO 
import ArgumentParsers 
import Data.Time.Clock 
import Data.Time.Clock.System 
import PrettyPrint 
import Data.Word (Word8, Word16, Word32)
import Numeric 
import Control.Concurrent (threadDelay)
import Data.Either (isRight)

tShow :: forall x. Show x => x -> T.Text 
tShow = T.pack . show 

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
        "stop"           -> stopFactory
        "stopAll"        -> stopAll
        "start"          -> startF 
        "run"            -> runF
        "kill"           -> killFactory
        "killAll"        -> killAll
        "clearMachines"  -> clearMachines
        "clearSources"   -> clearSources
        "deleteSource"   -> deleteSource
        "deleteMachine"  -> deleteMachine
        "deleteFactory"  -> deleteF
        "clearFactories" -> clearFactories
        "showFactories"  -> showFactories 
        "deviceInfo"     -> deviceInfo
        _                -> fail $ "Error: " <> first <> " is not a valid packet string, or perhaps you forgot to preface a machine definition with m:, or a source definition with s:"

deviceInfo :: Parser Command 
deviceInfo = lexeme $ try $ do
  void . lexeme $ string "deviceInfo"
  return $ do
    liftIO devinfo >>= display  

exit :: Parser Command
exit = lexeme $ try $ do
    void . lexeme $ string "exit"
    return $! do
        e <- ask
        liftIO . atomically . modifyTVar' e $ over cont $ const False 


saveFile :: Parser Command
saveFile = lexeme $ try $ do
    void . lexeme $ string "save"
    mode <- prefix "mode=" (Just Append) writeMode
    fPath <- prefix "path=" Nothing filePath
    return $ do
        d <- askForDisplayChan
        dat <- map ((\x -> "{ m: " <> x <> " }\n") . rebuildSchema . view schema . snd ) 
             . Map.toList <$> askForMachineData
        sdat <- map ((\x -> "{ s: " <> x <> "} \n") . rebuildSchema . view srcSchema . snd) 
             . Map.toList <$> askForSourceData
        fdat <-  map (formatFactory . snd) 
              .  Map.toList 
             <$> askForFactories  
        let defs = dat <> sdat <> fdat
        case mode of
            Write -> do
                (a :: Either IOError ()) <- liftIO . E.try $ TIO.writeFile fPath ""

                (b :: Either IOError ()) <- liftIO  
                                            . E.try
                                            $ mapM_ (TIO.appendFile fPath) defs
                case sequence [a,b] of
                    Right _ -> display $ "Success! Saved current definitions to " 
                                       <> T.pack fPath 
                    Left err -> display . T.pack . show $ err 
            Append -> do
                (b :: Either IOError ()) <- liftIO  
                              . E.try
                              $ mapM_ (TIO.appendFile fPath) defs
                case b of
                    Right _ -> display $ "Success! Saved current definitions to " 
                                       <> T.pack fPath 
                    Left err -> display . T.pack . show $ err 

   where
     formatFactory :: Factory -> T.Text
     formatFactory myFac = "{ f: " 
                          <> myFac ^. facName 
                          <> " = "
                          <> myFac ^. (srcData . srcSchema . _2)
                          <> " >> "
                          <> myFac ^. (mchData . schema . _2)
                          <> " }\n"


loadFile :: Parser Command
loadFile = lexeme $ try $ do
    void . lexeme $ string "load"
    fPath <- filePath
    return $ do
        d <- askForDisplayChan
        file <- liftIO $ E.try (TIO.readFile fPath)
        case file of
            Left (err :: IOError) -> display . tShow $  err 
            Right myFile -> do
                let unlined = T.filter (/= '\n') myFile 
                case parseLex parseDefs unlined of
                    Left err -> do
                        writeChan d $ "Error! Unable to parse file " 
                                    <> (T.pack fPath)
                                    <> "\nParser error info: " 
                                    <>  err 
                    Right action -> do action


showStats :: Parser Command
showStats = lexeme $ try $ do
    void . lexeme $ string "showStats"
    return $ do
        dat <- Map.toList <$> askForFactories
        myStats <- mapM go dat 
        display $  dashRow 
               <> T.concat (map makeDataRowLJ myStats)
               <> dashRow 
  where
    go :: (Word16,Factory) -> MyReader [T.Text]
    go (w16,fac) = 
      let inVar  = fac ^. pktCountIn
          outVar = fac ^. pktCountOut 
          fID    = w16 
          fName  = fac ^. facName
      in liftIO (readTVarIO inVar) >>= \inCount -> 
            liftIO (readTVarIO outVar) >>= \outCount -> 
              return $ ["Name: " <> fName
                       ,"ID#: "  <> tShow fID 
                       ,"Packets in: " <> tShow inCount
                       ,"Packets out: " <> tShow outCount ]

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
        ms <- askForMachineData
        let pretty = map (\x -> rebuildSchema $ x ^. schema ) $ map snd . Map.toList $ ms
        chan <- askForDisplayChan  
        let mysourceData = formatList "\nAvailable machines: "  pretty
        liftIO . atomically . writeTChan chan $ mysourceData



showSources :: Parser Command
showSources = lexeme $ try $ do
    void . lexeme $ string "showSources"
    return $! do
        ss <- askForSourceData
        let pretty = map (\x -> rebuildSchema $ x ^. srcSchema ) $ map snd . Map.toList $ ss
        chan <- askForDisplayChan  
        let mysourceData = formatList "\nAvailable sources: " $ pretty
        liftIO . atomically . writeTChan chan $ mysourceData

showFactories :: Parser Command 
showFactories = lexeme $ try $ do 
  void . lexeme $ string "showFactories"
  return $ do 
    fs <- askForFactories
    theTime <- liftIO $ getCurrentTime
    display $ T.concat $ map (go theTime) (Map.toList fs)
 where
   go :: UTCTime -> (Word16,Factory) -> T.Text
   go time (fID,fac) 

      =  (makeLabelRow $ ("Name: " <> fac ^. facName))

      <> makeDataRowLJ ["Factory ID: " <> (T.pack $ showHex fID "")]
      <> (makeLabelRowDotted "Source code:")
      <> makeDataRowLJ [fac ^. facName <> " = "
                    <> fac ^. (srcData . srcSchema . _2)
                    <> " >> " <> fac ^. (mchData . schema . _2)]
      <> dotRow
      <> makeDataRowLJ ["Status: "<> if fac ^. isActive then "Active" else "Inactive"]

      <> if fac ^. isActive 
          then case fac ^. startTime of
                Just t -> makeDataRowLJ $ ["Uptime: " <> (tShow $ diffUTCTime time t)]
                Nothing -> ""
          else ""
      <> dashRow



-- factoryBuilder processes a factory definition and loads it into memory. 
------
factoryBuilder :: T.Text -> MyReader () 
factoryBuilder txt =
  except (parseLex splitFactory txt) >>= \case
    Nothing -> return ()
    Just (nmTxt,srcTxt,mchTxt) -> 
      sourceBuilder Anonymous srcTxt >>= \case
        Nothing -> return ()
        Just mySrc ->  machineBuilder Anonymous mchTxt >>= \case
          Nothing -> return ()
          Just myMch -> mkFactoryV2 (Just nmTxt) myMch mySrc >>= \(fID,myFac) ->
            (isJust <$> getFactoryByName (myFac ^. facName)) >>= \exists -> 
              if exists
                then display $ "Error! A factory named " <> (myFac ^. facName) <> " already exists."
                else modifyEnv (over factories $ Map.insert fID myFac) >> 
                    display ("Succesfully parsed factory " <> nmTxt)
 where
    splitFactory :: Parser (T.Text,T.Text,T.Text)
    splitFactory = lexeme $ try $ do
      option () space
      fName <- T.pack <$>  (lexeme $ some (satisfy $ \x -> isLetter x || isDigit x))
      option () space
      void . lexeme $ char '='
      rawSource <- T.pack <$> (lexeme $ manyTill anySingle (lookAhead $ string ">>"))
      void . lexeme $ string ">>"
      rawMachine <- T.pack <$> some anySingle
      return $ (fName,rawSource,rawMachine)



-- Stopping Factories 
------

data StopMode = StopSome | StopAll deriving Eq

stopFactory :: Parser Command
stopFactory = lexeme $ try $ do
  void . lexeme $ string "stop"
  option () space
  facNm <- T.pack <$> lexeme (some (satisfy $ \x -> isLetter x || isDigit x))
  option () space
  return $ do
      stopF StopSome facNm

stopF :: StopMode -> T.Text -> Command 
stopF sMode fName = do 
  maybeF <- getFactoryByName fName 
  case maybeF of
        Nothing -> display $ "Error! No factory named " <> fName <> " exists!"
        Just (fID, toStop) -> runStop sMode fID toStop

runStop :: StopMode -> Word16 -> Factory -> Command 
runStop sMode fID toStop = do 
    if not (toStop ^. isActive)
      then when (sMode == StopSome) $ do display $ "Factory " <> (toStop ^. facName) <> " cannot be stopped because it is not running."
      
      else do
        writeQ (toStop ^. srcQ)  STOP
        writeQ (toStop ^. snkQ)  SINK_STOP
        sq <- askForServerQueue
        writeQ sq $ NOMOREPACKETS . fromIntegral $ fID
        modifyEnv . over factories $ Map.adjust (set isActive False . set startTime Nothing) fID
        display $ "Successfully stopped factory " <> (toStop ^. facName)

stopAll :: Parser Command
stopAll = lexeme $ try $ do
  void . lexeme $ string "stopAll"
  return $ do
    fs <- Map.toList <$> askForFactories
    mapM_ (uncurry $ runStop StopAll) fs


-- Killing factories (kill cancels the thread they are running in, "stop" just cuts off the source and plugs the sink)
------
killFactory :: Parser Command
killFactory = lexeme $ try $ do
  void . lexeme $ string "kill"
  facIDStr <- T.pack <$> lexeme (some (satisfy $ \x -> isLetter x || isDigit x))
  return $! do
    killFac facIDStr 

killFac :: T.Text -> Command 
killFac fName  = do 
  maybeF <- getFactoryByName fName 
  case maybeF of
      Nothing -> display $ "Error! No factory named " <> fName <> " exists!"
      Just (fID, toKill) -> runKill fID toKill 
      
runKill :: Word16 -> Factory -> Command 
runKill fID toKill = do
        writeQ (toKill ^. srcQ)  STOP
        writeQ (toKill ^. snkQ)  SINK_STOP
        sq <- askForServerQueue
        writeQ sq $ NOMOREPACKETS . fromIntegral $ fID
        forM_ (toKill ^. fThread) (liftIO . uninterruptibleCancel)
        modifyEnv . over factories $ Map.adjust (set isActive False . set fThread Nothing) fID
        display $ "Successfully killed factory " <> (toKill ^. facName)

killAll :: Parser Command
killAll = lexeme $ try $ do
  void . lexeme $ string "killAll"
  return $ do
    fs <-  Map.toList <$> askForFactories
    mapM_ (uncurry runKill) fs


-- Deleting/clearing factories (delete targets a single factory, clear wipes all of them from memory)
------

deleteF :: Parser Command
deleteF = lexeme $ try $ do
  void . lexeme $ string "deleteFactory"
  facIDStr <- T.pack <$> lexeme (some (satisfy $ \x -> isLetter x || isDigit x))
  return $ do
    fac <- getFactoryByName facIDStr
    case fac of
      Nothing -> display $ "Error: Cannot delete factory " <> facIDStr <> "because it does not exist"
      Just (fID,_) -> killFac facIDStr >> modifyEnv (over factories $ Map.delete fID )

clearFactories :: Parser Command
clearFactories = lexeme $ try $ do
  void . lexeme $ string "clearFactories" 
  return $ (Map.toList <$> askForFactories) >>= \fs -> 
    mapM_ (uncurry runKill) fs >> (modifyEnv $ set factories Map.empty)


-- Running and starting factories. "Run" is used to start up an unnamed factory, while "start" activates a factory that already exists 
-- but either has not been activated yet, or has been stopped with "stop"
------
data InitMode = AlreadyThere | NeedsInserted deriving (Show, Eq)

-- initFactory is the function that "turns a factory on". The InitMode paramer is used to signal different
-- behavior depending on whether the factory already exists or not.
------ 
initFactory :: InitMode -> Word16 -> Factory -> MyReader () 
initFactory iMode fID myFac = do 
    let toRun  = myFac ^. factory
    myThread  <- liftIO . async $ runT_ toRun 
    theTime   <- liftIO getCurrentTime 
    let activeFac = set fThread (Just myThread) . set startTime (Just theTime) . set isActive True $ myFac 
    when (iMode == NeedsInserted) $ do 
      modifyEnv $ over factories (Map.insert fID activeFac)
    when (iMode == AlreadyThere) $ do
      modifyEnv $ over factories $ Map.adjust 
        (set fThread (Just myThread) . set startTime (Just theTime) . set isActive True) fID
    sq <- askForServerQueue
    liftIO . atomically . writeTBQueue sq $ GIMMEPACKETS (fromIntegral fID) (activeFac ^. fQueues)
    display $ "Successfully activated Factory " 
            <> (("FactoryID: " <>) . T.pack . (\x -> showHex x "") $ fID) 
            <> "Factory Name: " <> (activeFac ^. facName)
            <> ":\n" 
            <>  (activeFac ^. (srcData . srcSchema . _2)) 
            <> " >> " 
            <> (activeFac ^. (mchData . schema . _2))

startF :: Parser Command
startF = lexeme $ try $ do
  void . lexeme $ string "start"
  option () space
  fName <- T.pack <$> lexeme (some (satisfy $ \x -> isLetter x || isDigit x))
  option () space
  return $ do
    maybeFac  <- getFactoryByName fName
    case maybeFac of
      Nothing -> display $ "Cannot start factory " <> fName <> " because it does not exist"
      Just (fID,myFac) -> initFactory AlreadyThere fID myFac 

-- run allows a factory to be assembled and activated without first defining it. 
-- The syntax is: "run: <SOURCE> >> <MACHINE>"
----- 
runF :: Parser Command 
runF = lexeme $ try $ do
    void . lexeme $ string "run:"
    rawSource <- lexeme $ manyTill anySingle (lookAhead $ string ">>")
    void . lexeme $ string ">>"
    rawMachine <- some anySingle 
    return $ do
      mabSrc <- sourceBuilder Anonymous (T.pack rawSource)
      mabMch <- machineBuilder Anonymous (T.pack rawMachine)
      mabFac <- pure mabSrc >>= \case 
                  Nothing -> return Nothing
                  Just aSrc -> pure aSrc >>= \mySrc -> 
                    pure mabMch >>= \case
                      Nothing -> return Nothing
                      Just aMch -> pure aMch >>= \myMch ->
                        Just <$> mkFactoryV2 Nothing myMch mySrc
      case mabFac of
        Nothing -> return () 
        Just (facID,myFac) -> do
          let toRun = myFac ^. factory
          myThread <- liftIO . async $ runT_ toRun 
          theTime <- liftIO getCurrentTime 
          let activeFac = set fThread (Just myThread) . set startTime (Just theTime) . set isActive True $ myFac 
          modifyEnv $ over factories (Map.insert facID activeFac)
          sq <- askForServerQueue
          liftIO . atomically . writeTBQueue sq $ GIMMEPACKETS (fromIntegral facID) (activeFac ^. fQueues)
          display $ "Successfully activated Factory " 
                  <> (T.pack . (\x -> showHex x "") $ facID) 
                  <> ":\n" 
                  <>  (T.pack rawSource) <> " >> " <> (T.pack rawMachine)





-- Utility command. Makes the packet server print a list of the factories that it is sending packets to. Used to determine whether a machine
-- is broken.
------
debugServer :: Parser Command
debugServer = lexeme $ try $ do
    void . lexeme $ string "debugServer"
    return $! do
        s <- askForServerQueue
        liftIO . atomically . writeTBQueue s $ SHOWACTIVE


all' :: Parser T.Text
all' = lexeme $ try $ do
    myStr <- lexeme $ string "all"
    return $!  myStr

list :: Parser a -> Parser [a]
list p = lexeme $ try $ do
    parsed <- p
    return [parsed]

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
        liftIO . atomically . modifyTVar' e $ set sourceData Map.empty 
        d <- askForDisplayChan
        writeChan d $ "Successfully erased all sources."

deleteSource :: Parser Command
deleteSource = lexeme $ try $ do
    void . lexeme $ string "deleteSource"
    s <- lexeme $ some $ satisfy (\x -> isLetter x || isDigit x)
    return $ do
        sids <- askForSourceData
        case Map.lookup (T.pack s) sids of
            Just x -> do
                e <- ask
                liftIO . atomically . modifyTVar' e $ over sourceData (Map.delete (T.pack s))
                d <- askForDisplayChan
                writeChan d $ "Successfully deleted source " <> T.pack s
            Nothing -> do
                d <- askForDisplayChan
                writeChan d $ "Error: Could not delete source " <> T.pack s <> " because it does not exist"
             
deleteMachine :: Parser Command
deleteMachine = lexeme $ try $ do
    void . lexeme $ string "deleteMachine"   
    s <- machineName
    return $ do
        modifyEnv $ over packetMachines (Map.delete s)
        display $ "Successfully deleted machine " <> mchName s 



clearMachines :: Parser Command
clearMachines = lexeme $ try $ do
    void . lexeme $ string "clearMachines"
    return $ do
        modifyEnv $ over packetMachines (const Map.empty)
        display "Successfully deleted all machines. "



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
                    MachineDef y -> y : acc
                    _            -> acc ) [] sorted

            let srcDefs = foldr (\x acc-> case x of
                    SourceDef y  -> y : acc
                    _            -> acc ) [] sorted

            let facDefs = foldr (\x acc -> case x of
                    FactoryDef y -> y : acc
                    _            -> acc) [] sorted 
            return $ do
                d <- askForDisplayChan 
                mapM_ (\x -> writeChan d $ (x <> "\n"))    mchDefs 
                mapM_ (machineBuilder Named) mchDefs 
                mapM_ (sourceBuilder Named) srcDefs
                mapM_ factoryBuilder facDefs 

sortInput :: Parser UserInput
sortInput = sourceDef <|> machineDef <|> factoryDef <|> userCommand 

sourceDef :: Parser UserInput
sourceDef = lexeme $ try $ do
    option () space 
    sourceString
    option () space 
    rest <- lexeme $ some anySingle
    return $! SourceDef . T.pack $ rest
    where
        sourceString :: Parser ()
        sourceString = lexeme $ try $ do
            option () space
            _ <- (lexeme $ string "s:") <|> (lexeme $ string "source:")
            return ()

machineDef :: Parser UserInput
machineDef = lexeme $ try $ do
    void $ (lexeme $ string "m:") <|> (lexeme $ string "machine:")
    rest <- lexeme $ some anySingle
    return $! MachineDef . T.pack $ rest

factoryDef :: Parser UserInput
factoryDef = lexeme $ try $ do
  option () space 
  void $ ((lexeme . try $ string "f:") <|> (lexeme . try $ string "factory:"))
  option () space 
  rest <- lexeme $ some anySingle
  return $! FactoryDef . T.pack $ rest 

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

data BuilderMode = Named | Anonymous deriving (Show, Eq)

sourceBuilder :: BuilderMode -> T.Text -> MyReader (Maybe SourceData) 
sourceBuilder bMode inputString =
    case bMode of
      Named -> except (parseLex splitEq inputString) >>= \case 
          Just (nm,srcTxt) -> go bMode nm srcTxt inputString 
          Nothing -> return Nothing   

      Anonymous -> mkRandomSrcName >>= \sName -> 
        pure (sName <> " = " <> inputString) >>= \newString -> 
           go bMode sName inputString newString
   where
     go :: BuilderMode -> T.Text -> T.Text -> T.Text -> MyReader (Maybe SourceData)
     go b srcName srcDefTxt rawString = do
       sData <- askForSourceData
       let alreadyExists = isJust $ Map.lookup srcName sData
       if alreadyExists
         then (display $ "Error! A source named " <> srcName <> " already exists!") >> return Nothing
         else 
           except (parseLex sources srcDefTxt) >>= \case
              Nothing -> return Nothing 
              Just anAction -> anAction >>= \x -> 
                except x >>= \case
                  Nothing -> return Nothing
                  Just z  -> 
                    let mySrcData =  (SourceData z (srcName,srcDefTxt)) 
                    in (modifyEnv $ over sourceData (Map.insert srcName mySrcData)) 
                        >> (if b == Named 
                              then display  ("Successfully compiled source " <> srcName )
                              else return () )
                        >> return (Just mySrcData)




machineBuilder :: BuilderMode -> T.Text  -> MyReader (Maybe MachineData)
machineBuilder bMode inputString =
    if bMode == Named 
      then do 
        case parseLex namedMachine inputString of
            Left err -> display err >> return Nothing 
            Right nmed -> go bMode nmed $ splitSchema inputString

      else do 
          newName <- mkRandomMchName 
          let newInString = newName <> " = " <> inputString 
          case parseLex namedMachine newInString  of
              Left err -> display err >> return Nothing
              Right nmed -> go bMode nmed $ splitSchema newInString 
   where

        go bMode' (NamedMachine nm arr ) inString = do
              display $ "Processing machine " <> mchName nm
              e <- askForEnvironment
              st <- liftIO $ execStateT (makeMachines arr) (MyParserState (Right echo) [] e)
              let macc = return (st ^. machineAcc) >>= except
              mach <- macc   
              let fs = st ^. funcsOnSuccess
              alreadyExists <- isRight <$> (getMachineDataByName nm)
              if alreadyExists
                then (display $ "Error! A machine with name " <> mchName nm <> " already exists!") 
                     >> return Nothing 
                else case mach of
                  Nothing -> do 
                        display $ "Failed to compile machine " <> mchName nm
                        return Nothing 
                  Just myMachine -> do
                        let myMachineData = MachineData myMachine inString
                        e <- ask 
                        liftIO . atomically . modifyTVar' e $ over packetMachines (Map.insert nm myMachineData)
                        liftIO . atomically . modifyTVar' e $ updateEnv fs
                        when (bMode' == Named) $ display $ "Success! Machine " <> mchName nm <> " has been compiled"
                        return (Just myMachineData) 

        updateEnv :: [Environment -> Environment] -> Environment -> Environment
        updateEnv fs en = foldl' (\x f-> f x) en fs 


trimSpaces :: T.Text -> T.Text
trimSpaces txt = T.dropWhile (== ' ') . T.reverse $ T.dropWhile (== ' ') . T.reverse $ txt 

rebuildSchema :: (T.Text,T.Text) -> T.Text
rebuildSchema (n,d) = n <> " = " <> d


splitSchema :: T.Text -> (T.Text,T.Text)
splitSchema txt = let a = trimSpaces . T.takeWhile (/= '=') $ txt
                      b = trimSpaces . T.drop 1 . T.dropWhile (/= '=') $ txt 
                  in (a,b)
