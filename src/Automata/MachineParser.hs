{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances, OverloadedStrings,  MultiWayIf #-}


module MachineParser where


import           Control.Monad
 
import           Control.Lens.TH 
import  Data.Time.Clock 

import           Data.Either 
import           Data.Machine hiding (zipWith)
import Data.Machine.Lift 
import           Control.Monad.Trans.State.Strict
import           Data.Default       (Default (..))
import qualified Data.Text as T
import           Data.List          (foldl')
import           Data.Proxy
import           Data.Maybe
import           Prelude hiding (until)
import           FactoryTypes
import           FieldClasses
import qualified Data.Map.Strict as Map
import           PacketFilters
import qualified PacketOperations as PO   
import           PrimParsers
import           PrimTypes
import           RecordParsers           
import           RecordTypes 
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Vector as V 
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Machine.Fanout
import ArgumentParsers 
import Data.Char
import OptionalFields
import PacketIO
import RecordFuncs (evalProtoSelectExp, evalMsgSelectorExp, formatList)
import PacketOperations (randomP, randomPs)
import UtilityMachines 
import BuilderMachines
import EffectfulMachines
    ( alert,
      counter,
      debug,
      doIO,
      dumpPkt,
      listener,
      prettyPrint,
      report,
      send,
      stash ) 
import HigherOrderMachines
import SelectorMachines 
import Control.Lens (view, set, (^.), over)





-- The machine parser gets passed a copy of the environment when it is run. It stores successfully parsed machine in _machineAcc (initialize with echo then compose new machine with ~>). 

-- Since the parser can fail and we don't want to modify the "live" environment unless it succeeds, the every machine which requires modifying the environment in some way also returns a function. The local copy of Environment in the state monad is discarded after a successful parse of a chain of machines, and the functions that modify the environment are applied to the live Environment. 
data MyParserState = 
    MyParserState {_machineAcc     :: Either T.Text PacketMachine
                  ,_funcsOnSuccess :: [Environment -> Environment]
                  ,_env            :: Environment}
makeFieldsNoPrefix ''MyParserState

-- Some type synonyms to avoid excessive verbosity.
type ParserMonad = StateT MyParserState IO (Either T.Text PacketMachine)

type ParseOutput = Either T.Text (PacketMachine, Maybe (Environment -> Environment))

-- makeMachines parses machine declarations and returns either an informative (hopfully) error message if the parse fails, or a packetmachine.
-- Note that the PacketMachine that it returns is not the composition of the accumulated machines stored in machineAcc. (I.e. to actually use makeMachines, use execStateT not evalStateT, evalStateT will only yield the last successful machine parsed.)
makeMachines :: MachineArrow T.Text -> ParserMonad
makeMachines mArr = case mArr of
    (x :~> ys) -> do
        mX <- (makeMachine x)
        case mX of
            Left err -> do
                modify $ over machineAcc $ \y -> Left err
                return $! Left err
            Right _ -> makeMachines ys
    (x :~+> ys) -> do
        mX <- makeMachine x
        case mX of
            Left err -> do 
                modify $ over machineAcc $ \x -> Left err
                return $! Left err
            Right _ -> do
                s <- get
                (res,st) <- liftIO $  unzip <$> mapM (\z -> runStateT (makeMachines z) s) ys
                case sequence res of
                    Left _ -> return $! 
                                Left $  foldr (\x y -> x <> "\n" <> y) "" (lefts res)
                    Right _ -> do
                        s' <- get 
                        let machs = foldr (\x y -> (Right (:)) <*> x ^. machineAcc <*> y) (Right []) st
                        let funcs = foldr (\x y -> x ^. funcsOnSuccess <> y) [] st
                        let newState = foldl' (\x f -> f x) (s' ^. env) funcs
                        modify $ set env newState
                        case machs of
                            Left err -> return $! Left err
                            Right mch -> do
                                let fannedout = (fanout $ map (\x -> x ~> semigrouper) mch) ~> flattened 
                                modify $ over machineAcc $ fmap (\x -> x ~> fannedout)
                                modify $ over funcsOnSuccess (\x -> x <> funcs)
                                s' <- get
                                return $ s' ^. machineAcc 
    (x :| ()) -> do
        mX <- makeMachine x
        case mX of 
            Left err -> do
                modify $ over machineAcc $ \y -> Left ("Parse error in: " <> x <> "\n" <> err)
                return $! Left err
            _ -> do
                s <- get
                let myMach = s ^. machineAcc
                return $!  myMach 


makeMachine :: T.Text -> ParserMonad
makeMachine mStr =
    let m = parseLex machines mStr
    in case m of
        Left err -> do
            return $ Left err 
        Right s  -> do
            s' <- s
            case s' of
                Left err -> do
                    modify $ over machineAcc $ \x -> Left err
                    return $! Left err
                Right (mach,func) -> do
                    modify $ over machineAcc $ fmap (\x -> x ~> mach)
                    case func of
                        Just f -> do
                            modify $ over funcsOnSuccess (<> [f])
                            modify $ over env f
                            return $ Right mach
                        Nothing -> return $ Right mach

makeRandom :: Parser (StateT MyParserState IO ParseOutput)
makeRandom = lexeme $ try $ do
    void . lexeme $ string "randomize"
    n <- intOptOne
    b <- builder protocolType
    let myVec = randomP b
    case myVec of
        Left err -> return . return $ Left err
        Right vec -> return $ do
            s <- view (env . randSeed) <$> get
            let (vecs, newSeed) = randomPs n s vec
            return $ Right (make vecs (Just 0) (Just 0), Just $ set randSeed newSeed)



getMachineByNameSTATE :: MachineName -> Environment -> Either T.Text PacketMachine
getMachineByNameSTATE nm en 
   =  foldr (\(b,MachineData pktMch _) y -> 
       if b  == nm 
           then Right pktMch
           else y) 
           (Left $ "\nError: No machine named " <> mchName nm <> " exists.\n") 
           $ Map.toList (en ^. packetMachines)


prettyPrintR :: Parser (StateT MyParserState IO ParseOutput)
prettyPrintR = lexeme $ try $ do
    void . lexeme $ (string "prettyPrint" <|> string "pp")
    m <- prefix "mode=" (Just Dflt) ppMode
    return $ do
        d <- view (env . displayChan) <$> get
        return $! Right (prettyPrint d m,Nothing)

printFieldR :: Parser (StateT MyParserState IO ParseOutput)
printFieldR = lexeme $ try $ do
    void . lexeme $ (string "printField" <|> string "pf")
    l <- prefix "label=" (Just "") quotedString
    m <- prefix "mode=" (Just Dflt) ppMode 
    ptype <- protocolType
    ostr  <- opticStrings
    return $ do
        dchan <- view (env . displayChan) <$> get
        let pfld = PO.apPrintField ptype ostr dchan m l
        case pfld of
            Left err -> return $ Left err
            Right f -> return $ Right (doIO f,  Nothing)

writeFieldR :: Parser (StateT MyParserState IO ParseOutput)
writeFieldR = lexeme $ try $ do
    void . lexeme $ (string "writeField" <|> string "wf")
    fPath <- prefix "path=" Nothing filePath 
    l     <- prefix "label=" (Just "") qsLabelPrefix
    pMode <- prefix "mode=" (Just Dflt) ppMode
    pType <- protocolType
    oStr  <- opticStrings
    return $ do
        dchan <- view (env . displayChan) <$> get
        let wfld = PO.apWritePrim pType oStr dchan fPath pMode l
        case wfld of
            Left err -> return $ Left . T.pack . show $ err
            Right f  -> return . Right $ (doIO f, Nothing)

    

    
-- Utility parser. Parses predicates for a message (i.e. functions :: Message -> Maybe All). Not sure if I still need the monoid constraint to make the record selectors work, possibly can change to a simpler :: Message -> Maybe Bool function later on.
{--
msgPredicate :: Parser (Either T.Text Predicate)
msgPredicate = do
    myPredicate <- protocolSelectorExp 
    return $ case evalProtoSelectExp myPredicate Nothing of
        Right f -> Right $ apFilter (mkCompare f)
        Left errs -> Left $  errs 
--}
-----

popR :: Parser (StateT MyParserState IO ParseOutput)
popR = lexeme $ try $ do
    void $ lexeme $ string "pop"
    pType <- protocolType
    let myPop = pop pType
    case myPop of
        Left err -> return . return  $ Left err
        Right po' -> return . return $ Right (po',Nothing)
----


pullR :: Parser (StateT MyParserState IO ParseOutput)
pullR = lexeme $ try $ do
    void $ lexeme $ string "pull"
    pType <- protocolType
    let myPull =  pull pType
    case myPull of
        Left err    -> return . return $ Left err
        Right pull' -> return . return $ Right (pull',Nothing) 

----

extractR :: Parser (StateT MyParserState IO ParseOutput)
extractR = lexeme $ try $ do
    void $ lexeme $ string "extract"
    pType <- protocolType
    let myExtract = extract  pType
    case myExtract of
        Left err   -> return . return $ Left err
        Right extr -> return . return $ Right (extr,Nothing)

--- 
 
cutR :: Parser (StateT MyParserState IO ParseOutput)
cutR = lexeme $ try $ do
    void $ lexeme $ string "cut"
    pType <- protocolType
    let myCut = cut pType
    case myCut of
        Left err   -> return . return $ Left err
        Right cut' -> return . return $ Right (cut',Nothing)
--

pushR :: Parser (StateT MyParserState IO ParseOutput)
pushR = lexeme $ try $ do
    void . lexeme $ string "push"
    pBuilder <- protocolBuilder
    let p = PO.makeProtocolMessageV2 pBuilder
    case p of
        Left  err  -> return . return $ Left err
        Right pkts -> let push' = push $ V.force pkts
                      in return . return $ Right (push',Nothing) 



liftR :: Parser (StateT MyParserState IO ParseOutput)
liftR = lexeme $ try $ do
    void . lexeme $ string "lift"
    pBuilder <- protocolBuilder
    let p = PO.makeProtocolMessageV2 pBuilder
    case p of
        Left  err  -> return . return $ Left err
        Right pkts -> let lift' = liftMch $ V.force pkts
                      in return . return $ Right (lift',Nothing)  

setR :: Parser (StateT MyParserState IO ParseOutput)
setR = lexeme $ try $ do
    void $ lexeme $ string "set"
    pBuilder <- protocolBuilder
    let mySet = setFields pBuilder
    case mySet of
        Left err    -> return . return $ Left err
        Right setIt -> return . return $ Right (setIt,Nothing)

chkSumR :: Parser (StateT MyParserState IO ParseOutput)
chkSumR = lexeme $ try $ do
    void . lexeme $ string "checksum"
    return . return $ Right (chkSum,Nothing)

-------
-- Optional Field Machine Parsers. These have to be treated separately, since it's not possible to express operations on optional fields in normal record selector syntax (and extending it to do so would make it very ugly.)
------


modifyOptR :: Parser (StateT MyParserState IO ParseOutput)
modifyOptR = lexeme $ try $ do
    void $ lexeme $ (string "modifyOpt" <|> string "mOpt")
    tStr <- protocolType 
    fStr <- lexeme $ some (satisfy (/= ' '))
    void . lexeme $ string "when" 
    fSel <- fieldSelectorExp 
    void . lexeme $ string "changeTo"
    fBld <- fieldBuilder
    let myMod =  fmap liftMachine . join $ withOptionalField tStr (T.pack fStr) $
                    modifyOMatic fSel fBld
    case myMod of
        Left err   -> return . return . Left $ err
        Right mod' -> return . return $ Right (mod',Nothing) 

insertOptR :: Parser (StateT MyParserState IO ParseOutput)
insertOptR = lexeme $ try $ do
    void $ lexeme $ (string "insertOpt" <|> string "iOpt")
    tStr <- protocolType 
    fStr <- lexeme $ some (satisfy (/= ' ')) 
    fBld <- some fieldBuilder
    let myInsert =  fmap liftMachine . join $ withOptionalField tStr (T.pack fStr) $    
                        insertOMatic fBld
    case myInsert of
        Left err  -> return . return $ Left err
        Right ins -> return . return $ Right (ins,Nothing) 

deleteOptR :: Parser (StateT MyParserState IO ParseOutput)
deleteOptR = lexeme $ try $ do
    void $ lexeme $ (string "deleteOpt" <|> string "dOpt")
    tStr <- protocolType 
    fStr <- lexeme $ some (satisfy (/= ' ')) 
    fSel <- fieldSelectorExp
    let myDelete = fmap liftMachine $ deleteOMaticV2 tStr (T.pack fStr) fSel
    case myDelete of
        Left err -> return . return . Left $ err
        Right del -> return . return $ Right (del,Nothing )
------
-- Select and discard. Parse a predicate and return a machine that passes through packets that it is true of (select), or discards a packet that meets the predicate (discard). 
-- The BPF filters provided by libpcap won't work here, since we want to be able to use these operations on packets that have been deserialized or generated in this application.
------

-- experimental 
experimentalSelect :: Parser (StateT MyParserState IO ParseOutput)
experimentalSelect = lexeme $ try $ do
    void . lexeme $ string "expSelect"
    msgSel <- msgSelectorExp
    let myPred = evalMsgSelectorExp Nothing msgSel
    case myPred of
        Right f -> return . return $ Right (select f, Nothing)
        Left err -> return . return $ Left err 

selectR :: Parser (StateT MyParserState IO ParseOutput)
selectR = lexeme $ try $ do
    void $ lexeme $ string "select"
    f <- protoSelectorPredicate 
    let myselect = select <$> f
    case myselect of
        Left err -> return . return $ Left err
        Right sel -> return . return $ Right (sel,Nothing)

discardR :: Parser (StateT MyParserState IO ParseOutput)
discardR = lexeme $ try $ do
    void $ lexeme $ string "discard"
    f <- protoSelectorPredicate
    let myDiscard = discard <$> f
    case myDiscard of 
        Left err  -> return . return $ Left err
        Right dis -> return . return $ Right (dis,Nothing) 

------
-- Parser for the "alert" machine. Alert takes a string (which must be entered between quotation marks) and a message predicate, and displays the string to the user when it receives a packet that satisfies the predicate.
-----
alertR :: Parser (StateT MyParserState IO ParseOutput)
alertR = lexeme $ try $ do
    void $ lexeme $ string "alert"
    alertStr <- lexeme $ quotedString
    pred'    <- protoSelectorPredicate
    case pred' of
        Left str -> return . return $! Left str 
        Right p -> return $! do
            s <- get
            let chan = s ^. (env . displayChan)
            return $! Right $ (alert (Just p) alertStr chan , Nothing)  
-------
-- Parser for the "stash" machine, which caches packets that it receives in memory. At the moment this doesn't do much, need to implement stash operations.
-------

stashR :: Parser (StateT MyParserState IO ParseOutput)
stashR = lexeme $ try $ do
    void $ lexeme $ string "stash"
    stashName <- some (satisfy $ (/= ' '))
    return $! do
        s <- get
        let stashMap = s ^. (env . stashes)
        case Map.lookup (T.pack stashName) stashMap of
            Just t  -> return $! Right $ (stash t,Nothing)
            Nothing -> do
                t <- liftIO $ newTVarIO V.empty
                return $! Right $
                        ( 
                            stash t
                            ,
                            Just $ over stashes (\x -> Map.insert (T.pack stashName) t x)
                        )
------
-- Parser for the void machine. It consumes all packets provided as input, and never outputs anything.
------
voidR :: Parser (StateT MyParserState IO ParseOutput)
voidR = lexeme $ try $ do
    void $ lexeme $ string "void"
    return . return $! Right (blackHole, Nothing)

-- Debug is for testing; it ugly-prints a string representation of packets. Need to replace with a prettyprinter machine once that's up and running.
debugR :: Parser (StateT MyParserState IO ParseOutput)
debugR = lexeme $ try $ do
    void $ lexeme $ string "debug"
    qString <- quotedString 
    return $! do
        s <- get
        let chan = s ^. (env . displayChan)
        return $! Right (debug qString chan, Nothing) 

----
reportR :: Parser (StateT MyParserState IO ParseOutput)
reportR = lexeme $ try $ do
    void . lexeme $ string "report"
    str <- quotedString
    return $! do
        s <- get
        let dChan = s ^. (env . displayChan)
        return $! Right (report str dChan,Nothing)




------
-- Parser for "make". Make is *not* a source, but creates a packet (or set of packets)
-----

createR :: Parser (StateT MyParserState IO ParseOutput)
createR = lexeme $ try $ do
    void $ lexeme $ string "create"
    dly  <- prefix "wait=" (Just 0) int
    rpts <- prefix "repeat=" (Just 0) int
    bld  <- builder protocolBuilder
    let ps = V.force <$> V.mapM PO.makeProtocolMessageV2 bld
    case ps of
            Left str -> return . return  $ Left str
            Right bldr -> do
                let myMach = make bldr (Just rpts) (Just dly)
                return . return $ Right (myMach,Nothing)

maybeInt:: Parser (Maybe Int)
maybeInt = option Nothing go
    where
        go :: Parser (Maybe Int)
        go = lexeme $ try $ do
            n <- some $ satisfy isDigit
            return $! Just (read n :: Int) 


-----
-- Parser for the counter machine. When counter has processed a specified  number of packets, it reports the time that it took to process that number.
----- 
counterR :: Parser (StateT MyParserState IO ParseOutput)
counterR = lexeme $ try $ do
    void $ lexeme $ string "count"
    toCount <- int 
    return $! do
        theTime <- liftIO $ getCurrentTime
        s <- get
        let myChan = s ^. (env . displayChan)
        return $! Right (execStateM (theTime,toCount ) $ counter myChan toCount, Nothing) 
------
-- Parser for the buffer machine. Buffer waits until it has collected n packets, then yields all of them sequentially upon reaching the specified number.
------
bufferR :: Parser (StateT MyParserState IO ParseOutput)
bufferR = lexeme $ try $ do
    void $ lexeme $ string "buffer"
    n <- some $ satisfy isDigit
    return . return $! Right (buffer (read n :: Int), Nothing)

dumpPktR :: Parser (StateT MyParserState IO ParseOutput)
dumpPktR = lexeme $ try $ do
    void . lexeme $ string "dump"
    fPath <- prefix "path=" Nothing filePath
    void . lexeme $ string "numPackets="
    n <- prefix "numPackets=" Nothing int 
    return $! do 
        paths <- view (env . openDumpFiles) <$> get
        case Map.lookup fPath paths of
            Just _ -> return $ Left $ "Error! A file named " 
                                   <> T.pack fPath 
                                   <> "is already in use!"
            Nothing -> do
                hdl <- liftIO $ initDumpFile fPath
                let insertPath = over openDumpFiles $ Map.insert fPath hdl
                d <- view (env . displayChan) <$> get 
                modify $ over env $  insertPath
                return $ Right (dumpPkt hdl n d fPath, Just insertPath)

------
-- The following are "higher order" machines that modify or combine other machines in various ways. See the notes in the PacketAutomata module for more detail.
------

untilR :: Parser (StateT MyParserState IO ParseOutput)
untilR = lexeme $ try $ do
    void $ lexeme $ string "until"
    p      <- protoSelectorPredicate
    mArg   <- machineArrParens
    return $! do
        mArg' <- subMachines mArg 
        case mArg' of
            Left err -> return $ Left err
            Right m -> 
                case p of
                    Right p' -> return $ Right (until p' m, Nothing)
                    Left err -> return $ Left err 

unlessR :: Parser (StateT MyParserState IO ParseOutput)
unlessR = lexeme $ try $ do
    void . lexeme $ string "unless"
    prd  <- protoSelectorPredicate
    mArg <- machineArrParens
    return $ do
        mArg' <- subMachines mArg
        case mArg' of
            Left err -> return $ Left err
            Right m  -> 
                case prd of
                    Left err -> return $ Left err
                    Right p  -> return $ Right (unless' p m,Nothing)


whenR :: Parser (StateT MyParserState IO ParseOutput)
whenR = lexeme $ try $ do
    void . lexeme $ string "when"
    p <- protoSelectorPredicate
    mArg   <- machineArrParens
    return $! do
        mArg' <- subMachines mArg 
        case mArg' of
            Left err -> return $ Left err
            Right m  -> 
                case p of
                    Right p' -> return $ Right (when' p' m, Nothing)
                    Left err -> return $ Left err 


afterR :: Parser (StateT MyParserState IO ParseOutput)
afterR = lexeme $ try $ do
    void $ lexeme $ string "after"
    p      <- protoSelectorPredicate
    mArg   <- machineArrParens
    return $! do
        mArg' <- subMachines mArg 
        case mArg' of
            Left err -> return $ Left err
            Right m  -> 
                case p of
                    Right p' -> return $ Right (after p' m, Nothing)
                    Left err -> return $ Left err 



switchR :: Parser (StateT MyParserState IO ParseOutput)
switchR = lexeme $ try $ do
    void $ lexeme $ (string "switch" <|> string "sw")
    mode   <- switchMode
    p      <- protoSelectorPredicate
    mArg1  <- machineArrParens
    mArg2  <- machineArrParens
    return $! do
        m1 <- subMachines mArg1
        m2 <- subMachines mArg2
        case sequence [m1,m2] of
            Right [m1',m2'] ->
                case p of
                    Right p' -> return $ Right (switch mode p' m1' m2', Nothing)
                    Left err -> return $ Left err
            Left err -> return $ Left err


countSwitchR :: Parser (StateT MyParserState IO ParseOutput)
countSwitchR = lexeme $ try $ do
    void $ lexeme $ (string "countSwitch" <|> string "swC")
    n <- lexeme $ some (satisfy isDigit)
    mode <- switchMode 
    mArg1 <- machineArrParens
    mArg2 <- machineArrParens
    let n' = read n :: Int 
    return $! do
        m1 <- subMachines mArg1
        m2 <- subMachines mArg2
        case sequence [m1,m2] of
            Right [m1',m2'] -> return $ Right (counterSwitch mode n' m1' m2', Nothing)
            Left err -> return $ Left err

timeSwitchR :: Parser (StateT MyParserState IO ParseOutput)
timeSwitchR = lexeme $ try $ do
    void $ lexeme  (string "timeSwitch" <|> "swT")
    n <- lexeme $ some (satisfy isDigit)
    mArg1 <- machineArrParens
    mArg2 <- machineArrParens
    let n' = read n :: Int 
    return $! do
        m1 <- subMachines mArg1
        m2 <- subMachines mArg2
        case sequence [m1,m2] of
            Right [m1',m2'] -> do
                myTVar <- liftIO $ newTVarIO Off
                return $! Right $ (timerSwitch myTVar n' m1' m2', Nothing) 
            Left err -> return $ Left err

caseSwitchR :: Parser (StateT MyParserState IO ParseOutput)
caseSwitchR = lexeme $ try $ do
    void $ lexeme $ string "case"
    void $ lexeme $ char '['
    cases <- lexeme $ caseParser `sepBy1` (lexeme $ char ';')
    void $ lexeme $ char ']'
    return $! do 
        s <- get
        let e = s ^. env 
        case sequence cases of
            Left _ -> return $ Left $ formatList "" . lefts $ cases  
            Right cs -> do
                mchs <- mapM (\(x,y) -> sequence  (x, subMachines y)) cs 
                let mchs' = foldr (\(x,y) acc -> case y of
                     Right m -> pure (:) <*> Right (x,m) <*> acc
                     Left err -> Left err) (Right []) mchs
                case mchs' of
                    Left err -> return $ Left err
                    Right cases -> return $ Right (case' cases, Nothing)

caseParser :: Parser (Either T.Text (Predicate, MachineArrow T.Text))
caseParser = lexeme $ try $ do
    myPredicate <- protoSelectorPredicate
    void $ lexeme $ string "=>"
    machName <- machineArrow
    return $! (\x -> (x , machName)) <$> myPredicate 


machByName :: Parser (StateT MyParserState IO ParseOutput)
machByName = lexeme $ try $ do
    rest  <- lexeme $ 
        some (satisfy $ \x -> isLetter x || isDigit x || x == '_')
    eof 
    return $! do
        s  <- get
        let x = getMachineByNameSTATE (MachineName $ T.pack rest) (s ^. env)
        case x of
            Right m -> return . Right $ (m,Nothing)
            Left err -> return $ Left err 

timeOutFunc :: Int -> Double -> Int -> Double -> Maybe Double
timeOutFunc  maxTOs toMultiplier timeOutCnt timeOut
    = if timeOutCnt >= maxTOs
        then Nothing
        else Just $ timeOut * toMultiplier 


listenForR :: Parser (StateT MyParserState IO ParseOutput)
listenForR = lexeme $ try $ do
    void $ lexeme $ (string "listenFor" <|> "lFor")
    lFor   <- msgSelectorExpPlus
    dbl    <- prefix "timeout=" Nothing double 
    maxTOs <- prefix "maxTimeouts=" Nothing int 
    mult   <- prefix "multiplier=" Nothing double
    mArg   <- prefix "onResponse" Nothing machineArrParens
    -- need to write a parser for the "OnTimeout" function
    return $! do
        m <- subMachines mArg
        case m of
            Left err -> return $ Left err  
            Right mch -> do
                s <- get 
                let e        = s ^. env 
                let dChan    = e ^. displayChan
                let lID      = e ^. listenID
                let rID      = e ^. responseID
                let reqChan  = e ^. listenReqChan
                lID' <- liftIO $ getID' lID
                let f = timeOutFunc maxTOs mult 
                return $ Right ( listener dChan lFor rID lID' reqChan dbl f mch, Nothing)


limitR :: Parser (StateT MyParserState IO ParseOutput)
limitR = lexeme $ try $ do
    void $ lexeme $ string "limit"
    n <- int
    m' <- machineArrParens 
    return $! do
        m <- subMachines m'
        case m of
            Right mach -> return $ Right (limit n mach, Nothing)
            Left err  -> return $ Left err 

subMachines :: MachineArrow T.Text -> ParserMonad 
subMachines m' = do
    s <- get
    s' <- liftIO $ 
        execStateT (makeMachines m') (set machineAcc (Right echo) $ set funcsOnSuccess [] s)
    case s' ^. machineAcc of
        Right m -> do

            let newFuncs = s' ^. funcsOnSuccess
            let e = s ^. env
            let e' = foldl' (\z f -> f z) e newFuncs

            modify $ over env (\x -> e')

            modify $ over funcsOnSuccess (<> newFuncs)
            
            return $ Right m 

        Left err -> return $ Left err 

sendIt :: Parser (StateT MyParserState IO ParseOutput)
sendIt = lexeme $ try $ do
    void $ lexeme $ string "send"
    return $! do 
        e <- view env <$>  get 
        let d    = e ^. displayChan 
        let lock = e ^. pcapLock
        let hdl  = e ^. pcapHandle
        return $! Right (send d lock hdl,Nothing)


machineName :: Parser MachineName
machineName = lexeme $ try $ do
    mname  <- (lexeme $ 
        some (satisfy $ \x -> isLetter x || isDigit x)) <?> "Error parsing machine name. Machine names may only consists of upper/lowercase letters or digits."
    return $ MachineName $ T.pack $ mname



machines :: Parser (StateT MyParserState IO ParseOutput)
machines = lexeme  
         $  popR 
        <|> pullR
        <|> extractR
        <|> cutR
        <|> setR
        <|> modifyOptR
        <|> insertOptR
        <|> deleteOptR
        <|> selectR
        <|> discardR
        <|> alertR
        <|> voidR
        <|> debugR
        <|> reportR
        <|> createR
        <|> counterR
        <|> bufferR
        <|> untilR
        <|> unlessR
        <|> dumpPktR
        <|> whenR
        <|> afterR
        <|> switchR
        <|> countSwitchR
        <|> timeSwitchR
        <|> caseSwitchR
        <|> listenForR
        <|> limitR
        <|> sendIt
        <|> experimentalSelect
        <|> prettyPrintR
        <|> makeRandom
        <|> pushR
        <|> liftR
        <|> chkSumR
        <|> printFieldR
        <|> writeFieldR 
        <|> machByName  
{--
machines :: Parser (StateT MyParserState IO ParseOutput)
machines = lexeme $ try $ do
    first <- lookAhead (many $ satisfy (/= ' '))
    case first of
        "pop"         -> popR 
        "pull"        -> pullR
        "extract"     -> extractR
        "cut"         -> cutR
        "set"         -> setR
        "modifyOpt"   -> modifyOptR
        "insertOpt"   -> insertOptR
        "deleteOpt"   -> deleteOptR
        "select"      -> selectR
        "discard"     -> discardR
        "alert"       -> alertR
        "void"        -> voidR
        "debug"       -> debugR
        "report"      -> reportR
        "create"      -> createR
        "count"       -> counterR
        "buffer"      -> bufferR
        "until"       -> untilR
        "unless"      -> unlessR
        "dump"        -> dumpPktR
        "when"        -> whenR
        "after"       -> afterR
        "switch"      -> switchR
        "countSwitch" -> countSwitchR
        "timeSwitch"  -> timeSwitchR
        "case"        -> caseSwitchR
        "listenFor"   -> listenForR
        "limit"       -> limitR
        "send"        -> sendIt
        "expSelect"   -> experimentalSelect
        "prettyPrint" -> prettyPrintR
        "randomize"   -> makeRandom
        "push"        -> pushR
        "lift"        -> liftR
        "checksum"    -> chkSumR
        "printField"  -> printFieldR
        "writeField"  -> writeFieldR 
        _             -> machByName 
--}


data NamedMachine = NamedMachine MachineName (MachineArrow T.Text) deriving (Show, Eq)

namedMachine :: Parser NamedMachine
namedMachine = lexeme $ try $ do
    name   <- lexeme $ some (satisfy $ \x -> isLetter  x || isDigit x)
    void $ lexeme $ string "="
    mach <- machineArrow
    return $! NamedMachine (MachineName . T.pack $ name) mach 



machineArrParens :: Parser (MachineArrow T.Text)
machineArrParens = lexeme $ try $ do
    x <- recParens
    let x' = T.drop 1 $ T.reverse  (T.drop 1 . T.reverse $ x) 
    case parseLex machineArrow x' of
        Left err -> fail $ show err
        Right y -> return y 

machineArrow :: Parser (MachineArrow T.Text)
machineArrow = lexeme $ try $ do
            a <- untilArr
            aSymb <- voidEof <|> arrSymb
            case aSymb of
                "~>" -> do
                    rest <- machineArrow 
                    return $ a :~> rest
                "~+>" -> do
                    rest <- some machineArrParens
                    return $   a :~+> rest
                ":|"  -> return $  a :| ()

arrSymb :: Parser T.Text
arrSymb =  (lexeme $ try $ string "~>") <|> (lexeme $ try $ string "~+>") 



voidEof :: Parser T.Text
voidEof = lexeme $ try $ do
    _ <- eof
    return ":|"


untilArr :: Parser T.Text
untilArr = lexeme $ try $ do
    first <- lexeme $ manyTill anySingle (lookAhead arrSymb <|> lookAhead (string "(") <|> voidEof)
    sep   <- voidEof <|>  (lookAhead $ lexeme . try $ string "(") <|>  (lookAhead arrSymb)
    case sep of
        "(" -> do

            btwn <- recParens -- between (char '(') (char ')') (many $ satisfy (\x -> x /= ')')) 

            rest <- untilArr
            return $ T.pack first <>  btwn <> rest
        "[" -> do
            btwn <- manyTill anySingle (lookAhead . lexeme $ char '[')
            void . lexeme $ char ']' 
            rest <- untilArr
            return $ T.pack (first <> "[" <> btwn <> "]") <> rest
        _   -> return $ T.pack  first 

notSep :: Parser [Char]
notSep = lexeme . try $ many $ satisfy (\x -> x `notElem` ("()" :: String))

builder :: Parser a -> Parser (V.Vector a)
builder p = lexeme $ try $ do
    first <- (between 
            (lexeme $ char '[') 
            (lexeme $ char ']') 
            (p  `sepBy1` (lexeme $ char ';') )) <?> "Error: Expected a builder. A builder has the form [ <SOMETHING> ; <MAYBE SOMETHING ELSE> ]"
    return $! V.force $ V.fromList $ first 
        
-- This is a hack to allow higher or machines to work properly. "ignores" nested parentheses between top-level machine arrows
-- (Doesn't actually ignore them, parses them then reconstructs the string)
------
recParens :: Parser T.Text
recParens = lexeme $ try $ do 
    (void . lexeme $ open) <?> "Error: Expected an open paren '('"
    first <- manyTill anySingle (lookAhead open <|> lookAhead close)
    c <- lookAhead . lexeme $ open <|> close
    case c of
        '(' -> do
            child <- (some recParens) <?> "Error: Mismatched parentheses (maybe?)"
            rest  <- manyTill anySingle close <?> "Expected a close paren ')' "
            return $ "(" <> T.pack first <> T.concat child <> T.pack rest <> ")" 
        ')' -> do
            (void . lexeme $ close ) <?> "Error: Mismatched parentheses (maybe?)"
            return $ "(" <> T.pack first <> ")"


   where
       open :: Parser Char
       open = (lexeme . try $ char '(') 

       close :: Parser Char
       close = (lexeme . try $ char ')') 

