{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications #-} 

module SourceParser where 

import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified PacketOperations as PO
import FactoryTypes
import Control.Monad
import PrimParsers
import Text.Megaparsec
import MachineParser (builder)
import PacketIO 
import Data.Proxy 
import Data.Char
import PacketSources
import MyReaderT 
import RecordTypes (ProtocolBuilder)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Lens (over, view)
import PrimTypes (EthernetFrame(EthernetFrame))
import RecordParsers (protocolType, protocolBuilder)
import ArgumentParsers 


type SourceParser = Parser (MyReader (Either T.Text PacketSrc))

generateS :: SourceParser
generateS = lexeme $ try $ do
    void $ lexeme $ string "generate"
    dly <- prefix "wait=" (Just 0) int 
    void . lexeme $ string "repeat="
    rpts <-  prefix "repeat=" (Just 0) int 
    bld <- builder protocolBuilder
    let ps =  V.force <$> V.mapM PO.makeProtocolMessageV2 bld
    case ps of
            Left str -> return . return $! Left str
            Right bldr -> do
                return . return $! 
                 Right $ Generator $ generator bldr (Just  rpts ) (Just dly )

genRandomS :: SourceParser
genRandomS = lexeme $ try $ do
    void . lexeme $ string "genRandoms"
    n <- prefix "num=" (Just 1) int 
    dly <-   prefix "wait=" (Just 0) int
    rpts <-  prefix "repeat=" (Just 0) int 
    b <- builder protocolType 
    let myVec = PO.randomP b
    case myVec of
        Left err -> return . return $ Left err
        Right vec -> return $ do
            s <- ask >>= \e' -> 
                (liftIO . readTVarIO $ e') >>= \e -> 
                    return $ view randSeed e 
            let (!vecs, newSeed) = PO.randomPs n s vec
            return $! Right . Generator $ generator vecs (Just rpts) (Just dly)



-- Eventually want to add support for multiple listeners on different devices. To do: Get a second network adapter in order to test that. 

whyS :: SourceParser
whyS = lexeme $ try $ do
    first <- (lexeme $ (between (lexeme $ char '(') (lexeme $ char ')') sources) ) 
    void . lexeme $ string ":Y:"
    second <- lexeme $ (between (lexeme $ char '(') (lexeme $ char ')') sources)  
    return $ do
        m1 <- first
        m2 <- second
        case sequence [m1,m2] of
            Right [m1',m2'] -> return . Right $ Why m1' m2'
            Left err        -> return $ Left err 

teaS :: SourceParser 
teaS = lexeme $ try $ do
    lexeme $ try $ do
        first <- (lexeme $ (between (lexeme $ char '(') (lexeme $ char ')') sources) ) 
        void . lexeme $ string ":T:"
        second <- (lexeme $ (between (lexeme $ char '(') (lexeme $ char ')') sources) ) 
        return $ do
            m1 <- first
            m2 <- second
            case sequence [m1,m2] of
                Right [m1',m2'] -> return . Right $ Tea m1' m2'
                Left err        -> return $ Left err 

sourceNm :: Parser T.Text
sourceNm = lexeme $ try $ do
    s <- some $ letterChar <|> digitChar 
    return . T.pack $ s 


listenS :: SourceParser
listenS = lexeme $ try $ do
    void $ lexeme $ string "listen"
    -- hdl <- option Nothing deviceNm
    return . return $  Right  $ Listener -- hdl
   where
       deviceNm :: Parser (Maybe T.Text)
       deviceNm = lexeme $ try $ do
           void . lexeme $ string "handle="
           myHdl <- quotedString
           return $ Just  myHdl 

readPcapS :: SourceParser 
readPcapS = lexeme $ try $ do
    void . lexeme $ string "read"
    fPath <- prefix "path=" Nothing filePath 
    return $ do
        paths <- ask >>= \e -> liftIO (readTVarIO e) >>= \e' -> return $ view openReadFiles e' 
        case Map.lookup fPath paths of
            Just _ -> return $ Left $ "Error! A file named " 
                                   <> T.pack fPath 
                                   <> "is already in use!"
            Nothing -> do
                hdl <- liftIO $ initOffline fPath
                e <- ask 
                liftIO . atomically . modifyTVar' e $ over openReadFiles $ Map.insert fPath hdl
                l <- liftIO $ newTMVarIO ()
                return $ Right $ Generator $ readPcap hdl l (Proxy @EthernetFrame)
                
sourceByName :: SourceParser
sourceByName = lexeme $ try $ do
    name <- sourceNm
    return $ do
        s <- getSourceByName name
        case s of
            Left err -> return $ Left err
            Right s  -> return $ Right s


sources :: SourceParser
sources = whyS <|> teaS <|> listenS <|> generateS <|> readPcapS <|> genRandomS <|> sourceByName
 where
  go = lexeme $ try $ do 
    first <- lookAhead (some $ satisfy (/= ' '))
    case first of
        "listen"       -> listenS
        "generate"     -> generateS
        "read"         -> readPcapS
        "genRandoms"   -> genRandomS
        _              -> sourceByName 