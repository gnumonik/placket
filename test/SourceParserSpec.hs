{-# LANGUAGE OverloadedStrings #-}

module SourceParserSpec where

import MachineParser
import FactoryTypes 
import PrimParsers 
import FieldClasses
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Trans.State.Strict  
import Control.Concurrent.STM
import Data.Machine  
import qualified Data.Text as T
import PacketIO 
import System.Random.Mersenne.Pure64
import qualified Data.Map as Map 
import Control.Applicative
import MachineParserSpec hiding (runTest)
import SourceParser 
import PrimParsers 
import PrimParserSpec
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import CommandParser

-- Tests for command parser are also here (uses the same setup, more or less )

mkDummyEnv :: IO Env
mkDummyEnv = do
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
    newTVarIO initEnv

runTest :: T.Text -> IO (Result T.Text)
runTest txt = do
  myEnv <- mkDummyEnv
  r <- liftIO $ runReaderT (sourceBuilder Anonymous txt) myEnv
  case r of
    Just _ -> return Pass
    _      -> return $ Fail "FAIL!"


runTestC :: T.Text -> IO (Result T.Text)
runTestC txt = do
  myEnv <- mkDummyEnv
  let c = parseT commands txt 
  case c of
    Right _  -> return Pass
    Left err -> return $ Fail $ T.pack . show $ err 

testSourceParsers :: IO ()
testSourceParsers = hspec $
  describe "SourceParsers" $ do

    describe "generateS" $ do
      it "passes" $ 
        runTest "generate wait=10 repeat=10 [ARP (op=7) ; ETH (etherType=2054)]" 
              >>= \x -> x `shouldBe` Pass

    describe "generateS2" $ do
      it "passes" $
        runTest "generate 10 10 [ARP (op=7) ; ETH (etherType=2054)]" >>= \x -> 
          x `shouldBe` Pass

    describe "generateS3" $ do
      it "passes" $
        runTest "generate 10 10 [ARP (op=7) ; ETH (etherType=2054)]" >>= \x -> 
          x `shouldBe` Pass

    describe "generateS4" $ do
      it "passes" $
        runTest "generate [HARP (op=7) ; ERTH (etherType=2054)]" >>= \x -> 
          x `shouldBe` Fail "FAIL!"

    describe "genRandomS" $ do
      it "passes" $ 
        runTest "genRandoms num=100 wait=500 repeat=1000 [DNS ; UDP ; IP4 ; ETH]" >>= \x -> x `shouldBe` Pass

    describe "genRandomS2" $ do
      it "passes" $ 
        runTest "genRandoms 100 500 1000 [DNS ; UDP ; IP4 ; ETH]" >>= \x -> 
          x `shouldBe` Pass

    describe "genRandomS3" $ do
      it "passes" $ 
        runTest "genRandoms  [DNS ; UDP ; IP4 ; ETH]" >>= \x -> 
          x `shouldBe` Pass

    describe "listen" $ do
      it "passes" $
        runTest "listen" >>= \x -> x `shouldBe` Pass

    describe "why" $ do
      it "passes" $ 
        runTest "(listen) :Y: (genRandoms  [DNS ; UDP ; IP4 ; ETH])" >>= \x -> 
          x `shouldBe` Pass

    describe "tea" $ do
      it "passes" $ 
        runTest "(listen) :T: (genRandoms  [DNS ; UDP ; IP4 ; ETH])" >>= \x -> 
          x `shouldBe` Pass

    describe "readPcap" $ do
      it "passes" $
        runTest "read path=\"/home/gnumonic/testPCAP.pcap\"" >>= \x ->
          x `shouldBe` Pass

    describe "readPcap2" $ do
      it "passes" $
        runTest "read \"/home/gnumonic/testPCAP.pcap\"" >>= \x ->
          x `shouldBe` Pass
    

testCommandParsers :: IO ()
testCommandParsers = hspec $
  describe "CommandParsers" $ do

    describe "deviceInfo" $ do 
      it "passes" $
        runTestC "deviceInfo" >>= \x -> x `shouldBe` Pass

    describe "exit" $ do
      it "passes" $
        runTestC "exit" >>= \x -> x `shouldBe` Pass

    describe "saveFile" $ do
      it "passes" $
        runTestC "save mode=append path=\"/home/gnumonic/testsave.txt\"" >>= \x -> 
          x `shouldBe` Pass

    describe "saveFile2" $ do
      it "passes" $
        runTestC "save mode=write path=\"/home/gnumonic/testsave.txt\"" >>= \x -> 
          x `shouldBe` Pass

    describe "saveFile3" $ do
      it "passes" $
        runTestC "save path=\"/home/gnumonic/testsave.txt\"" >>= \x -> 
          x `shouldBe` Pass

    describe "loadFile" $ do
      it "passes" $
        runTestC "load path=\"/home/gnumonic/testsave.txt\"" >>= \x -> 
          x `shouldBe` Pass

    describe "showStats" $ do
      it "passes" $
        runTestC "showStats" >>= \x -> 
          x `shouldBe` Pass

    describe "showArpCache" $ do
      it "passes" $
        runTestC "showArpCache" >>= \x -> 
          x `shouldBe` Pass

    describe "showSources" $ do
      it "passes" $
        runTestC "showSources" >>= \x -> 
          x `shouldBe` Pass

    describe "showFactories" $ do
      it "passes" $
        runTestC "showFactories" >>= \x -> 
          x `shouldBe` Pass
    
    describe "showMachines" $ do
      it "passes" $
        runTestC "showMachines" >>= \x -> 
          x `shouldBe` Pass

    describe "kill" $ do
      it "passes" $
        runTestC "kill abcd" >>= \x -> 
          x `shouldBe` Pass

    describe "killAll" $ do
      it "passes" $
        runTestC "killAll" >>= \x -> 
          x `shouldBe` Pass

    describe "stop" $ do
      it "passes" $ 
        runTestC "stop abc" >>= \x -> 
          x `shouldBe` Pass

    describe "stopAll" $ do
      it "passes" $ 
        runTestC "stopAll" >>= \x -> 
          x `shouldBe` Pass

    describe "run: listen >> void" $ do
      it "passes" $ 
        runTestC "run: listen >> void" >>= \x -> 
          x `shouldBe` Pass

    describe "clearMachines" $ do
      it "passes" $
        runTestC "clearMachines" >>= \x -> 
          x `shouldBe` Pass

    describe "deleteMachine abc" $ do
      it "passes" $
        runTestC "deleteMachine abc" >>= \x -> 
          x `shouldBe` Pass 

    describe "clearSources" $ do
      it "passes" $
        runTestC "clearSources" >>= \x -> 
          x `shouldBe` Pass

    describe "deleteSource abc" $ do
      it "passes" $
        runTestC "deleteSource abc" >>= \x -> 
          x `shouldBe` Pass 

    describe "clearFactories" $ do
      it "passes" $
        runTestC "clearFactories" >>= \x -> 
          x `shouldBe` Pass