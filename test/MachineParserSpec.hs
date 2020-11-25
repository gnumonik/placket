{-# LANGUAGE OverloadedStrings #-}

module MachineParserSpec where

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

-- yeah yeah it's basically Maybe 
-- ^. originally refers to: Data Result a = Pass | Fail a. 
data Result a = Pass | Fail a | Abort deriving (Show, Eq)

--testing is boring, defining a monad is more fun 
instance Functor Result where
  fmap f Abort = Abort 
  fmap f Pass  = Pass 
  fmap f (Fail x) = Fail (f x)

-- useless applicative instance
instance Applicative Result where
  pure = Fail 
  (Fail f) <*> (Fail x) = Fail $ f x 
  _ <*> _ = Abort 

instance Monad Result where

  return = pure 

  (Fail x) >>= f = case f x of
    Fail y -> Fail y 
    Pass   -> Pass 
    Abort  -> Abort
    
  Abort >>= _ = Abort

  Pass >>= _ = Pass 

instance Alternative Result where 

  Fail _ <|> Fail y = Fail y

  _    <|> Pass     = Pass 
  Pass <|> _        = Pass 
  
  Abort <|> x        = x
  x     <|> Abort    = x

  empty = Abort 


-- pass <|> (fail <|> abort ) = (pass <|> fail) <|> abort 
-- fail <|> (pass <|> abort)  = (fail <|> pass) <|> abort 
-- 


runTest :: T.Text -> IO (Result T.Text)
runTest txt  = do
    myEnv <- mkDummyState
    r     <- evalStateT (makeMachine txt) myEnv
    case r of
      Left err -> return $ Fail err
      Right _  -> return Pass 

  
mkDummyState :: IO MyParserState
mkDummyState = do
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

-- Initialize PCAP (NOTE: Switch to bounded channels so packets don't accrete forever)
    pcapHandle <- initPCAP --initOffline --initPCAP

    let initEnv = Environment  
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
    return $ MyParserState (Right echo) [] initEnv 


testMachineParsers :: IO () 
testMachineParsers = hspec $ 
  describe "MachineParsers" $ do
    
    describe "makeRandom" $ do
      it "passes" $ 
        runTest "randomize 10 [ARP ; ETH]" >>= \x -> x `shouldBe` Pass  

    describe "makeRandom" $ do
      it "passes" $ 
        runTest "randomize [ARP ; ETH]" >>= \x -> x `shouldBe` Pass  

    describe "prettyPrintDef" $ do
      it "passes" $ 
        runTest "prettyPrint mode=default" >>= \x -> x `shouldBe` Pass 

    describe "prettyPrintVar1" $ do
      it "passes" $ 
        runTest "prettyPrint default" >>= \x -> x `shouldBe` Pass 

    describe "prettyPrintVar2" $ do 
      it "passes" $
        runTest "prettyPrint" >>= \x -> x `shouldBe` Pass 

    describe "pp" $ do 
      it "passes" $ 
        runTest "pp" >>= \x -> x `shouldBe` Pass 

    describe "printField" $ do 
      it "passes" $ 
        runTest "printField  label=\"label\" mode=hex IP4 proto" >>= \x -> 
          x `shouldBe` Pass 

    describe "printField3" $ do 
      it "passes" $ 
        runTest "printField \"label\" mode=hex IP4 proto" >>= \x -> 
          x `shouldBe` Pass      

    describe "printField4" $ do 
      it "passes" $ 
        runTest "printField mode=hex IP4 proto" >>= \x -> 
          x `shouldBe` Pass     

    describe "printField5" $ do 
      it "passes" $ 
        runTest "printField hex IP4 proto" >>= \x -> 
          x `shouldBe` Pass   

    describe "printField6" $ do 
      it "passes" $ 
        runTest "printField IP4 proto" >>= \x -> 
          x `shouldBe` Pass   

    describe "writeField" $ do 
      it "passes" $ 
        runTest "writeField path=\"path\" label=\"label\" mode=hex IP4 proto" >>= \x -> 
          x `shouldBe` Pass 

    describe "writeField2" $ do 
      it "passes" $ 
        runTest "writeField path=\"path\" hex IP4 proto" >>= \x -> 
          x `shouldBe` Pass 

    describe "writeField3" $ do 
      it "passes" $ 
        runTest "wf path=\"path\" IP4 proto" >>= \x -> x `shouldBe` Pass 

    describe "writeField4" $ do 
      it "passes" $ 
        runTest "wf path=\"path\" IP4 proto" >>= \x -> x `shouldBe` Pass 

    describe "pop" $ do
      it "passes" $ 
        runTest "pop DNS" >>= \x -> x `shouldBe` Pass 

    describe "pull" $ do 
      it "passes" $ 
        runTest "pull DNS" >>= \x -> x `shouldBe` Pass 

    describe "extract" $ do 
      it "passes" $ 
        runTest "extract DNS" >>= \x -> x `shouldBe` Pass 

    describe "cut" $ do 
        it "passes" $
          runTest "cut DNS" >>= \x -> x `shouldBe` Pass 

    describe "push" $ do 
        it "passes" $
          runTest "push DNS" >>= \x -> x `shouldBe` Pass 

    describe "lift" $ do 
        it "passes" $ 
          runTest "lift DNS" >>= \x -> x `shouldBe` Pass 

    describe "set" $ do 
        it "passes" $ 
          runTest "set IP4(proto=77 flags.df=T src=10.10.10.10)" >>= \x -> 
            x `shouldBe` Pass  

    describe "set2" $ do 
      it "passes" $ 
        runTest "set IP4" >>= \x -> x `shouldBe` Pass 

    -- if this fails i'll eat a toaster 

    describe "checksum" $ do 
      it "passes" $ 
        runTest "checksum" >>= \x -> x `shouldBe` Pass 

    