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
          runTest "push ARP (op=2 sha=12:34:56:78:aa:bb)" >>= \x -> x `shouldBe` Pass 

    describe "lift" $ do 
        it "passes" $ 
          runTest "lift ETH (src=12:34:56:78:aa:bb dst=ff:ff:ff:ff:ff:ff etherType=666)" >>= \x -> x `shouldBe` Pass 

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

    describe "modifyOpt" $ do
      it "passes" $ 
        runTest "modifyOpt IP4 opts [(opType.opClass/=0) => opType.opClass=8]" >>= \x -> 
          x `shouldBe` Pass
    
    describe "modifyOpt2" $ do
      it "passes" $ 
        runTest "mOpt IP4 opts [(opType.opClass/=0) => opType.opClass=8]" >>= \x -> 
          x `shouldBe` Pass 

    describe "insertOpt" $ do
      it "passes" $ 
        runTest "insertOpt IP4 opts (opType.opNum=2 opType.opClass=3 opType.opNum=4 opLength=6 opData=face)" >>= \x -> 
          x `shouldBe` Pass 

    describe "insertOpt" $ do
      it "passes" $ 
        runTest "iOpt IP4 opts (opType.opNum=2 opType.opClass=3 opType.opNum=4 opLength=6 opData=face)" >>= \x -> 
          x `shouldBe` Pass 

    describe "deleteOpt" $ do
      it "passes" $ 
        runTest "deleteOpt IP4 opts (opType.opNum=3 || opType.opNum<2)" >>= \x -> 
          x `shouldBe` Pass 

    describe "expSelect" $ do
      it "passes" $ 
        runTest "expSelect [* ; TCP (src=443 || dst=443) ; *]" >>= \x -> 
          x `shouldBe` Pass

    describe "select1" $ do 
      it "passes" $ 
        runTest "select ARP" >>= \x -> x `shouldBe` Pass 

    describe "select2" $ do 
      it "passes" $ 
        runTest "select ARP (op>2 && hrd!=6)" >>= \x -> x `shouldBe` Pass 

    describe "discard" $ do
      it "passes" $ 
        runTest "discard ARP" >>= \x -> x `shouldBe` Pass 

    describe "discard2" $ do 
      it "passes" $ 
        runTest "discard TCP(win<600)" >>= \x -> x `shouldBe` Pass 

    describe "alert" $ do
      it "passes" $ 
        runTest "alert \"someAlert\" IP4(flags.df=T || checksum=0)" >>= \x -> 
          x `shouldBe` Pass 

    -- need to rework 'stash'

    describe "void" $ do 
      it "passes" $ 
        runTest "void" >>= \x -> x `shouldBe` Pass

    describe "report" $ do 
      it "passes" $ 
        runTest "report \"SHOOP!\"" >>= \x -> x `shouldBe` Pass 

    describe "create1" $ do
      it "passes" $ 
        runTest ("create wait=0 repeat=0 [ARP (op=2 tpa=192.168.0.2)" 
                <> "; ETH (etherType=2054)]" ) >>= \x -> x `shouldBe` Pass    

    describe "create2" $ do
      it "passes" $ 
        runTest ("create repeat=0 [ARP (op=2 tpa=192.168.0.2)" 
                <> "; ETH (etherType=2054)]" ) >>= \x -> x `shouldBe` Pass    

    describe "create3" $ do
      it "passes" $ 
        runTest ("create [ARP (op=2 tpa=192.168.0.2)" 
                <> "; ETH (etherType=2054)]" ) >>= \x -> x `shouldBe` Pass    

    describe "create1" $ do
      it "passes" $ 
        runTest ("create 0 0 [ARP (op=2 tpa=192.168.0.2)" 
                <> "; ETH (etherType=2054)]" ) >>= \x -> x `shouldBe` Pass    

    describe "count" $ do 
      it "passes" $ 
        runTest "count 100" >>= \x -> x `shouldBe` Pass 

    describe "buffer" $ do
      it "passes" $ 
        runTest "buffer 100" >>= \x -> x `shouldBe` Pass 

    describe "dump1" $ do
      it "passes" $ 
        runTest "dump path=\"/home/gnumonic/DUMPPPP\" numPackets=10000" >>= \x -> 
          x `shouldBe` Pass 

    describe "dump2" $ do
      it "passes" $ 
        runTest "dump \"/home/gnumonic/DUMPPPP\" 10000" >>= \x -> 
          x `shouldBe` Pass 

    describe "until" $ do 
      it "passes" $ 
        runTest "until TCP(seqNum>100) (pp)" >>= \x -> x `shouldBe` Pass 

    describe "unless" $ do 
      it "passes" $ 
        runTest "unless IP4(flags.df=1) (void)" >>= \x -> x `shouldBe` Pass 

    describe "when" $ do 
      it "passes" $ 
        runTest "when IP4(flags.df=1) (void)" >>= \x -> x `shouldBe` Pass   

    describe "after" $ do 
      it "passes" $ 
        runTest "after TCP(seqNum>100) (pp)" >>= \x -> x `shouldBe` Pass 

    describe "switch1" $ do 
      it "passes" $ 
        runTest "switch reset IP4(flags.df=T) (void) (pp)" >>= \x -> x `shouldBe` Pass 

    describe "switch2" $ do 
      it "passes" $ 
        runTest "switch IP4(flags.df=T) (void) (pp)" >>= \x -> x `shouldBe` Pass 

    describe "switch3" $ do 
      it "passes" $ 
        runTest "sw IP4(flags.df=T) (void) (pp)" >>= \x -> x `shouldBe` Pass 

    describe "countSwitch" $ do 
      it "passes" $ 
        runTest "countSwitch 100 reset (pp) (void)" >>= \x -> x `shouldBe` Pass 

    describe "countSwitch2" $ do 
      it "passes" $ 
        runTest "swC 100 reset (pp) (void)" >>= \x -> x `shouldBe` Pass 

    describe "timeSwitch" $ do 
      it "passes" $ 
        runTest "timeSwitch 500000 (pp) (void)" >>= \x -> x `shouldBe` Pass 

    describe "case" $ do 
      it "passes" $ 
        runTest "case [IP4 => (pp) ; ARP(op>2 || hrd/=4) => (report \"doop\") ]" >>= \x -> 
          x `shouldBe` Pass 

    describe "listenFor" $ do 
      it "passes" $ 
        runTest ("listenFor [ * ; IP4(proto=$(proto)) ; * ] timeout=2.5"
                 <> "maxTimeouts=3 multiplier=.4 onResponse=(void)") >>= \x -> x `shouldBe` Pass 

    describe "listenFor2" $ do 
      it "passes" $ 
        runTest "listenFor [ * ; ARP (op<3) ; * ] timeout=2.5 maxTimeouts=3 multiplier=.4 onResponse=(void)" >>= \x -> x `shouldBe` Pass 

    describe "listenFor3" $ do 
      it "passes" $ 
        runTest ("listenFor [ * ; IP4(src=$(dst) || dst=$(src)) ; * ] timeout=2.5"
                 <> "maxTimeouts=3 multiplier=.4 onResponse=(void)") >>= \x -> x `shouldBe` Pass 

    describe "limit" $ do 
      it "passes" $ 
        runTest "limit 100 (report \"doop\")" >>= \x -> x `shouldBe` Pass  

    describe "limit2" $ do 
      it "passes" $ 
        runTest "limit 100 (select ARP (op/= 1 && op /= 2) ~> report \"Got a weird arp!\")" >>= \x -> x `shouldBe` Pass  
