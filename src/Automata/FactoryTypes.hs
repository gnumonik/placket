{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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
{-# LANGUAGE UndecidableInstances, OverloadedStrings, MultiWayIf, BangPatterns #-}

module FactoryTypes where

import Control.Lens.TH 
import Data.Machine
import Classes
import  Data.Default
import PrimTypes 
import qualified Data.Text as T
import qualified Data.Vector as V
import LibTypes
import Control.Monad 
import RecordFuncs
import PacketOperations hiding (setFields)
import PrimParsers
import Data.Char 
import Data.Proxy
import Data.Time.Clock 
import Text.Parsec hiding (token)
import Text.Parsec.Text 
import Control.Monad.Extra 
import Control.Lens 
import Data.Word (Word32, Word16, Word64)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Wrappers 
import Control.Monad.Trans.Reader
import THWrappers (fromA, wrapP, WrapProtocol, Possibly)
import Data.Kind (Type)
import Data.Monoid
import Data.Either (lefts)
import Data.List (foldl')
import Network.Pcap (PcapHandle, PktHdr)
import System.IO (Handle)
import System.Random.Mersenne.Pure64
import Control.Concurrent.Async

-- MachineArrow: A data type that mirrors the structure of Packet Machine definitions. 
-- Not strictly necessary, but useful in developing the parsers.
-- Note: Might be able to simplify the parser if I gave it a traversable instance.
------
data MachineArrow a =
    a :~> MachineArrow a
  | a :~+> [MachineArrow a]
  | a :| () deriving (Eq)


instance Show a => Show (MachineArrow a) where
    show (a :~>  bs) = show a <> " ~> " <> show bs
    show (a :~+> bs) = show a <> "[" <> foldr (\x y -> show x <> "\n," <> y ) "" bs
    show (a :| ())   = show a ++ ":|"

infixr 9 :~>
infixr 9 :~+>
infixr 9 :|


data Factory a b c = Factory a b c
    deriving (Show, Eq)

instance Functor MachineArrow where
    fmap f (a :~> as)  = f a :~> fmap f  as
    fmap f (a :~+> as) = f a :~+> map (fmap f) as
    fmap f (a :| ())   = f a :| ()

machArrToList :: MachineArrow a -> [a]
machArrToList mArr = case mArr of
    (a :| ())   -> [a]
    (a :~> as)  -> a : machArrToList as
    (a :~+> as) -> a : concatMap machArrToList as

instance Foldable MachineArrow where
    foldr f e mArr = let mArr' = machArrToList mArr
                     in foldr f e mArr'


-- Misc. Type Synonyms
------
type StashName = T.Text 

type Predicate = V.Vector ProtocolMessage -> Bool

type TagCount = Int

type PacketMachine = MachineT IO (Is Message) Message

type PrintThis = T.Text

type Message = (PktHdr, V.Vector ProtocolMessage)

type SendChan = TChan Message

type DisplayChan = TChan PrintThis

type PrintChan = TChan T.Text

type RequestID = Int

type ListenerID = Int 

type Repeats = Maybe Int

type Delay   = Maybe Int

data ListenResponse 
    = ListenResponse ListenerID RequestID Message 
    | TimeOut ListenerID RequestID deriving (Show)

data ListenRequest = ListenRequest RequestID ListenData  



data ListenData = ListenData {_packet  :: Message -- The packet that was SENT 
                             ,_pFilter :: Message -> Bool -- The function compiled from a "listenFor" data structure that the ListenServer uses to match inputpackets with listenrequests
                             ,_timeOutCount :: Int
                             ,_timeSent :: UTCTime 
                             ,_timeOut  :: Double-- amount of time in seconds before a resend
                             ,_retryF   :: Int -> Double -> Maybe Double-- how to modify the timeout value after a failure  
                             ,_listenerID :: Int 
                             ,_onMatch    :: PacketMachine}
makeLenses ''ListenData 

data UserInput = Command T.Text | MachineDef T.Text | SourceDef T.Text 

data SrcState' 
    = SRC_ACTIVE
    | SRC_INACTIVE deriving (Show, Eq)

type SrcID = Int

type PacketCount = Int 

data SrcState = SrcState SrcID SrcState' (TVar PacketCount)

data PacketSinkState
    = SINK_INIT 
    | SINK_ACTIVE 
    | SINK_INACTIVE deriving (Show, Eq)

data ToServer
    = GIMMEPACKETS Int [TBQueue Message]
    | NOMOREPACKETS Int 
    | SHOWACTIVE deriving (Eq)

data ToSrc
    = STOP 



type ARPCache = Map IP4Address (MacAddr, UTCTime)

data ServerState = ServerState {_directory   :: Map Int (TBQueue Message)
                               ,_serverStats :: Map Int Int}

data PacketSrc = Generator (MachineT IO (Is ()) Message)
               | Listener -- (Maybe PcapHandle)
               | Why PacketSrc PacketSrc
               | Tea PacketSrc PacketSrc
               | Plugged (MachineT IO (Is ()) Message) [TBQueue Message]

data CappedSrc = CappedSrc (MachineT IO (Is ()) Message) [TBQueue Message]

data ProgramError = ParserError  T.Text | IOError T.Text
makePrisms ''ProgramError

type ErrorLog = [ProgramError]


data ARPServerCmd = PrintARPTables | ResetARPCache deriving (Eq, Show)




data Environment = Environment  {_tagCount       :: !TagCount
                                ,_packetMachines :: !(Map Int MachineData)
                                ,_sourceIDs      :: !(Map T.Text SourceData)
                                ,_pcapLock       :: !(TMVar ())
                                ,_serverQueue    :: !(TBQueue ToServer)
                                ,_sendChan       :: !SendChan
                                ,_displayChan    :: !DisplayChan
                                ,_stashes        :: !(Map T.Text (TVar(V.Vector Message)))
                                ,_errorLog       :: !ErrorLog
                                ,_listenReqChan  :: !(TChan ListenRequest)
                                ,_listenID       :: !(TVar Int)
                                ,_responseID     :: !(TVar Int)
                                ,_arpCache       :: !(TVar ARPCache)
                                ,_openDumpFiles  :: !(Map FilePath Handle)
                                ,_openReadFiles  :: !(Map FilePath PcapHandle)
                                ,_randSeed       :: !(PureMT)
                                ,_cont           :: Bool}

type Env = TVar Environment

type MachineIDs  = Map T.Text PacketMachine

type MyReader = ReaderT (TVar Environment) IO

type PacketSource = SourceT IO  Message 

type PacketSink   = MachineT IO (Is Message) ()

data SourceData = SourceData {_pktSrc :: PacketSrc , _srcSchema :: T.Text}

newtype MachineName = MachineName {mchName :: T.Text} deriving (Show, Eq)

data MachineData = MachineData {_packetMch   :: !PacketMachine 
                               ,_machineNm   :: !MachineName
                               ,_commQueue   :: !(TBQueue ToSrc)
                               ,_isActive    :: !Bool
                               ,_pktCountIn  :: !(TVar Int)
                     --          ,_pktCountOut :: !(TVar Int)
                               ,_schema      :: !(T.Text)
                               ,_thread      :: !(Maybe (Async ()))
                               ,_msgQueue    :: ![TBQueue Message]} 

makeLenses ''MachineData

makeLenses ''SourceData  

makeLenses ''Environment

