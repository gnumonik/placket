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
import PrimTypes 
import qualified Data.Text as T
import qualified Data.Vector as V
import FieldClasses
import Staging 
import Data.Time.Clock 
import Control.Lens hiding (to, from)
import IP4 
import Data.Serialize
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Control.Monad.Trans.Reader

import Network.Pcap (PcapHandle, PktHdr)
import System.IO (Handle)
import System.Random.Mersenne.Pure64
import Control.Concurrent.Async
import Wrappers (as, is)
import qualified Data.ByteString as BS
import Data.Word
import Generics.SOP 
import Data.Maybe
import Serializer (serializeMessage)
import RecordTypes
import Data.Kind (Type)
import Unsafe.Coerce


data WriteMode = Write | Append
-- MachineArrow: A data type that mirrors the structure of Packet Machine definitions. 
-- Not strictly necessary, but useful in developing the parsers.
-- Note: Might be able to simplify the parser if I gave it a traversable instance.
------
data MachineArrow a =
    a :~> MachineArrow a
  | a :~+> [MachineArrow a]
  | a :| () deriving (Eq)

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

instance Show a => Show (MachineArrow a) where
    show (a :~>  bs) = show a <> " ~> " <> show bs
    show (a :~+> bs) = show a <> "[" <> foldr (\x y -> show x <> "\n," <> y ) "" bs
    show (a :| ())   = show a ++ ":|"

infixr 9 :~>
infixr 9 :~+>
infixr 9 :|

-- Move this somewhere 
----

class Checksum a where
    mkChecksum :: V.Vector ProtocolMessage -> a -> a

instance Checksum EthernetFrame where
    mkChecksum _ = id

instance Checksum ARPMessage where
    mkChecksum _ = id 

instance Checksum IP4Packet where
    mkChecksum _ = makeIPv4Checksum

instance Checksum ICMPMessage where
    mkChecksum _ a = let raw = runPut . put $ set (icmpHdr . icmpChecksum) 0 a
                         mySum = calcIPChecksum raw 
                     in  set (icmpHdr . icmpChecksum) mySum a

instance Checksum UDPMessage where
    mkChecksum msg a = 
        let maybeIP4 = foldr (\x y -> if isJust $ as @IP4Packet x then  as @IP4Packet x else y) Nothing msg
        in case maybeIP4 of
            Just ip -> 
                let len = (\x -> fromIntegral x :: Word16) 
                        . BS.length
                        . serializeMessage
                        $ V.reverse 
                        . V.takeWhile (\x -> not $ is @UDPMessage x) 
                        $ V.reverse msg 
                    p :: forall a. Serialize a => a -> BS.ByteString
                    p = runPut . put 
                    pseudoHeader = BS.concat $ map (runPut . put) [p . unIP4 $ ip ^. i4Src 
                                                                  ,p . unIP4 $ ip ^. i4Dst
                                                                  ,p (0 :: Word8)
                                                                  ,p (17 :: Word8) 
                                                                  ,p len ]
                in set uChecksum (calcIPChecksum . (pseudoHeader <>) . p $ set uLen len . set uChecksum 0 $ a) a 
            Nothing -> a

instance Checksum TCPSegment where
    mkChecksum msg a = 
        let maybeIP4 = foldr (\x y -> if isJust $ as @IP4Packet x then  as @IP4Packet x else y) Nothing msg
        in case maybeIP4 of
            Just ip -> 
                let len = (\x -> fromIntegral x :: Word16) 
                        . BS.length
                        . serializeMessage
                        $ V.reverse 
                        . V.takeWhile (\x -> not $ is @TCPSegment x) 
                        $ V.reverse msg 
                    p :: forall a. Serialize a => a -> BS.ByteString
                    p = runPut . put 
                    pseudoHeader = BS.concat $ map (runPut . put) [p . unIP4 $ ip ^. i4Src 
                                                                  ,p . unIP4 $ ip ^. i4Dst
                                                                  ,p (0 :: Word8)
                                                                  ,p (6 :: Word8) 
                                                                  ,p len ]
                in set tChecksum (calcIPChecksum . (pseudoHeader <>) . p $ set tChecksum 0 $ a) a 
            Nothing -> a

instance Checksum DNSMessage where
    mkChecksum _ = id 

instance Checksum MessageContent where
    mkChecksum _ = id 

apChecksum :: V.Vector ProtocolMessage -> V.Vector ProtocolMessage
apChecksum msg = go msg msg
    where
        go :: V.Vector ProtocolMessage -> V.Vector ProtocolMessage -> V.Vector ProtocolMessage 
        go vec acc = if V.null vec 
            then acc
            else let new = gChecksum vec (V.head vec) 
                 in  new `V.cons`  go (V.tail vec) (new `V.cons` V.tail vec) 

gChecksum :: (Generic a, AllN SOP Checksum (Code a), a ~ ProtocolMessage) 
          => V.Vector ProtocolMessage 
          -> ProtocolMessage
          -> ProtocolMessage
gChecksum vec = to . hcmap (Proxy @Checksum) (mapII $ mkChecksum vec) . from 







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


-- Types for expanding the dsl. Not yet implemented. 
-----
data ArgType
    = ArgInt -- Int
    | ArgPType -- T.Text 
    | ArgMaybeInt -- Maybe Int 
    | ArgPrintMode -- PrintMode
    | ArgWriteMode -- WriteMode 
    | ArgQuotedString --T.Text
    | ArgOpticString -- [T.Text]
    | ArgFilePath -- FilePath
    | ArgPSelExp -- ProtocolSelectorExp  
    | ArgProtoBuilder -- ProtocolBuilder
    | ArgFieldBuilder -- FieldBuilder
    | ArgFieldType -- T.Text
    | ArgFieldSelExp -- FieldSelectorExp
    | ArgMsgSelExp --- MsgSelectorExp 
    | ArgMachine -- (MachineArrow T.Text)
    | ArgTime -- Int
    | ArgCases -- (Predicate, MachineArrow T.Text)
    | ArgDouble -- Double
    | ArgMsgSelExpPlus -- MsgSelectorExp
    | ArgPcapLock      -- TMVar () 
    | ArgPcapHandle    -- PcapHandle
    | ArgDumpHandle    -- Handle 
    | ArgDisplayChan  deriving (Show, Eq) -- DisplayChan 


newtype PType =  PType T.Text
newtype QuotedString =  QuotedString T.Text
newtype MachineSchema = MachineSchema (MachineArrow T.Text)
newtype OpticStrings = OpticStrings [T.Text]
newtype Time         = Time Int 
newtype FieldType = FieldType T.Text
newtype MsgSelectorExpPlus = MsgSelectorExpPlus MsgSelectorExp
newtype Cases = Cases (Predicate,MachineArrow T.Text)

type ArgExp = (ArgType, Arg I)

data Arg (f :: k -> Type) where
    Arg :: f t -> Arg f

class CoerceArg a where
    coerceArg :: Proxy a -> Arg I -> a
    coerceArg _ (Arg (I x)) = unsafeCoerce x :: a

instance CoerceArg a 

withArgType :: ArgType -> Arg I -> (forall a. CoerceArg a => ArgType -> a -> b) -> b 
withArgType aType arg f =  case aType of
    ArgInt           -> f ArgInt           $ coerceArg (Proxy @Int) arg 
    ArgPType         -> f ArgPType         $ coerceArg (Proxy @PType) arg 
    ArgMaybeInt      -> f ArgMaybeInt      $ coerceArg (Proxy @PType) arg 
    ArgPrintMode     -> f ArgPrintMode     $ coerceArg (Proxy @PrintMode)  arg
    ArgWriteMode     -> f ArgWriteMode     $ coerceArg (Proxy @WriteMode) arg
    ArgQuotedString  -> f ArgQuotedString  $ coerceArg (Proxy @QuotedString) arg  
    ArgOpticString   -> f ArgOpticString   $ coerceArg (Proxy @OpticStrings) arg
    ArgFilePath      -> f ArgFilePath      $ coerceArg (Proxy @FilePath) arg 
    ArgPSelExp       -> f ArgPSelExp       $ coerceArg (Proxy @ProtocolSelectorExp) arg
    ArgProtoBuilder  -> f ArgProtoBuilder  $ coerceArg (Proxy @ProtocolBuilder) arg
    ArgFieldBuilder  -> f ArgFieldBuilder  $ coerceArg (Proxy @FieldBuilder) arg 
    ArgFieldType     -> f ArgFieldType     $ coerceArg (Proxy @FieldType) arg
    ArgFieldSelExp   -> f ArgFieldSelExp   $ coerceArg (Proxy @FieldSelectorExp)
    ArgMsgSelExp     -> f ArgMsgSelExp     $ coerceArg (Proxy @MsgSelectorExp) arg
    ArgMachine       -> f ArgMachine       $ coerceArg (Proxy @(MachineArrow T.Text)) arg
    ArgTime          -> f ArgTime          $ coerceArg (Proxy @Time) arg
    ArgCases         -> f ArgCases         $ coerceArg (Proxy @Cases) arg
    ArgDouble        -> f ArgDouble        $ coerceArg (Proxy @Double) arg 
    ArgMsgSelExpPlus -> f ArgMsgSelExpPlus $ coerceArg (Proxy @MsgSelectorExpPlus) arg
    ArgPcapLock      -> f ArgPcapLock      $ coerceArg (Proxy @(TMVar ())) arg
    ArgPcapHandle    -> f ArgPcapHandle    $ coerceArg (Proxy @PcapHandle) arg
    ArgDumpHandle    -> f ArgDumpHandle    $ coerceArg (Proxy @Handle) arg
    ArgDisplayChan   -> f ArgDisplayChan   $ coerceArg (Proxy @DisplayChan) arg 
    



newtype MachineArg = MachineArg (forall a. a -> ArgType -> Type) 

data MachineBuilder = MachineBuilder {_mbNm   :: T.Text
                                     ,_mbMch  :: [ArgType] -> [ArgExp] -> Either T.Text PacketMachine
                                     ,_mbArgs :: [ArgType]
                                     ,_mbEnvF :: [Environment -> Environment]}




data Environment = Environment  {_tagCount       :: !TagCount
                                ,_packetMachines :: !(Map Int MachineData)
                                ,_sourceIDs      :: !(Map T.Text SourceData)
                                ,_pcapLock       :: !(TMVar ())
                                ,_pcapHandle     :: !(PcapHandle)
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

