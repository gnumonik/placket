{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module PrimTypes where


import           Classes
import           Control.Lens.TH
import qualified Data.ByteString as BS
import           Data.Typeable
import           Data.Word       (Word16, Word32, Word8)
import           Generics.SOP.TH
import           THDefaults 
import GenericFunctions 
 


   

{-------------------------------
--------------------------------
                                        Protocol ADTs
--------------------------------
--------------------------------}


{---------------
----------------
                                        Ethernet
----------------
----------------}
data EthernetFrame = EthernetFrame   {_eDst     :: !MacAddr
                                     ,_eSrc     :: !MacAddr
                                     ,_eEtherType    :: !Word16} deriving (Eq, Show)

makeLenses ''EthernetFrame
deriveGeneric ''EthernetFrame

{---------------
----------------
                                        ARP
----------------
----------------}

data ARPMessage = ARPMessage {_aHrd :: !Word16
                             ,_aPro :: !Word16
                             ,_aHln :: !Word8
                             ,_aPln :: !Word8
                             ,_aOp  :: !Word16
                             ,_aSha :: !MacAddr
                             ,_aSpa :: !IP4Address
                             ,_aTha :: !MacAddr
                             ,_aTpa :: !IP4Address} deriving (Show, Eq)


makeLenses ''ARPMessage
deriveGeneric ''ARPMessage
{---------------
----------------
                                        IP4
----------------
----------------}


data OptionType = OptionType {_i4CFlag   :: !Word8
                             ,_i4OpClass :: !Word8
                             ,_i4OpNum   :: !Word8} deriving (Show, Eq)
makeLenses ''OptionType
deriveGeneric ''OptionType

data IPFlags = IPFlags {_i4Rsvd :: !Flag
                       ,_i4Df   :: !Flag
                       ,_i4Mf   :: !Flag} deriving (Show, Eq)
makeLenses ''IPFlags
deriveGeneric ''IPFlags 


data Option = Option {_i4OpType   :: !OptionType
                     ,_i4OpLength :: !Word8
                     ,_i4OpData    :: !BS.ByteString} deriving (Show, Eq)
makeLenses ''Option
deriveGeneric ''Option 

data IP4Packet = IP4Packet    {_i4Vers     :: !Word8
                             ,_i4Ihl      :: !Word8
                             ,_i4Tos      :: !Word8
                             ,_i4Tl       :: !Word16
                             ,_i4Iden     :: !Word16
                             ,_i4Flags    :: !IPFlags
                             ,_i4Off      :: !Word16
                             ,_i4Ttl      :: !Word8
                             ,_i4Proto    :: !Word8
                             ,_i4Checksum :: !Word16
                             ,_i4Src      :: !IP4Address
                             ,_i4Dst      :: !IP4Address
                             ,_i4Opts     :: ![Option]} deriving (Show, Eq)


makeLenses ''IP4Packet
deriveGeneric ''IP4Packet
{---------------
----------------
                                        IP6 (Work In Progress)
----------------
----------------}



data IP6MainHeader = IPv6MainHeader {_i6Version        :: !Word8
                                     ,_i6TrafficClass  :: !Word8
                                     ,_i6FlowLabel     :: !Word32
                                     ,_i6PayloadLength :: !Word16
                                     ,_i6NextHeader    :: !Word8
                                     ,_i6HopLimit      :: !Word8
                                     ,_i6SourceAddr    :: !IP6Address
                                     ,_i6DestAddr      :: !IP6Address } deriving (Show, Eq)
makeLenses ''IP6MainHeader


{---------------
----------------
                                        ICMP4
----------------
----------------}

data ICMPHeader = ICMPHeader {_icmpType     :: !Word8
                             ,_icmpCode     :: !Word8
                             ,_icmpChecksum :: !Word16} deriving (Show, Eq)
makeLenses ''ICMPHeader 
deriveGeneric ''ICMPHeader 

data DestUnreachable = DestUnreachable {_duUnused    :: !Word32
                                       ,_duDgPortion :: !BS.ByteString} deriving (Show, Eq)
makeLenses  ''DestUnreachable
deriveGeneric ''DestUnreachable

data SourceQuench = SourceQuench {_sqUnused    :: !Word32
                                 ,_sqDgPortion:: !BS.ByteString} deriving (Show, Eq)
makeLenses ''SourceQuench
deriveGeneric ''SourceQuench

data  TimeExceeded = TimeExceeded {_teUnused    :: !Word32
                                  ,_teDgPortion :: !BS.ByteString} deriving (Show, Eq)
makeLenses ''TimeExceeded
deriveGeneric ''TimeExceeded

data Redirect =      Redirect {_rdAddr      :: !IP4Address
                              ,_rdDgPortion :: !BS.ByteString} deriving (Show, Eq)
makeLenses  ''Redirect
deriveGeneric ''Redirect

data ParamProblem = ParamProblem {_ppPtr      :: !Word8
                                 ,_ppUnused    :: !Word24
                                 ,_ppDgPortion :: !BS.ByteString} deriving (Show, Eq)
makeLenses  ''ParamProblem
deriveGeneric ''ParamProblem

data EchoRequest = EchoRequest {_erqId     :: !Word16
                               ,_erqSeqNum :: !Word16
                               ,_erqData   :: !Word16} deriving (Show, Eq)
makeLenses ''EchoRequest
deriveGeneric ''EchoRequest 


data EchoReply = EchoReply {_erpId     :: !Word16
                          ,_erpSeqNum :: !Word16
                          ,_erpData   :: !Word16} deriving (Show, Eq)
makeLenses  ''EchoReply
deriveGeneric ''EchoReply


data TimeStampRequest = TimeStampRequest {_trqId     :: !Word16
                                         ,_trqSeqNum :: !Word16
                                         ,_trqOrg    :: !Word32
                                         ,_trqRcv    :: !Word32
                                         ,_trqTrs    :: !Word32} deriving (Show, Eq)
makeLenses ''TimeStampRequest
deriveGeneric ''TimeStampRequest

data TimeStampReply = TimeStampReply  {_trpId     :: !Word16
                                      ,_trpSeqNum :: !Word16
                                      ,_trpOrg    :: !Word32
                                      ,_trpRcv    :: !Word32
                                      ,_trpTrs    :: !Word32} deriving (Show, Eq)
makeLenses  ''TimeStampReply
deriveGeneric ''TimeStampReply

data RouterAdvertisement = RouterAdvertisement {_raNumAddrs        :: !Word8
                                               ,_raEntrySize   :: !Word8
                                               ,_raLifetime        :: !Word16
                                               ,_raEntries         :: !BS.ByteString} deriving (Show, Eq)
makeLenses ''RouterAdvertisement
deriveGeneric ''RouterAdvertisement

data RouterSolicitation = RouterSolicitation  {_rsReserved :: !Word32} deriving (Show, Eq)
makeLenses ''RouterSolicitation
deriveGeneric ''RouterSolicitation


data TraceRoute = TraceRoute {_trId          :: !Word16
                             ,_trUnused      :: !Word16
                             ,_trOutHop      :: !Word16
                             ,_trRetHop      :: !Word16
                             ,_trOutSpd      :: !Word32
                             ,_trOutMTU      :: !Word32} deriving (Show, Eq)
makeLenses ''TraceRoute
deriveGeneric ''TraceRoute

data AddressMaskRequest = AddressMaskRequest  {_amqId     :: !Word16
                                              ,_amqSeqNum :: !Word16
                                              ,_amqMask   :: !Word32 } deriving (Show, Eq)
makeLenses ''AddressMaskRequest
deriveGeneric ''AddressMaskRequest

data AddressMaskReply = AddressMaskReply   {_ampId     :: !Word16
                                           ,_ampSeqNum :: !Word16
                                           ,_ampMask   :: !Word32 } deriving (Show, Eq)
makeLenses ''AddressMaskReply
deriveGeneric ''AddressMaskReply

data ICMPData 
    = DU   DestUnreachable
    | SQ   SourceQuench
    | TE   TimeExceeded
    | RD   Redirect
    | PP   ParamProblem
    | ERQ  EchoRequest
    | ERP  EchoReply
    | TSRQ TimeStampRequest
    | TSRP TimeStampReply
    | RA   RouterAdvertisement
    | RS   RouterSolicitation
    | TR   TraceRoute
    | AMRQ AddressMaskRequest
    | AMRP AddressMaskReply deriving (Show, Eq)
makePrisms ''ICMPData
deriveGeneric ''ICMPData 

data ICMPMessage = ICMPMessage {_icmpHdr :: ICMPHeader
                               ,_icmpData :: ICMPData} deriving (Show, Eq)
makeLenses ''ICMPMessage
deriveGeneric ''ICMPMessage 


{---------------
----------------
                                        UDP
----------------
----------------}

data UDPMessage   = UDPMessage  {_uSrc      :: !Word16
                                ,_uDst      :: !Word16
                                ,_uLen      :: !Word16
                                ,_uChecksum :: !Word16} deriving (Show, Eq)

makeLenses ''UDPMessage
deriveGeneric ''UDPMessage
{---------------
----------------
                                        TCP
----------------
----------------}

data TCPOption = TCPOption       {_tOpKind :: !Word8
                                 ,_tOpLen  :: !Word8
                                 ,_tOpData :: !BS.ByteString} deriving (Eq, Show)
makeLenses ''TCPOption
deriveGeneric ''TCPOption
data ControlFlags = ControlFlags {_tUrg :: !Flag
                                 ,_tAck :: !Flag
                                 ,_tPsh :: !Flag
                                 ,_tRst :: !Flag
                                 ,_tSyn :: !Flag
                                 ,_tFin :: !Flag } deriving (Eq, Show)
makeLenses ''ControlFlags

data TCPSegment = TCPSegment {_tSrc      :: !Word16
                             ,_tDst      :: !Word16
                             ,_tSeqNum   :: !Word32
                             ,_tAckNum   :: !Word32
                             ,_tOffset   :: !Word16
                             ,_tFlags    :: !ControlFlags
                             ,_tWin      :: !Word16
                             ,_tChecksum :: !Word16
                             ,_tUrgPntr  :: !Word16
                             ,_tOpts     :: ![TCPOption]} deriving (Eq, Show)
makeLenses ''TCPSegment
deriveGeneric ''TCPSegment
{---------------
----------------
                                        DNS
----------------
----------------}

data DNSHeader = DNSHeader {_dhId      :: !Word16
                           ,_dhQr      :: !Flag
                           ,_dhOp      :: !Word8
                           ,_dhAa      :: !Flag
                           ,_dhTc      :: !Flag
                           ,_dhRd      :: !Flag
                           ,_dhRa      :: !Flag
                           ,_dhZ       :: !Word8
                           ,_dhRCode   :: !Word8
                           ,_dhQdCount :: !Word16
                           ,_dhAnCount :: !Word16
                           ,_dhNsCount :: !Word16
                           ,_dhArCount :: !Word16} deriving (Show, Eq)
makeLenses ''DNSHeader
deriveGeneric ''DNSHeader


data DNSQuestion = DNSQuestion {_qName  :: !DNSName
                               ,_qType  :: !Word16
                               ,_qClass :: !Word16} deriving (Show, Eq)
makeLenses ''DNSQuestion
deriveGeneric ''DNSQuestion

data  RR_SOA = RR_SOA {_soaMName   :: !DNSName
                      ,_soaRName   :: !DNSName
                      ,_soaSerial  :: !Word32
                      ,_soaRefresh :: !Word32
                      ,_soaRetry   :: !Word32
                      ,_soaExp     :: !Word32
                      ,_soaMin     :: !Word32} deriving (Show, Eq)
makeLenses ''RR_SOA
deriveGeneric ''RR_SOA

data RR_MX     = RR_MX {_mxPref :: !Word16
                       ,_mxExch :: !DNSName} deriving (Show, Eq)
makeLenses ''RR_MX
deriveGeneric ''RR_MX

newtype RR_TXT = RR_TXT{_txtData :: BS.ByteString} deriving (Show, Eq)
makeLenses ''RR_TXT
deriveGeneric ''RR_TXT

newtype RR_NS    = RD_NS {_rrNs :: DNSName} deriving (Show, Eq)

newtype RR_CNAME = RD_CNAME {_rrCName :: DNSName} deriving (Show, Eq)

newtype RR_PTR   = RD_PTR {_rrPtr :: DNSName} deriving (Show, Eq)

concat <$> mapM makeLenses [''RR_NS, ''RR_CNAME, ''RR_PTR]
concat <$> mapM deriveGeneric [''RR_NS, ''RR_CNAME, ''RR_PTR]

data RData = A     !IP4Address
           | NS    !RR_NS
           | CName !RR_CNAME
           | SOA   !RR_SOA
           | PTR   !RR_PTR
           | MX    !RR_MX
           | TXT   !BS.ByteString deriving (Show, Eq)
makeClassyPrisms ''RData
deriveGeneric ''RData

data DNSRR = DNSRR  {_rrName  :: !DNSName
                    ,_rrType  :: !Word16
                    ,_rrClass :: !Word16
                    ,_rrTtl   :: !Word32
                    ,_rrLen   :: !Word16
                    ,_rrData  :: !RData} deriving (Show, Eq)
makeLenses    ''DNSRR
deriveGeneric ''DNSRR

newtype DNSAnswer = DNSAns  {_dAnswer :: DNSRR} deriving (Show, Eq)
newtype DNSAuth   = DNSAuth {_dAuth   :: DNSRR} deriving (Show, Eq)
newtype DNSAdd    = DNSAdd  {_dAdd    :: DNSRR} deriving (Show, Eq)

concat <$> mapM makeLenses [''DNSAnswer, ''DNSAuth, ''DNSAdd] 

data DNSMessage = DNSMessage {_dnsHdr      :: !DNSHeader
                             ,_dnsQuestion :: ![DNSQuestion]
                             ,_dnsAnswer   :: ![DNSAnswer]
                             ,_dnsAuth     :: ![DNSAuth]
                             ,_dnsAdd      :: ![DNSAdd]} deriving (Show, Eq)
makeLenses ''DNSMessage
deriveGeneric ''DNSMessage
-- want syntax like: ICMP:DU w/ hdr.type=8 unused=3

-- or for non-top-level: DNS 


concat <$> mapM deriveDefault [''EthernetFrame,''ARPMessage,''IP4Packet,''ICMPMessage, ''TCPSegment, ''UDPMessage, ''DNSMessage]