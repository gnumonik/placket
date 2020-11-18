{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
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
{-# LANGUAGE UndecidableSuperClasses, OverloadedStrings   #-}

module Wrappers where

import           Classes
import           Control.Lens    hiding (Contains, (:>), transform)
import           Control.Lens.TH
import           Data.Default
import           Data.Proxy
import qualified Data.Text as T 
import           Generics.SOP
import           Generics.SOP.TH
import           LibTypes
import           PrimTypes
import           THWrappers
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import RecordTypes (OpticStrs)
import Control.Monad
import THRecords


--mkProtocolInstances

makeClassyPrisms ''ProtocolMessage

deriveGeneric ''ProtocolMessage

mkProtocolWrappers

instance Possibly  DestUnreachable ICMPData where
    isA _ (DU x) = Just x
    isA _  _ = Nothing
    fromA x = DU x

instance Possibly SourceQuench ICMPData where
    isA _ (SQ x) = Just x
    isA _  _ = Nothing
    fromA x = SQ x

instance Possibly TimeExceeded ICMPData where
    isA _ (TE x) = Just x 
    isA _  _ = Nothing
    fromA x = TE x 

instance Possibly Redirect ICMPData where
    isA _ (RD x) = Just x 
    isA _  _ = Nothing
    fromA x = RD x 

instance Possibly ParamProblem ICMPData where
    isA _ (PP x) = Just x 
    isA _  _ = Nothing
    fromA x = PP x 

instance Possibly EchoRequest ICMPData where
    isA _ (ERQ x) = Just x 
    isA _  _ = Nothing
    fromA x = ERQ x 

instance Possibly EchoReply ICMPData where
    isA _ (ERP x) = Just x 
    isA _  _ = Nothing
    fromA x = ERP x

instance Possibly TimeStampRequest ICMPData where
    isA _ (TSRQ x) = Just x 
    isA _  _ = Nothing
    fromA x = TSRQ x

instance Possibly TimeStampReply ICMPData where
    isA _ (TSRP x) = Just x 
    isA _  _ = Nothing
    fromA x = TSRP x   

instance Possibly RouterAdvertisement ICMPData where
    isA _ (RA x) = Just x 
    isA _  _ = Nothing
    fromA x = RA x 

instance Possibly RouterSolicitation ICMPData where
    isA _ (RS x) = Just x 
    isA _  _ = Nothing
    fromA x = RS x 

instance Possibly TraceRoute ICMPData where
    isA _ (TR x) = Just x 
    isA _  _ = Nothing
    fromA x = TR x

instance Possibly AddressMaskRequest ICMPData where
    isA _ (AMRQ x) = Just x 
    isA _  _ = Nothing
    fromA x = AMRQ x 

instance Possibly AddressMaskReply ICMPData where
    isA _ (AMRP x) = Just x 
    isA _  _ = Nothing
    fromA x = AMRP x  

instance Possibly IP4Address RData where
    isA _ (A x) = Just x 
    isA _ _     = Nothing
    fromA x = A x 

instance Possibly RR_NS RData where
    isA _ (NS x) = Just x 
    isA _ _     = Nothing
    fromA x = NS x 

instance Possibly RR_CNAME RData where
    isA _ (CName x) = Just x 
    isA _ _     = Nothing
    fromA x = CName x 

instance Possibly RR_SOA RData where
    isA _ (SOA x) = Just x
    isA _ _ = Nothing
    fromA x = SOA x

instance Possibly RR_PTR RData where
    isA _ (PTR x) = Just x
    isA _ _ = Nothing
    fromA x = PTR x

instance Possibly RR_MX RData where
    isA _ (MX x) = Just x
    isA _ _ = Nothing
    fromA x = MX x

instance Possibly BS.ByteString RData where
    isA _ (TXT x) = Just x 
    isA _ _ = Nothing
    fromA x = TXT x 

conjure :: forall b a. (HasAlias (FromAlias b) b, WrapProtocol (FromAlias b), Default (FromAlias b), FromAlias b ~ a) => b -> ProtocolMessage
conjure _ = wrapP $ (def :: FromAlias b)

liftB :: (WrapProtocol (FromAlias b), HasAlias (FromAlias b) b, Default (FromAlias b)) => b -> Builder ProtocolMessage
liftB b = V.singleton $ conjure b 


as :: forall choice union. Possibly choice union => union -> Maybe choice
as protocolmessage = isA (Proxy @choice) protocolmessage

with :: forall choice union b. Possibly choice union => union ->  (union -> Maybe choice) ->  (choice -> b) -> Maybe b
with pr _as  f = f <$> (_as pr)

is :: forall choice union. Possibly choice union => union ->  Bool
is p  = case isA (Proxy @choice) p of
    Just _  -> True
    Nothing -> False



getField' :: forall a. (Primitive a) => Proxy a -> Either T.Text (a -> [Value])
getField' _ = Right $ \a ->  [(packVal @a a)]

getField :: forall a. (StringyLens a, Possibly a ProtocolMessage) => Proxy a -> OpticStrs ->  Either T.Text (ProtocolMessage -> Maybe [(Value)] )
getField prox fs =  case applyTo prox fs (getField' ) of
    Right f   -> Right $ \p -> (f <=< as @a) p
    Left  err -> Left err 

unliftedGetField :: forall a. (StringyLens a) => Proxy a -> OpticStrs ->  Either T.Text (a -> Maybe [(Value)] )
unliftedGetField prox fs = applyTo prox fs (getField') 

proxyA' :: forall a b. Lens' a b -> Proxy a
proxyA' _ = Proxy 



instance StringyLens MessageContent where
    update prox ys f = case ys of
        [] -> case f prox of 
            Right g -> Right $ \m -> map ($ m) g
            Left err -> Left err
        _  -> Left $ "Error: Invalid record selector!"

    applyTo prox ys f = case ys of
        [] -> case f prox of
            Right g -> Right $ \m -> pure $ g m
            Left err -> Left err  
        _  -> Left $ "Error: Invalid record selector!"

deriveStringyLens ''EthernetFrame
deriveStringyLens ''ARPMessage 
deriveStringyLens ''IP4Packet
deriveStringyLens ''UDPMessage
deriveStringyLens ''TCPSegment
deriveStringyLens ''ICMPMessage
deriveStringyLens ''DNSMessage 


{--
instance StringyLens EthernetFrame where
    update _ strs f
        = case strs of
            ("dst" : ys) -> (normalRecUpdate eDst) f
            ("src" : ys) -> (normalRecUpdate eSrc) f
            ("etherType" : ys) -> (normalRecUpdate eType) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["dst"] -> (applyToPrimNormalRec f) eDst
            ["src"] -> (applyToPrimNormalRec f) eSrc
            ["etherType"] -> (applyToPrimNormalRec f) eType
            _ -> Left "Error: Invalid record selector!"

instance StringyLens IPFlags where
    update _ strs f
        = case strs of
            ("rsvd" : ys) -> (normalRecUpdate i4Rsvd) f
            ("df" : ys) -> (normalRecUpdate i4Df) f
            ("mf" : ys) -> (normalRecUpdate i4Mf) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["rsvd"] -> (applyToPrimNormalRec f) i4Rsvd
            ["df"] -> (applyToPrimNormalRec f) i4Df
            ["mf"] -> (applyToPrimNormalRec f) i4Mf
            _ -> Left "Error: Invalid record selector!"

instance StringyLens OptionType where
    update _ strs f
        = case strs of
            ("cFlag" : ys) -> (normalRecUpdate i4CFlag) f
            ("opClass" : ys) -> (normalRecUpdate i4OpClass) f
            ("opNum" : ys) -> (normalRecUpdate i4OpNum) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["cFlag"] -> (applyToPrimNormalRec f) i4CFlag
            ["opClass"] -> (applyToPrimNormalRec f) i4OpClass
            ["opNum"] -> (applyToPrimNormalRec f) i4OpNum
            _ -> Left "Error: Invalid record selector!"

instance StringyLens Option where
    update _ strs f
        = case strs of
            ("opType" : ys) -> ((nonBottomRecUpdate ys) f) i4OpType
            ("opLength" : ys) -> (normalRecUpdate i4OpLength) f
            ("opData" : ys) -> (normalRecUpdate i4OData) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ("opType" : ys) -> ((applyToPrimNonBottom ys) f) i4OpType
            ["opLength"] -> (applyToPrimNormalRec f) i4OpLength
            ["opData"] -> (applyToPrimNormalRec f) i4OData
            _ -> Left "Error: Invalid record selector!"


instance StringyLens IP4Packet where
    update _ strs f
        = case strs of
            ("vers" : ys) -> (normalRecUpdate i4Vers) f
            ("ihl" : ys) -> (normalRecUpdate i4Ihl) f
            ("tos" : ys) -> (normalRecUpdate i4Tos) f
            ("tl" : ys) -> (normalRecUpdate i4Tl) f
            ("id" : ys) -> (normalRecUpdate i4Iden) f
            ("flags" : ys) -> ((nonBottomRecUpdate ys) f) i4Flags
            ("off" : ys) -> (normalRecUpdate i4Off) f
            ("ttl" : ys) -> (normalRecUpdate i4Ttl) f
            ("protocol" : ys) -> (normalRecUpdate i4Protocol) f
            ("checksum" : ys) -> (normalRecUpdate i4Checksum) f
            ("src" : ys) -> (normalRecUpdate i4Src) f
            ("dst" : ys) -> (normalRecUpdate i4Dst) f
            ("opts" : ys) -> ((listUpdate' (Proxy @IP4Packet) ys) f) i4Opts
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["vers"] -> (applyToPrimNormalRec f) i4Vers
            ["ihl"] -> (applyToPrimNormalRec f) i4Ihl
            ["tos"] -> (applyToPrimNormalRec f) i4Tos
            ["tl"] -> (applyToPrimNormalRec f) i4Tl
            ["id"] -> (applyToPrimNormalRec f) i4Iden
            ("flags" : ys) -> ((applyToPrimNonBottom ys) f) i4Flags
            ["off"] -> (applyToPrimNormalRec f) i4Off
            ["ttl"] -> (applyToPrimNormalRec f) i4Ttl
            ["protocol"] -> (applyToPrimNormalRec f) i4Protocol
            ["checksum"] -> (applyToPrimNormalRec f) i4Checksum
            ["src"] -> (applyToPrimNormalRec f) i4Src
            ["dst"] -> (applyToPrimNormalRec f) i4Dst
            ("opts" : ys) -> ((listApplyTo' ys) f) i4Opts
            _ -> Left "Error: Invalid record selector!"

instance StringyLens ARPMessage where
    update _ strs f
        = case strs of
            ("hrd" : ys) -> (normalRecUpdate aHrd) f
            ("pro" : ys) -> (normalRecUpdate aPro) f
            ("hln" : ys) -> (normalRecUpdate aHln) f
            ("pln" : ys) -> (normalRecUpdate aPln) f
            ("op" : ys) -> (normalRecUpdate aOp) f
            ("sha" : ys) -> (normalRecUpdate aSha) f
            ("spa" : ys) -> (normalRecUpdate aSpa) f
            ("tha" : ys) -> (normalRecUpdate aTha) f
            ("tpa" : ys) -> (normalRecUpdate aTpa) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["hrd"] -> (applyToPrimNormalRec f) aHrd
            ["pro"] -> (applyToPrimNormalRec f) aPro
            ["hln"] -> (applyToPrimNormalRec f) aHln
            ["pln"] -> (applyToPrimNormalRec f) aPln
            ["op"] -> (applyToPrimNormalRec f) aOp
            ["sha"] -> (applyToPrimNormalRec f) aSha
            ["spa"] -> (applyToPrimNormalRec f) aSpa
            ["tha"] -> (applyToPrimNormalRec f) aTha
            ["tpa"] -> (applyToPrimNormalRec f) aTpa
            _ -> Left "Error: Invalid record selector!"
------
-- DNS            
------
instance StringyLens DNSHeader where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate dhID) f
             ("qr" : ys) -> (normalRecUpdate dhQR) f
             ("op" : ys) -> (normalRecUpdate dhOP) f
             ("aa" : ys) -> (normalRecUpdate dhAA) f
             ("tc" : ys) -> (normalRecUpdate dhTC) f
             ("rd" : ys) -> (normalRecUpdate dhRD) f
             ("ra" : ys) -> (normalRecUpdate dhRA) f
             ("z" : ys) -> (normalRecUpdate dhZ) f
             ("rCode" : ys) -> (normalRecUpdate dhRCode) f
             ("qdCount" : ys) -> (normalRecUpdate dhQDCount) f
             ("anCount" : ys) -> (normalRecUpdate dhANCount) f
             ("nsCount" : ys) -> (normalRecUpdate dhNSCount) f
             ("arCount" : ys) -> (normalRecUpdate dhARCount) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) dhID
             ["qr"] -> (applyToPrimNormalRec f) dhQR
             ["op"] -> (applyToPrimNormalRec f) dhOP
             ["aa"] -> (applyToPrimNormalRec f) dhAA
             ["tc"] -> (applyToPrimNormalRec f) dhTC
             ["rd"] -> (applyToPrimNormalRec f) dhRD
             ["ra"] -> (applyToPrimNormalRec f) dhRA
             ["z"] -> (applyToPrimNormalRec f) dhZ
             ["rCode"] -> (applyToPrimNormalRec f) dhRCode
             ["qdCount"] -> (applyToPrimNormalRec f) dhQDCount
             ["anCount"] -> (applyToPrimNormalRec f) dhANCount
             ["nsCount"] -> (applyToPrimNormalRec f) dhNSCount
             ["arCount"] -> (applyToPrimNormalRec f) dhARCount
             _ -> Left "Error: Invalid record selector!"

instance StringyLens DNSQuestion where
       update _ strs f
         = case strs of
             ("name" : ys) -> (normalRecUpdate qName) f
             ("type" : ys) -> (normalRecUpdate qType) f
             ("class" : ys) -> (normalRecUpdate qClass) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["name"] -> (applyToPrimNormalRec f) qName
             ["type"] -> (applyToPrimNormalRec f) qType
             ["class"] -> (applyToPrimNormalRec f) qClass
             _ -> Left "Error: Invalid record selector!"

instance StringyLens RR_NS where
       update _ strs f
         = case strs of
             [] -> (normalRecUpdate ns) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             [] -> (applyToPrimNormalRec f) ns
             _ -> Left "Error: Invalid record selector!"

instance StringyLens RR_CNAME where
       update _ strs f
         = case strs of
             [] -> (normalRecUpdate cname) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             [] -> (applyToPrimNormalRec f) cname
             _ -> Left "Error: Invalid record selector!"

instance StringyLens RR_SOA where
       update _ strs f
         = case strs of
             ("mName" : ys) -> (normalRecUpdate soaMName) f
             ("rName" : ys) -> (normalRecUpdate soaRName) f
             ("serial" : ys) -> (normalRecUpdate soaSerial) f
             ("refresh" : ys) -> (normalRecUpdate soaRefresh) f
             ("retry" : ys) -> (normalRecUpdate soaRetry) f
             ("exp" : ys) -> (normalRecUpdate soaExp) f
             ("min" : ys) -> (normalRecUpdate soaMin) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["mName"] -> (applyToPrimNormalRec f) soaMName
             ["rName"] -> (applyToPrimNormalRec f) soaRName
             ["serial"] -> (applyToPrimNormalRec f) soaSerial
             ["refresh"] -> (applyToPrimNormalRec f) soaRefresh
             ["retry"] -> (applyToPrimNormalRec f) soaRetry
             ["exp"] -> (applyToPrimNormalRec f) soaExp
             ["min"] -> (applyToPrimNormalRec f) soaMin
             _ -> Left "Error: Invalid record selector!"

instance StringyLens RR_PTR where
       update _ strs f
         = case strs of
             [] -> (normalRecUpdate ptr) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             [] -> (applyToPrimNormalRec f) ptr
             _ -> Left "Error: Invalid record selector!"

instance StringyLens RR_MX where
       update _ strs f
         = case strs of
             ("pref" : ys) -> (normalRecUpdate mxPref) f
             ("exch" : ys) -> (normalRecUpdate mxExch) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["pref"] -> (applyToPrimNormalRec f) mxPref
             ["exch"] -> (applyToPrimNormalRec f) mxExch
             _ -> Left "Error: Invalid record selector!"

instance StringyLens RData where
       update _ strs f
         = case strs of
             ["a"] -> (sumNormalUpdate f) _A
             ("ns" : ys) -> ((sumNonBottomUpdate ys) f) _NS
             ("cname" : ys) -> ((sumNonBottomUpdate ys) f) _CName
             ("soa" : ys) -> ((sumNonBottomUpdate ys) f) _SOA
             ("ptr" : ys) -> ((sumNonBottomUpdate ys) f) _PTR
             ("mx" : ys) -> ((sumNonBottomUpdate ys) f) _MX
             ["txt"] -> (sumNormalUpdate f) _TXT
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["a"] -> (sumNormalApplyTo _A) f
             ("ns" : ys) -> ((sumNonBottomApplyTo ys) _NS) f
             ("cname" : ys) -> ((sumNonBottomApplyTo ys) _CName) f
             ("soa" : ys) -> ((sumNonBottomApplyTo ys) _SOA) f
             ("ptr" : ys) -> ((sumNonBottomApplyTo ys) _PTR) f
             ("mx" : ys) -> ((sumNonBottomApplyTo ys) _MX) f
             ["txt"] -> (sumNormalApplyTo _TXT) f
             _ -> Left "Error: Invalid record selector!"

instance StringyLens DNSRR where
       update _ strs f
         = case strs of
             ("name" : ys) -> (normalRecUpdate rrName) f
             ("type" : ys) -> (normalRecUpdate rrType) f
             ("class" : ys) -> (normalRecUpdate rrClass) f
             ("ttl" : ys) -> (normalRecUpdate rrTTL) f
             ("len" : ys) -> (normalRecUpdate rrLen) f
             ("data" : ys) -> ((nonBottomRecUpdate ys) f) rrData
             ("data@A" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @IP4Address) d 
             ("data@NS" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @RR_NS) d 
             ("data@CNAME" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @RR_CNAME) d 
             ("data@SOA" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @RR_SOA) d 
             ("data@PTR" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @RR_PTR) d 
             ("data@MX" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @RR_MX) d 
             ("data@TXT" : ys) -> case ((nonBottomRecUpdate ys) f) rrData of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans rrData (Proxy @BS.ByteString) d 
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["name"] -> (applyToPrimNormalRec f) rrName
             ["type"] -> (applyToPrimNormalRec f) rrType
             ["class"] -> (applyToPrimNormalRec f) rrClass
             ["tTL"] -> (applyToPrimNormalRec f) rrTTL
             ["len"] -> (applyToPrimNormalRec f) rrLen
             ("data" : ys) -> ((applyToPrimNonBottom ys) f) rrData
             _ -> Left "Error: Invalid record selector!"

instance StringyLens DNSAnswer where
       update _ strs f
         = case strs of
             ("answer" : ys) -> ((nonBottomRecUpdate ys) f) dAnswer
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("answer" : ys) -> ((applyToPrimNonBottom ys) f) dAnswer
             _ -> Left "Error: Invalid record selector!"

instance StringyLens DNSAuth where
       update _ strs f
         = case strs of
             ("auth" : ys) -> ((nonBottomRecUpdate ys) f) dAuth
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("auth" : ys) -> ((applyToPrimNonBottom ys) f) dAuth
             _ -> Left "Error: Invalid record selector!"

instance StringyLens DNSAdd where
       update _ strs f
         = case strs of
             ("add" : ys) -> ((nonBottomRecUpdate ys) f) dAdd
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("add" : ys) -> ((applyToPrimNonBottom ys) f) dAdd
             _ -> Left "Error: Invalid record selector!"

instance StringyLens DNSMessage where
       update _ strs f
         = case strs of
             ("hdr" : ys) -> ((nonBottomRecUpdate ys) f) dnsHDR
             --("question" : ys) -> ((nonBottomRecUpdate ys) f) dnsQuestion
             --("answer" : ys) -> ((nonBottomRecUpdate ys) f) dnsAnswer
             ---("auth" : ys) -> ((nonBottomRecUpdate ys) f) dnsAuth
             --("add" : ys) -> ((nonBottomRecUpdate ys) f) dnsAdd
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("hdr" : ys) -> ((applyToPrimNonBottom ys) f) dnsHDR
             --("question" : ys) -> ((applyToPrimNonBottom strs) f) dnsQuestion
             --("answer" : ys) -> ((applyToPrimNonBottom strs) f) dnsAnswer
             --("auth" : ys) -> ((applyToPrimNonBottom strs) f) dnsAuth
             --("add" : ys) -> ((applyToPrimNonBottom strs) f) dnsAdd
             _ -> Left "Error: Invalid record selector!"
----
-- ICMPv4
----
instance StringyLens ICMPHeader where
    update _ strs f
        = case strs of
            ("type" : ys) -> (normalRecUpdate icmpType) f
            ("code" : ys) -> (normalRecUpdate icmpCode) f
            ("checksum" : ys) -> (normalRecUpdate icmpChecksum) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["type"] -> (applyToPrimNormalRec f) icmpType
            ["code"] -> (applyToPrimNormalRec f) icmpCode
            ["checksum"] -> (applyToPrimNormalRec f) icmpChecksum
            _ -> Left "Error: Invalid record selector!"

instance StringyLens DestUnreachable where
    update _ strs f
        = case strs of
            ("unused" : ys) -> (normalRecUpdate duUnused) f
            ("dgPortion" : ys) -> (normalRecUpdate duDgPortion) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["unused"] -> (applyToPrimNormalRec f) duUnused
            ["dgPortion"] -> (applyToPrimNormalRec f) duDgPortion
            _ -> Left "Error: Invalid record selector!"

instance StringyLens SourceQuench where
    update _ strs f
        = case strs of
            ("unused" : ys) -> (normalRecUpdate sqUnused) f
            ("dgPortion" : ys) -> (normalRecUpdate sqDgPortion) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["unused"] -> (applyToPrimNormalRec f) sqUnused
            ["dgPortion"] -> (applyToPrimNormalRec f) sqDgPortion
            _ -> Left "Error: Invalid record selector!"

instance StringyLens TimeExceeded where
    update _ strs f
        = case strs of
            ("unused" : ys) -> (normalRecUpdate teUnused) f
            ("dgPortion" : ys) -> (normalRecUpdate teDgPortion) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["unused"] -> (applyToPrimNormalRec f) teUnused
            ["dgPortion"] -> (applyToPrimNormalRec f) teDgPortion
            _ -> Left "Error: Invalid record selector!"

instance StringyLens Redirect where
    update _ strs f
        = case strs of
            ("addr" : ys) -> (normalRecUpdate rdAddr) f
            ("dgPortion" : ys) -> (normalRecUpdate rdDgPortion) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["addr"] -> (applyToPrimNormalRec f) rdAddr
            ["dgPortion"] -> (applyToPrimNormalRec f) rdDgPortion
            _ -> Left "Error: Invalid record selector!"

instance StringyLens ParamProblem where
    update _ (str:[]) f
        = case str of
            "ptr"      -> (normalRecUpdate ppPntr) f
            "unused"    -> (normalRecUpdate ppUnused) f
            "dgPortion" -> (normalRecUpdate ppDgPortion) f
            _ -> Left $ "Error: Invalid record selector: " <> str
    update _ [] _ = Left "Error! Empty record selector."
    update _ x _  = Left $ "Error! Field mismatch in " <> (T.pack . show $ x)
    applyTo _ strs f
        = case strs of
            ["ptr"] -> (applyToPrimNormalRec f) ppPntr
            ["unused"] -> (applyToPrimNormalRec f) ppUnused
            ["dgPortion"] -> (applyToPrimNormalRec f) ppDgPortion
            _ -> Left "Error: Invalid record selector!"

instance StringyLens EchoRequest where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate erqID) f
            ("seqNum" : ys) -> (normalRecUpdate erqSeqNum) f
            ("data" : ys) -> (normalRecUpdate erqData) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) erqID
            ["seqNum"] -> (applyToPrimNormalRec f) erqSeqNum
            ["data"] -> (applyToPrimNormalRec f) erqData
            _ -> Left "Error: Invalid record selector!"

instance StringyLens EchoReply where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate erpID) f
            ("seqNum" : ys) -> (normalRecUpdate erpSeqNum) f
            ("data" : ys) -> (normalRecUpdate erpData) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) erpID
            ["seqNum"] -> (applyToPrimNormalRec f) erpSeqNum
            ["data"] -> (applyToPrimNormalRec f) erpData
            _ -> Left "Error: Invalid record selector!"

instance StringyLens TimeStampRequest where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate trqID) f
            ("seqNum" : ys) -> (normalRecUpdate trqSeqNum) f
            ("org" : ys) -> (normalRecUpdate trqOrg) f
            ("rcv" : ys) -> (normalRecUpdate trqRcv) f
            ("trs" : ys) -> (normalRecUpdate trqTrs) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) trqID
            ["seqNum"] -> (applyToPrimNormalRec f) trqSeqNum
            ["org"] -> (applyToPrimNormalRec f) trqOrg
            ["rcv"] -> (applyToPrimNormalRec f) trqRcv
            ["trs"] -> (applyToPrimNormalRec f) trqTrs
            _ -> Left "Error: Invalid record selector!"

instance StringyLens TimeStampReply where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate trpID) f
            ("seqNum" : ys) -> (normalRecUpdate trpSeqNum) f
            ("org" : ys) -> (normalRecUpdate trpOrg) f
            ("rcv" : ys) -> (normalRecUpdate trpRcv) f
            ("trs" : ys) -> (normalRecUpdate trpTrs) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) trpID
            ["seqNum"] -> (applyToPrimNormalRec f) trpSeqNum
            ["org"] -> (applyToPrimNormalRec f) trpOrg
            ["rcv"] -> (applyToPrimNormalRec f) trpRcv
            ["trs"] -> (applyToPrimNormalRec f) trpTrs
            _ -> Left "Error: Invalid record selector!"

instance StringyLens RouterAdvertisement where
    update _ strs f
        = case strs of
            ("numAddrs" : ys) -> (normalRecUpdate raNumAddrs) f
            ("addrEntrySize" : ys) -> (normalRecUpdate raAddrEntrySize) f
            ("lifetime" : ys) -> (normalRecUpdate raLifetime) f
            ("entries" : ys) -> (normalRecUpdate raEntries) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["numAddrs"] -> (applyToPrimNormalRec f) raNumAddrs
            ["addrEntrySize"] -> (applyToPrimNormalRec f) raAddrEntrySize
            ["lifetime"] -> (applyToPrimNormalRec f) raLifetime
            ["entries"] -> (applyToPrimNormalRec f) raEntries
            _ -> Left "Error: Invalid record selector!"

instance StringyLens RouterSolicitation where
    update _ strs f
        = case strs of
            ("reserved" : ys) -> (normalRecUpdate rsReserved) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["reserved"] -> (applyToPrimNormalRec f) rsReserved
            _ -> Left "Error: Invalid record selector!"

instance StringyLens TraceRoute where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate trID) f
            ("unused" : ys) -> (normalRecUpdate trUnused) f
            ("outHopCount" : ys) -> (normalRecUpdate trOutHopCount) f
            ("retHopCount" : ys) -> (normalRecUpdate trRetHopCount) f
            ("outLinkSpd" : ys) -> (normalRecUpdate trOutLinkSpd) f
            ("outLinkMTU" : ys) -> (normalRecUpdate trOutLinkMTU) f
            _ -> Left "Error: Invalid record selector!"

    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) trID
            ["unused"] -> (applyToPrimNormalRec f) trUnused
            ["outHopCount"] -> (applyToPrimNormalRec f) trOutHopCount
            ["retHopCount"] -> (applyToPrimNormalRec f) trRetHopCount
            ["outLinkSpd"] -> (applyToPrimNormalRec f) trOutLinkSpd
            ["outLinkMTU"] -> (applyToPrimNormalRec f) trOutLinkMTU
            _ -> Left "Error: Invalid record selector!"

instance StringyLens AddressMaskRequest where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate amqID) f
            ("seqNum" : ys) -> (normalRecUpdate amqSeqNum) f
            ("mask" : ys) -> (normalRecUpdate amqMask) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) amqID
            ["seqNum"] -> (applyToPrimNormalRec f) amqSeqNum
            ["mask"] -> (applyToPrimNormalRec f) amqMask
            _ -> Left "Error: Invalid record selector!"

instance StringyLens AddressMaskReply where
    update _ strs f
        = case strs of
            ("id" : ys) -> (normalRecUpdate ampID) f
            ("seqNum" : ys) -> (normalRecUpdate ampSeqNum) f
            ("mask" : ys) -> (normalRecUpdate ampMask) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["id"] -> (applyToPrimNormalRec f) ampID
            ["seqNum"] -> (applyToPrimNormalRec f) ampSeqNum
            ["mask"] -> (applyToPrimNormalRec f) ampMask
            _ -> Left "Error: Invalid record selector!"

instance StringyLens ICMPData where
    update _ (str:ys) f
        = case str of
            "du" -> ((sumNonBottomUpdate ys) f) _DU
            "sq" -> ((sumNonBottomUpdate ys) f) _SQ
            "te" -> ((sumNonBottomUpdate ys) f) _TE
            "rd" -> ((sumNonBottomUpdate ys) f) _RD
            "pp" -> ((sumNonBottomUpdate ys) f) _PP
            "erq" -> ((sumNonBottomUpdate ys) f) _ERQ
            "erp" -> ((sumNonBottomUpdate ys) f) _ERP
            "tsrq" -> ((sumNonBottomUpdate ys) f) _TSRQ
            "tsrp" -> ((sumNonBottomUpdate ys) f) _TSRP
            "ra" -> ((sumNonBottomUpdate ys) f) _RA
            "rs" -> ((sumNonBottomUpdate ys) f) _RS
            "tr" -> ((sumNonBottomUpdate ys) f) _TR
            "amrq" -> ((sumNonBottomUpdate ys) f) _AMRQ
            "amrp" -> ((sumNonBottomUpdate ys) f) _AMRP
            _ -> Left $ "Error: Invalid record selector ICMPDATA: " <> str 
    update _ [] _ = Left "Error: Empty record selector!"

    applyTo _ strs f
        = case strs of
            ("du" : ys) -> ((sumNonBottomApplyTo ys) _DU) f
            ("sq" : ys) -> ((sumNonBottomApplyTo ys) _SQ) f
            ("te" : ys) -> ((sumNonBottomApplyTo ys) _TE) f
            ("rd" : ys) -> ((sumNonBottomApplyTo ys) _RD) f
            ("pp" : ys) -> ((sumNonBottomApplyTo ys) _PP) f
            ("erq" : ys) -> ((sumNonBottomApplyTo ys) _ERQ) f
            ("erp" : ys) -> ((sumNonBottomApplyTo ys) _ERP) f
            ("tsrq" : ys) -> ((sumNonBottomApplyTo ys) _TSRQ) f
            ("tsrp" : ys) -> ((sumNonBottomApplyTo ys) _TSRP) f
            ("ra" : ys) -> ((sumNonBottomApplyTo ys) _RA) f
            ("rs" : ys) -> ((sumNonBottomApplyTo ys) _RS) f
            ("tr" : ys) -> ((sumNonBottomApplyTo ys) _TR) f
            ("amrq" : ys) -> ((sumNonBottomApplyTo ys) _AMRQ) f
            ("amrp" : ys) -> ((sumNonBottomApplyTo ys) _AMRP) f
            _ -> Left "Error: Invalid record selector!"

instance StringyLens ICMPMessage where
    update _ (str:ys) f
        = case str of
            "hdr" -> ((nonBottomRecUpdate ys) f) icmpHdr

            "data"-> ((nonBottomRecUpdate ys) f) icmpData

            "data@DU" ->  case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @DestUnreachable) i

            "data@SQ" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @SourceQuench) i

            "data@TE"  -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @TimeExceeded) i
            "data@RD"  -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @Redirect) i
            "data@PP" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @ParamProblem) i
            "data@ERQ"   -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @EchoRequest) i
            "data@ERP"   -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @EchoReply) i
            "data@TSRQ"  -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @TimeStampRequest) i
            "data@TSRP" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @TimeStampReply) i
            "data@RA" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @RouterAdvertisement) i
            "data@RS" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @RouterSolicitation) i
            "data@TR" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @TraceRoute) i
            "data@AMRQ" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @AddressMaskRequest) i
            "data@AMRP" -> case ((nonBottomRecUpdate ys) f) icmpData of
                Left err -> Left err
                Right f  -> Right $ \i -> f $ trans icmpData (Proxy @AddressMaskReply) i
            _ -> Left $ "Error: Invalid record selector: ICMPMESSAGE: " <> str
    update _ [] _ = Left "Error: Empty record selector!"

    applyTo _ strs f
        = case strs of
            ("hdr" : ys) -> ((applyToPrimNonBottom ys) f) icmpHdr
            ("data" : ys) -> ((applyToPrimNonBottom ys) f) icmpData
            ("data:DU" : ys) -> ((applyToPrimNonBottom ys) f) icmpData
            _ -> Left "Error: Invalid record selector!"
----
-- TCP 
----
instance StringyLens ControlFlags where
    update _ strs f
        = case strs of
            ("urg" : ys) -> (normalRecUpdate tUrg) f
            ("ack" : ys) -> (normalRecUpdate tAck) f
            ("psh" : ys) -> (normalRecUpdate tPsh) f
            ("rst" : ys) -> (normalRecUpdate tRst) f
            ("syn" : ys) -> (normalRecUpdate tSyn) f
            ("fin" : ys) -> (normalRecUpdate tFin) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["urg"] -> (applyToPrimNormalRec f) tUrg
            ["ack"] -> (applyToPrimNormalRec f) tAck
            ["psh"] -> (applyToPrimNormalRec f) tPsh
            ["rst"] -> (applyToPrimNormalRec f) tRst
            ["syn"] -> (applyToPrimNormalRec f) tSyn
            ["fin"] -> (applyToPrimNormalRec f) tFin
            _ -> Left "Error: Invalid record selector!"
instance StringyLens TCPOption where
    update _ strs f
        = case strs of
            ("opKind" : ys) -> (normalRecUpdate tOpKind) f
            ("opLen" : ys) -> (normalRecUpdate tOpLen) f
            ("opData" : ys) -> (normalRecUpdate tOpData) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["opKind"] -> (applyToPrimNormalRec f) tOpKind
            ["opLen"] -> (applyToPrimNormalRec f) tOpLen
            ["opData"] -> (applyToPrimNormalRec f) tOpData
            _ -> Left "Error: Invalid record selector!"
instance StringyLens TCPSegment where
    update _ strs f
        = case strs of
            ("src" : ys) -> (normalRecUpdate tSrc) f
            ("dst" : ys) -> (normalRecUpdate tDst) f
            ("seqNum" : ys) -> (normalRecUpdate tSeqNum) f
            ("ackNum" : ys) -> (normalRecUpdate tAckNum) f
            ("offset" : ys) -> (normalRecUpdate tOffset) f
            ("flags" : ys) -> ((nonBottomRecUpdate ys) f) tFlags
            ("win" : ys) -> (normalRecUpdate tWin) f
            ("checksum" : ys) -> (normalRecUpdate tChecksum) f
            ("urgPtr" : ys) -> (normalRecUpdate tUrgPntr) f
            --("opts" : ys) -> ((nonBottomRecUpdate ys) f) tOpts
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["src"] -> (applyToPrimNormalRec f) tSrc
            ["dst"] -> (applyToPrimNormalRec f) tDst
            ["seqNum"] -> (applyToPrimNormalRec f) tSeqNum
            ["ackNum"] -> (applyToPrimNormalRec f) tAckNum
            ["offset"] -> (applyToPrimNormalRec f) tOffset
            ("flags" : ys) -> ((applyToPrimNonBottom ys) f) tFlags
            ["win"] -> (applyToPrimNormalRec f) tWin
            ["checksum"] -> (applyToPrimNormalRec f) tChecksum
            ["urgPtr"] -> (applyToPrimNormalRec f) tUrgPntr
            --("opts" : ys) -> ((applyToPrimNonBottom ys) f) tOpts
            _ -> Left "Error: Invalid record selector!"
----
-- UDP
-----
instance StringyLens UDPMessage where
    update _ strs f
        = case strs of
            ("src" : ys) -> (normalRecUpdate uSrc) f
            ("dst" : ys) -> (normalRecUpdate uDst) f
            ("len" : ys) -> (normalRecUpdate uLen) f
            ("checksum" : ys) -> (normalRecUpdate uChecksum) f
            _ -> Left "Error: Invalid record selector!"
    applyTo _ strs f
        = case strs of
            ["src"] -> (applyToPrimNormalRec f) uSrc
            ["dst"] -> (applyToPrimNormalRec f) uDst
            ["len"] -> (applyToPrimNormalRec f) uLen
            ["checksum"] -> (applyToPrimNormalRec f) uChecksum
            _ -> Left "Error: Invalid record selector!"

--}

{--

     deriveStringyLens ''EthernetFrame
   ======>
     instance StringyLens EthernetFrame where
       update _ strs f
         = case strs of
             ("dst" : ys) -> (normalRecUpdate eDst) f
             ("src" : ys) -> (normalRecUpdate eSrc) f
             ("etherType" : ys) -> (normalRecUpdate eEtherType) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["dst"] -> (applyToPrimNormalRec f) eDst
             ["src"] -> (applyToPrimNormalRec f) eSrc
             ["etherType"] -> (applyToPrimNormalRec f) eEtherType
             _ -> Left "Error: Invalid record selector!"
 /home/gnumonic/Code/Haskell/Hektor/src/Base/Wrappers.hs:222:1-30: Splicing declarations
     deriveStringyLens ''ARPMessage
   ======>
     instance StringyLens ARPMessage where
       update _ strs f
         = case strs of
             ("hrd" : ys) -> (normalRecUpdate aHrd) f
             ("pro" : ys) -> (normalRecUpdate aPro) f
             ("hln" : ys) -> (normalRecUpdate aHln) f
             ("pln" : ys) -> (normalRecUpdate aPln) f
             ("op" : ys) -> (normalRecUpdate aOp) f
             ("sha" : ys) -> (normalRecUpdate aSha) f
             ("spa" : ys) -> (normalRecUpdate aSpa) f
             ("tha" : ys) -> (normalRecUpdate aTha) f
             ("tpa" : ys) -> (normalRecUpdate aTpa) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["hrd"] -> (applyToPrimNormalRec f) aHrd
             ["pro"] -> (applyToPrimNormalRec f) aPro
             ["hln"] -> (applyToPrimNormalRec f) aHln
             ["pln"] -> (applyToPrimNormalRec f) aPln
             ["op"] -> (applyToPrimNormalRec f) aOp
             ["sha"] -> (applyToPrimNormalRec f) aSha
             ["spa"] -> (applyToPrimNormalRec f) aSpa
             ["tha"] -> (applyToPrimNormalRec f) aTha
             ["tpa"] -> (applyToPrimNormalRec f) aTpa
             _ -> Left "Error: Invalid record selector!"
 /home/gnumonic/Code/Haskell/Hektor/src/Base/Wrappers.hs:223:1-29: Splicing declarations
     deriveStringyLens ''IP4Packet
   ======>
     instance StringyLens IPFlags where
       update _ strs f
         = case strs of
             ("rsvd" : ys) -> (normalRecUpdate i4Rsvd) f
             ("df" : ys) -> (normalRecUpdate i4Df) f
             ("mf" : ys) -> (normalRecUpdate i4Mf) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["rsvd"] -> (applyToPrimNormalRec f) i4Rsvd
             ["df"] -> (applyToPrimNormalRec f) i4Df
             ["mf"] -> (applyToPrimNormalRec f) i4Mf
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens OptionType where
       update _ strs f
         = case strs of
             ("cFlag" : ys) -> (normalRecUpdate i4CFlag) f
             ("opClass" : ys) -> (normalRecUpdate i4OpClass) f
             ("opNum" : ys) -> (normalRecUpdate i4OpNum) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["cFlag"] -> (applyToPrimNormalRec f) i4CFlag
             ["opClass"] -> (applyToPrimNormalRec f) i4OpClass
             ["opNum"] -> (applyToPrimNormalRec f) i4OpNum
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens Option where
       update _ strs f
         = case strs of
             ("opType" : ys) -> ((nonBottomRecUpdate ys) f) i4OpType
             ("opLength" : ys) -> (normalRecUpdate i4OpLength) f
             ("opData" : ys) -> (normalRecUpdate i4OpData) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("opType" : ys) -> ((applyToPrimNonBottom ys) f) i4OpType
             ["opLength"] -> (applyToPrimNormalRec f) i4OpLength
             ["opData"] -> (applyToPrimNormalRec f) i4OpData
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens IP4Packet where
       update _ strs f
         = case strs of
             ("vers" : ys) -> (normalRecUpdate i4Vers) f
             ("ihl" : ys) -> (normalRecUpdate i4Ihl) f
             ("tos" : ys) -> (normalRecUpdate i4Tos) f
             ("tl" : ys) -> (normalRecUpdate i4Tl) f
             ("iden" : ys) -> (normalRecUpdate i4Iden) f
             ("flags" : ys) -> ((nonBottomRecUpdate ys) f) i4Flags
             ("off" : ys) -> (normalRecUpdate i4Off) f
             ("ttl" : ys) -> (normalRecUpdate i4Ttl) f
             ("proto" : ys) -> (normalRecUpdate i4Proto) f
             ("checksum" : ys) -> (normalRecUpdate i4Checksum) f
             ("src" : ys) -> (normalRecUpdate i4Src) f
             ("dst" : ys) -> (normalRecUpdate i4Dst) f
             ("opts" : ys)
               -> (((listUpdate (proxyA i4Opts)) ys) f) i4Opts ::
                    Either T.Text (IP4Packet -> [IP4Packet])
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["vers"] -> (applyToPrimNormalRec f) i4Vers
             ["ihl"] -> (applyToPrimNormalRec f) i4Ihl
             ["tos"] -> (applyToPrimNormalRec f) i4Tos
             ["tl"] -> (applyToPrimNormalRec f) i4Tl
             ["iden"] -> (applyToPrimNormalRec f) i4Iden
             ("flags" : ys) -> ((applyToPrimNonBottom ys) f) i4Flags
             ["off"] -> (applyToPrimNormalRec f) i4Off
             ["ttl"] -> (applyToPrimNormalRec f) i4Ttl
             ["proto"] -> (applyToPrimNormalRec f) i4Proto
             ["checksum"] -> (applyToPrimNormalRec f) i4Checksum
             ["src"] -> (applyToPrimNormalRec f) i4Src
             ["dst"] -> (applyToPrimNormalRec f) i4Dst
             ("opts" : ys) -> ((listApplyTo ys) f) i4Opts
             _ -> Left "Error: Invalid record selector!"
 /home/gnumonic/Code/Haskell/Hektor/src/Base/Wrappers.hs:224:1-30: Splicing declarations
     deriveStringyLens ''UDPMessage
   ======>
     instance StringyLens UDPMessage where
       update _ strs f
         = case strs of
             ("src" : ys) -> (normalRecUpdate uSrc) f
             ("dst" : ys) -> (normalRecUpdate uDst) f
             ("len" : ys) -> (normalRecUpdate uLen) f
             ("checksum" : ys) -> (normalRecUpdate uChecksum) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["src"] -> (applyToPrimNormalRec f) uSrc
             ["dst"] -> (applyToPrimNormalRec f) uDst
             ["len"] -> (applyToPrimNormalRec f) uLen
             ["checksum"] -> (applyToPrimNormalRec f) uChecksum
             _ -> Left "Error: Invalid record selector!"
 /home/gnumonic/Code/Haskell/Hektor/src/Base/Wrappers.hs:225:1-30: Splicing declarations
     deriveStringyLens ''TCPSegment
   ======>
     instance StringyLens ControlFlags where
       update _ strs f
         = case strs of
             ("urg" : ys) -> (normalRecUpdate tUrg) f
             ("ack" : ys) -> (normalRecUpdate tAck) f
             ("psh" : ys) -> (normalRecUpdate tPsh) f
             ("rst" : ys) -> (normalRecUpdate tRst) f
             ("syn" : ys) -> (normalRecUpdate tSyn) f
             ("fin" : ys) -> (normalRecUpdate tFin) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["urg"] -> (applyToPrimNormalRec f) tUrg
             ["ack"] -> (applyToPrimNormalRec f) tAck
             ["psh"] -> (applyToPrimNormalRec f) tPsh
             ["rst"] -> (applyToPrimNormalRec f) tRst
             ["syn"] -> (applyToPrimNormalRec f) tSyn
             ["fin"] -> (applyToPrimNormalRec f) tFin
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens TCPOption where
       update _ strs f
         = case strs of
             ("opKind" : ys) -> (normalRecUpdate tOpKind) f
             ("opLen" : ys) -> (normalRecUpdate tOpLen) f
             ("opData" : ys) -> (normalRecUpdate tOpData) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["opKind"] -> (applyToPrimNormalRec f) tOpKind
             ["opLen"] -> (applyToPrimNormalRec f) tOpLen
             ["opData"] -> (applyToPrimNormalRec f) tOpData
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens TCPSegment where
       update _ strs f
         = case strs of
             ("src" : ys) -> (normalRecUpdate tSrc) f
             ("dst" : ys) -> (normalRecUpdate tDst) f
             ("seqNum" : ys) -> (normalRecUpdate tSeqNum) f
             ("ackNum" : ys) -> (normalRecUpdate tAckNum) f
             ("offset" : ys) -> (normalRecUpdate tOffset) f
             ("flags" : ys) -> ((nonBottomRecUpdate ys) f) tFlags
             ("win" : ys) -> (normalRecUpdate tWin) f
             ("checksum" : ys) -> (normalRecUpdate tChecksum) f
             ("urgPntr" : ys) -> (normalRecUpdate tUrgPntr) f
             ("opts" : ys)
               -> (((listUpdate (proxyA tOpts)) ys) f) tOpts ::
                    Either T.Text (TCPSegment -> [TCPSegment])
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["src"] -> (applyToPrimNormalRec f) tSrc
             ["dst"] -> (applyToPrimNormalRec f) tDst
             ["seqNum"] -> (applyToPrimNormalRec f) tSeqNum
             ["ackNum"] -> (applyToPrimNormalRec f) tAckNum
             ["offset"] -> (applyToPrimNormalRec f) tOffset
             ("flags" : ys) -> ((applyToPrimNonBottom ys) f) tFlags
             ["win"] -> (applyToPrimNormalRec f) tWin
             ["checksum"] -> (applyToPrimNormalRec f) tChecksum
             ["urgPntr"] -> (applyToPrimNormalRec f) tUrgPntr
             ("opts" : ys) -> ((listApplyTo ys) f) tOpts
             _ -> Left "Error: Invalid record selector!"
 /home/gnumonic/Code/Haskell/Hektor/src/Base/Wrappers.hs:226:1-31: Splicing declarations
     deriveStringyLens ''ICMPMessage
   ======>
     instance StringyLens ICMPHeader where
       update _ strs f
         = case strs of
             ("type" : ys) -> (normalRecUpdate icmpType) f
             ("code" : ys) -> (normalRecUpdate icmpCode) f
             ("checksum" : ys) -> (normalRecUpdate icmpChecksum) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["type"] -> (applyToPrimNormalRec f) icmpType
             ["code"] -> (applyToPrimNormalRec f) icmpCode
             ["checksum"] -> (applyToPrimNormalRec f) icmpChecksum
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DestUnreachable where
       update _ strs f
         = case strs of
             ("unused" : ys) -> (normalRecUpdate duUnused) f
             ("dgPortion" : ys) -> (normalRecUpdate duDgPortion) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["unused"] -> (applyToPrimNormalRec f) duUnused
             ["dgPortion"] -> (applyToPrimNormalRec f) duDgPortion
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens SourceQuench where
       update _ strs f
         = case strs of
             ("unused" : ys) -> (normalRecUpdate sqUnused) f
             ("dgPortion" : ys) -> (normalRecUpdate sqDgPortion) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["unused"] -> (applyToPrimNormalRec f) sqUnused
             ["dgPortion"] -> (applyToPrimNormalRec f) sqDgPortion
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens TimeExceeded where
       update _ strs f
         = case strs of
             ("unused" : ys) -> (normalRecUpdate teUnused) f
             ("dgPortion" : ys) -> (normalRecUpdate teDgPortion) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["unused"] -> (applyToPrimNormalRec f) teUnused
             ["dgPortion"] -> (applyToPrimNormalRec f) teDgPortion
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens Redirect where
       update _ strs f
         = case strs of
             ("addr" : ys) -> (normalRecUpdate rdAddr) f
             ("dgPortion" : ys) -> (normalRecUpdate rdDgPortion) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["addr"] -> (applyToPrimNormalRec f) rdAddr
             ["dgPortion"] -> (applyToPrimNormalRec f) rdDgPortion
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens ParamProblem where
       update _ strs f
         = case strs of
             ("ptr" : ys) -> (normalRecUpdate ppPtr) f
             ("unused" : ys) -> (normalRecUpdate ppUnused) f
             ("dgPortion" : ys) -> (normalRecUpdate ppDgPortion) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["ptr"] -> (applyToPrimNormalRec f) ppPtr
             ["unused"] -> (applyToPrimNormalRec f) ppUnused
             ["dgPortion"] -> (applyToPrimNormalRec f) ppDgPortion
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens EchoRequest where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate erqId) f
             ("seqNum" : ys) -> (normalRecUpdate erqSeqNum) f
             ("data" : ys) -> (normalRecUpdate erqData) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) erqId
             ["seqNum"] -> (applyToPrimNormalRec f) erqSeqNum
             ["data"] -> (applyToPrimNormalRec f) erqData
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens EchoReply where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate erpId) f
             ("seqNum" : ys) -> (normalRecUpdate erpSeqNum) f
             ("data" : ys) -> (normalRecUpdate erpData) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) erpId
             ["seqNum"] -> (applyToPrimNormalRec f) erpSeqNum
             ["data"] -> (applyToPrimNormalRec f) erpData
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens TimeStampRequest where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate trqId) f
             ("seqNum" : ys) -> (normalRecUpdate trqSeqNum) f
             ("org" : ys) -> (normalRecUpdate trqOrg) f
             ("rcv" : ys) -> (normalRecUpdate trqRcv) f
             ("trs" : ys) -> (normalRecUpdate trqTrs) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) trqId
             ["seqNum"] -> (applyToPrimNormalRec f) trqSeqNum
             ["org"] -> (applyToPrimNormalRec f) trqOrg
             ["rcv"] -> (applyToPrimNormalRec f) trqRcv
             ["trs"] -> (applyToPrimNormalRec f) trqTrs
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens TimeStampReply where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate trpId) f
             ("seqNum" : ys) -> (normalRecUpdate trpSeqNum) f
             ("org" : ys) -> (normalRecUpdate trpOrg) f
             ("rcv" : ys) -> (normalRecUpdate trpRcv) f
             ("trs" : ys) -> (normalRecUpdate trpTrs) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) trpId
             ["seqNum"] -> (applyToPrimNormalRec f) trpSeqNum
             ["org"] -> (applyToPrimNormalRec f) trpOrg
             ["rcv"] -> (applyToPrimNormalRec f) trpRcv
             ["trs"] -> (applyToPrimNormalRec f) trpTrs
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RouterAdvertisement where
       update _ strs f
         = case strs of
             ("numAddrs" : ys) -> (normalRecUpdate raNumAddrs) f
             ("entrySize" : ys) -> (normalRecUpdate raEntrySize) f
             ("lifetime" : ys) -> (normalRecUpdate raLifetime) f
             ("entries" : ys) -> (normalRecUpdate raEntries) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["numAddrs"] -> (applyToPrimNormalRec f) raNumAddrs
             ["entrySize"] -> (applyToPrimNormalRec f) raEntrySize
             ["lifetime"] -> (applyToPrimNormalRec f) raLifetime
             ["entries"] -> (applyToPrimNormalRec f) raEntries
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RouterSolicitation where
       update _ strs f
         = case strs of
             ("reserved" : ys) -> (normalRecUpdate rsReserved) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["reserved"] -> (applyToPrimNormalRec f) rsReserved
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens TraceRoute where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate trId) f
             ("unused" : ys) -> (normalRecUpdate trUnused) f
             ("outHop" : ys) -> (normalRecUpdate trOutHop) f
             ("retHop" : ys) -> (normalRecUpdate trRetHop) f
             ("outSpd" : ys) -> (normalRecUpdate trOutSpd) f
             ("outMTU" : ys) -> (normalRecUpdate trOutMTU) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) trId
             ["unused"] -> (applyToPrimNormalRec f) trUnused
             ["outHop"] -> (applyToPrimNormalRec f) trOutHop
             ["retHop"] -> (applyToPrimNormalRec f) trRetHop
             ["outSpd"] -> (applyToPrimNormalRec f) trOutSpd
             ["outMTU"] -> (applyToPrimNormalRec f) trOutMTU
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens AddressMaskRequest where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate amqId) f
             ("seqNum" : ys) -> (normalRecUpdate amqSeqNum) f
             ("mask" : ys) -> (normalRecUpdate amqMask) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) amqId
             ["seqNum"] -> (applyToPrimNormalRec f) amqSeqNum
             ["mask"] -> (applyToPrimNormalRec f) amqMask
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens AddressMaskReply where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate ampId) f
             ("seqNum" : ys) -> (normalRecUpdate ampSeqNum) f
             ("mask" : ys) -> (normalRecUpdate ampMask) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) ampId
             ["seqNum"] -> (applyToPrimNormalRec f) ampSeqNum
             ["mask"] -> (applyToPrimNormalRec f) ampMask
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens ICMPData where
       update _ strs f
         = case strs of
             ("du" : ys) -> ((sumNonBottomUpdate ys) f) _DU
             ("sq" : ys) -> ((sumNonBottomUpdate ys) f) _SQ
             ("te" : ys) -> ((sumNonBottomUpdate ys) f) _TE
             ("rd" : ys) -> ((sumNonBottomUpdate ys) f) _RD
             ("pp" : ys) -> ((sumNonBottomUpdate ys) f) _PP
             ("erq" : ys) -> ((sumNonBottomUpdate ys) f) _ERQ
             ("erp" : ys) -> ((sumNonBottomUpdate ys) f) _ERP
             ("tsrq" : ys) -> ((sumNonBottomUpdate ys) f) _TSRQ
             ("tsrp" : ys) -> ((sumNonBottomUpdate ys) f) _TSRP
             ("ra" : ys) -> ((sumNonBottomUpdate ys) f) _RA
             ("rs" : ys) -> ((sumNonBottomUpdate ys) f) _RS
             ("tr" : ys) -> ((sumNonBottomUpdate ys) f) _TR
             ("amrq" : ys) -> ((sumNonBottomUpdate ys) f) _AMRQ
             ("amrp" : ys) -> ((sumNonBottomUpdate ys) f) _AMRP
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("du" : ys) -> ((sumNonBottomApplyTo ys) _DU) f
             ("sq" : ys) -> ((sumNonBottomApplyTo ys) _SQ) f
             ("te" : ys) -> ((sumNonBottomApplyTo ys) _TE) f
             ("rd" : ys) -> ((sumNonBottomApplyTo ys) _RD) f
             ("pp" : ys) -> ((sumNonBottomApplyTo ys) _PP) f
             ("erq" : ys) -> ((sumNonBottomApplyTo ys) _ERQ) f
             ("erp" : ys) -> ((sumNonBottomApplyTo ys) _ERP) f
             ("tsrq" : ys) -> ((sumNonBottomApplyTo ys) _TSRQ) f
             ("tsrp" : ys) -> ((sumNonBottomApplyTo ys) _TSRP) f
             ("ra" : ys) -> ((sumNonBottomApplyTo ys) _RA) f
             ("rs" : ys) -> ((sumNonBottomApplyTo ys) _RS) f
             ("tr" : ys) -> ((sumNonBottomApplyTo ys) _TR) f
             ("amrq" : ys) -> ((sumNonBottomApplyTo ys) _AMRQ) f
             ("amrp" : ys) -> ((sumNonBottomApplyTo ys) _AMRP) f
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens ICMPMessage where
       update _ strs f
         = case strs of
             ("hdr" : ys) -> ((nonBottomRecUpdate ys) f) icmpHdr
             ("data" : ys) -> ((nonBottomRecUpdate ys) f) icmpData
             ("data@DU" : ys)
               -> (((transformerUpdate (Proxy :: Proxy DestUnreachable)) ys) f)
                    icmpData
             ("data@SQ" : ys)
               -> (((transformerUpdate (Proxy :: Proxy SourceQuench)) ys) f)
                    icmpData
             ("data@TE" : ys)
               -> (((transformerUpdate (Proxy :: Proxy TimeExceeded)) ys) f)
                    icmpData
             ("data@RD" : ys)
               -> (((transformerUpdate (Proxy :: Proxy Redirect)) ys) f) icmpData
             ("data@PP" : ys)
               -> (((transformerUpdate (Proxy :: Proxy ParamProblem)) ys) f)
                    icmpData
             ("data@ERQ" : ys)
               -> (((transformerUpdate (Proxy :: Proxy EchoRequest)) ys) f)
                    icmpData
             ("data@ERP" : ys)
               -> (((transformerUpdate (Proxy :: Proxy EchoReply)) ys) f) icmpData
             ("data@TSRQ" : ys)
               -> (((transformerUpdate (Proxy :: Proxy TimeStampRequest)) ys) f)
                    icmpData
             ("data@TSRP" : ys)
               -> (((transformerUpdate (Proxy :: Proxy TimeStampReply)) ys) f)
                    icmpData
             ("data@RA" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RouterAdvertisement)) ys)
                     f)
                    icmpData
             ("data@RS" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RouterSolicitation)) ys) f)
                    icmpData
             ("data@TR" : ys)
               -> (((transformerUpdate (Proxy :: Proxy TraceRoute)) ys) f)
                    icmpData
             ("data@AMRQ" : ys)
               -> (((transformerUpdate (Proxy :: Proxy AddressMaskRequest)) ys) f)
                    icmpData
             ("data@AMRP" : ys)
               -> (((transformerUpdate (Proxy :: Proxy AddressMaskReply)) ys) f)
                    icmpData
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("hdr" : ys) -> ((applyToPrimNonBottom ys) f) icmpHdr
             ("data" : ys) -> ((applyToPrimNonBottom ys) f) icmpData
             _ -> Left "Error: Invalid record selector!"
 /home/gnumonic/Code/Haskell/Hektor/src/Base/Wrappers.hs:227:1-30: Splicing declarations
     deriveStringyLens ''DNSMessage
   ======>
     instance StringyLens DNSHeader where
       update _ strs f
         = case strs of
             ("id" : ys) -> (normalRecUpdate dhId) f
             ("qr" : ys) -> (normalRecUpdate dhQr) f
             ("op" : ys) -> (normalRecUpdate dhOp) f
             ("aa" : ys) -> (normalRecUpdate dhAa) f
             ("tc" : ys) -> (normalRecUpdate dhTc) f
             ("rd" : ys) -> (normalRecUpdate dhRd) f
             ("ra" : ys) -> (normalRecUpdate dhRa) f
             ("z" : ys) -> (normalRecUpdate dhZ) f
             ("rCode" : ys) -> (normalRecUpdate dhRCode) f
             ("qdCount" : ys) -> (normalRecUpdate dhQdCount) f
             ("anCount" : ys) -> (normalRecUpdate dhAnCount) f
             ("nsCount" : ys) -> (normalRecUpdate dhNsCount) f
             ("arCount" : ys) -> (normalRecUpdate dhArCount) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["id"] -> (applyToPrimNormalRec f) dhId
             ["qr"] -> (applyToPrimNormalRec f) dhQr
             ["op"] -> (applyToPrimNormalRec f) dhOp
             ["aa"] -> (applyToPrimNormalRec f) dhAa
             ["tc"] -> (applyToPrimNormalRec f) dhTc
             ["rd"] -> (applyToPrimNormalRec f) dhRd
             ["ra"] -> (applyToPrimNormalRec f) dhRa
             ["z"] -> (applyToPrimNormalRec f) dhZ
             ["rCode"] -> (applyToPrimNormalRec f) dhRCode
             ["qdCount"] -> (applyToPrimNormalRec f) dhQdCount
             ["anCount"] -> (applyToPrimNormalRec f) dhAnCount
             ["nsCount"] -> (applyToPrimNormalRec f) dhNsCount
             ["arCount"] -> (applyToPrimNormalRec f) dhArCount
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DNSQuestion where
       update _ strs f
         = case strs of
             ("name" : ys) -> (normalRecUpdate qName) f
             ("type" : ys) -> (normalRecUpdate qType) f
             ("class" : ys) -> (normalRecUpdate qClass) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["name"] -> (applyToPrimNormalRec f) qName
             ["type"] -> (applyToPrimNormalRec f) qType
             ["class"] -> (applyToPrimNormalRec f) qClass
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RR_NS where
       update _ strs f
         = case strs of
             [] -> (normalRecUpdate rrNs) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             [] -> (applyToPrimNormalRec f) rrNs
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RR_CNAME where
       update _ strs f
         = case strs of
             [] -> (normalRecUpdate rrCName) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             [] -> (applyToPrimNormalRec f) rrCName
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RR_SOA where
       update _ strs f
         = case strs of
             ("mName" : ys) -> (normalRecUpdate soaMName) f
             ("rName" : ys) -> (normalRecUpdate soaRName) f
             ("serial" : ys) -> (normalRecUpdate soaSerial) f
             ("refresh" : ys) -> (normalRecUpdate soaRefresh) f
             ("retry" : ys) -> (normalRecUpdate soaRetry) f
             ("exp" : ys) -> (normalRecUpdate soaExp) f
             ("min" : ys) -> (normalRecUpdate soaMin) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["mName"] -> (applyToPrimNormalRec f) soaMName
             ["rName"] -> (applyToPrimNormalRec f) soaRName
             ["serial"] -> (applyToPrimNormalRec f) soaSerial
             ["refresh"] -> (applyToPrimNormalRec f) soaRefresh
             ["retry"] -> (applyToPrimNormalRec f) soaRetry
             ["exp"] -> (applyToPrimNormalRec f) soaExp
             ["min"] -> (applyToPrimNormalRec f) soaMin
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RR_PTR where
       update _ strs f
         = case strs of
             [] -> (normalRecUpdate rrPtr) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             [] -> (applyToPrimNormalRec f) rrPtr
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RR_MX where
       update _ strs f
         = case strs of
             ("pref" : ys) -> (normalRecUpdate mxPref) f
             ("exch" : ys) -> (normalRecUpdate mxExch) f
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["pref"] -> (applyToPrimNormalRec f) mxPref
             ["exch"] -> (applyToPrimNormalRec f) mxExch
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens RData where
       update _ strs f
         = case strs of
             ["a"] -> (sumNormalUpdate f) _A
             ("ns" : ys) -> ((sumNonBottomUpdate ys) f) _NS
             ("cname" : ys) -> ((sumNonBottomUpdate ys) f) _CName
             ("soa" : ys) -> ((sumNonBottomUpdate ys) f) _SOA
             ("ptr" : ys) -> ((sumNonBottomUpdate ys) f) _PTR
             ("mx" : ys) -> ((sumNonBottomUpdate ys) f) _MX
             ["txt"] -> (sumNormalUpdate f) _TXT
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["a"] -> (sumNormalApplyTo _A) f
             ("ns" : ys) -> ((sumNonBottomApplyTo ys) _NS) f
             ("cname" : ys) -> ((sumNonBottomApplyTo ys) _CName) f
             ("soa" : ys) -> ((sumNonBottomApplyTo ys) _SOA) f
             ("ptr" : ys) -> ((sumNonBottomApplyTo ys) _PTR) f
             ("mx" : ys) -> ((sumNonBottomApplyTo ys) _MX) f
             ["txt"] -> (sumNormalApplyTo _TXT) f
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DNSRR where
       update _ strs f
         = case strs of
             ("name" : ys) -> (normalRecUpdate rrName) f
             ("type" : ys) -> (normalRecUpdate rrType) f
             ("class" : ys) -> (normalRecUpdate rrClass) f
             ("ttl" : ys) -> (normalRecUpdate rrTtl) f
             ("len" : ys) -> (normalRecUpdate rrLen) f
             ("data" : ys) -> ((nonBottomRecUpdate ys) f) rrData
             ("data@A" : ys)
               -> (((transformerUpdate (Proxy :: Proxy IP4Address)) ys) f) rrData
             ("data@NS" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RR_NS)) ys) f) rrData
             ("data@CName" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RR_CNAME)) ys) f) rrData
             ("data@SOA" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RR_SOA)) ys) f) rrData
             ("data@PTR" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RR_PTR)) ys) f) rrData
             ("data@MX" : ys)
               -> (((transformerUpdate (Proxy :: Proxy RR_MX)) ys) f) rrData
             ("data@TXT" : ys)
               -> (((transformerUpdate (Proxy :: Proxy BS.ByteString)) ys) f)
                    rrData
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ["name"] -> (applyToPrimNormalRec f) rrName
             ["type"] -> (applyToPrimNormalRec f) rrType
             ["class"] -> (applyToPrimNormalRec f) rrClass
             ["ttl"] -> (applyToPrimNormalRec f) rrTtl
             ["len"] -> (applyToPrimNormalRec f) rrLen
             ("data" : ys) -> ((applyToPrimNonBottom ys) f) rrData
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DNSAnswer where
       update _ strs f
         = case strs of
             ("answer" : ys) -> ((nonBottomRecUpdate ys) f) dAnswer
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ys -> ((applyToPrimNonBottom ys) f) dAnswer
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DNSAuth where
       update _ strs f
         = case strs of
             ("auth" : ys) -> ((nonBottomRecUpdate ys) f) dAuth
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ys -> ((applyToPrimNonBottom ys) f) dAuth
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DNSAdd where
       update _ strs f
         = case strs of
             ("add" : ys) -> ((nonBottomRecUpdate ys) f) dAdd
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ys -> ((applyToPrimNonBottom ys) f) dAdd
             _ -> Left "Error: Invalid record selector!"
     instance StringyLens DNSMessage where
       update _ strs f
         = case strs of
             ("hdr" : ys) -> ((nonBottomRecUpdate ys) f) dnsHdr
             ("question" : ys)
               -> (((listUpdate (proxyA dnsQuestion)) ys) f) dnsQuestion ::
                    Either T.Text (DNSMessage -> [DNSMessage])
             ("answer" : ys)
               -> (((listUpdate (proxyA dnsAnswer)) ys) f) dnsAnswer ::
                    Either T.Text (DNSMessage -> [DNSMessage])
             ("auth" : ys)
               -> (((listUpdate (proxyA dnsAuth)) ys) f) dnsAuth ::
                    Either T.Text (DNSMessage -> [DNSMessage])
             ("add" : ys)
               -> (((listUpdate (proxyA dnsAdd)) ys) f) dnsAdd ::
                    Either T.Text (DNSMessage -> [DNSMessage])
             _ -> Left "Error: Invalid record selector!"
       applyTo _ strs f
         = case strs of
             ("hdr" : ys) -> ((applyToPrimNonBottom ys) f) dnsHdr
             ("question" : ys) -> ((listApplyTo ys) f) dnsQuestion
             ("answer" : ys) -> ((listApplyTo ys) f) dnsAnswer
             ("auth" : ys) -> ((listApplyTo ys) f) dnsAuth
             ("add" : ys) -> ((listApplyTo ys) f) dnsAdd
             _ -> Left "Error: Invalid record selector!"
 
--}

































