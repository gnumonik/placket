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
{-# LANGUAGE UndecidableSuperClasses, OverloadedStrings   #-}

module GPrettyPrint where 
import PrettyPrint 
import PrimTypes 
import FieldClasses
import Control.Lens hiding (from)
import Data.Proxy 
import GenericFunctions 
import Staging
import Classes
import Generics.SOP
import Data.Char
import qualified Data.Text as T 

formatRecord :: String -> String
formatRecord  nm =  case dropWhile (\x -> isLower x || isDigit x || x == '_') $ nm of
    (x : xs) -> toLower x : xs
    ys       -> ys

gPrettyPrint :: forall a r. (Generic a, HasDatatypeInfo a,All Top (Code a), AllN SOP PrettyPrint (Code a), Code a ~ '[r]) 
             => T.Text
             -> PrintMode
             -> a 
             -> T.Text 
gPrettyPrint label mode a 
    = let recordNames =  map (T.pack . formatRecord) $ records (Proxy @a)
      in makeLabelRow label 
      <> makeDataRow (zipWith (\x y -> x <> ": " <> y) recordNames (go mode $ from a))
      <> dashRow
   where
       go :: forall xss. (All2 PrettyPrint xss, All2 Top xss) => PrintMode -> SOP I xss -> [T.Text]
       go m xss = hcollapse $ hcmap (Proxy @PrettyPrint) (mapIK $ pprint m) $ xss 



gPrettyPrint' :: forall a r. (Generic a, HasDatatypeInfo a,All Top (Code a), AllN SOP PrettyPrint (Code a), Code a ~ '[r]) 
             => T.Text
             -> PrintMode
             -> a 
             -> T.Text 
gPrettyPrint' label mode a 
    = let recordNames =  map (T.pack . formatRecord) $ records (Proxy @a)
      in makeLabelRowDotted label 
      <> makeDataRow (zipWith (\x y -> x <> ": " <> y) recordNames (go mode $ from a))
      <> dotRow
   where
       go :: forall xss. (All2 PrettyPrint xss, All2 Top xss) => PrintMode -> SOP I xss -> [T.Text]
       go m xss = hcollapse $ hcmap (Proxy @PrettyPrint) (mapIK $ pprint m) $ xss 

gPrettyPrintSum :: forall a b bs. (Generic a, All PrettyPrint b, All2 PrettyPrint bs,  Code a ~ bs, bs ~ '[b]) =>  PrintMode -> a -> T.Text
gPrettyPrintSum  mode a = go (from a)
    where
        go :: forall xs. (AllN SOP PrettyPrint xs, AllN SOP  Top xs) =>  SOP I xs-> T.Text
        go (SOP (Z xs))  = gogo xs 
        go (SOP (S xs) ) = go (SOP xs)

        gogo :: forall xs. (All PrettyPrint xs, All Top xs) => NP I xs -> T.Text 
        gogo xss = case (hcollapse . hcmap (Proxy :: Proxy  PrettyPrint) (mapIK $ pprint mode) $ xss) of
            [x] -> x
            _   -> error "Impossible empty type!"


instance PrettyPrint EthernetFrame where
    pprint m x = gPrettyPrint "Ethernet" m x 

instance PrettyPrint ARPMessage where
    pprint m x = gPrettyPrint "ARP" m x 

instance PrettyPrint OptionType where
    pprint m x = gPrettyPrint' "IP4 Option Type" m x

instance PrettyPrint IPFlags where
    pprint m x = gPrettyPrint' "IP4 Flags" m x 

instance PrettyPrint Option where
    pprint m x = makeLabelRowDotted "IP4 Option"
               <> pprint m (x ^. i4OpType)
               <> makeDataRow ["opLength: " <> (pprint m $ x ^. i4OpLength)
                              ,"opData: "   <> (pprint m $ x ^. i4OpData)  ]

instance PrettyPrint IP4Packet where
    pprint m x = makeLabelRow "IP4 Packet"
              <> makeDataRow ["vers: " <> (pprint m $ x ^. i4Vers)
                             ,"ihl: "  <> (pprint m $ x ^. i4Ihl)
                             ,"tos: "  <> (pprint m $ x ^. i4Tos)
                             ,"tl: "   <> (pprint m $ x ^. i4Tl)
                             ,"id: "   <> (pprint m $ x ^. i4Iden)]
              <> pprint m (x ^. i4Flags)
              <> makeDataRow ["off: "      <> (pprint m $ x ^. i4Off)
                             ,"ttl: "      <> (pprint m $ x ^. i4Ttl)
                             ,"protocol: " <> (pprint m $ x ^. i4Proto)
                             ,"checksum: " <> (pprint m $ x ^. i4Checksum)
                             ,"src: " <> (pprint m $ x ^. i4Src)
                             ,"dst: " <> (pprint m $ x ^. i4Dst)]
              <> (T.concat $ map (pprint m) (x ^. i4Opts))
              <> dashRow  

instance PrettyPrint ICMPHeader where
    pprint m x = gPrettyPrint' "ICMP Header" m x

instance PrettyPrint DestUnreachable where
    pprint m x = gPrettyPrint' "Destination Unreachable" m x 

instance PrettyPrint SourceQuench where
    pprint m x = gPrettyPrint' "Source Quench" m x 

instance PrettyPrint TimeExceeded where
    pprint m x = gPrettyPrint' "Time Exceeded" m x 

instance PrettyPrint Redirect where
    pprint m x = gPrettyPrint'"Redirect" m x

instance PrettyPrint ParamProblem where
    pprint m x = gPrettyPrint' "Paramater Problem" m x 

instance PrettyPrint EchoRequest where
    pprint m x = gPrettyPrint' "Echo Request" m x 

instance PrettyPrint EchoReply where
    pprint m x = gPrettyPrint' "Echo Reply" m x

instance PrettyPrint TimeStampRequest where
    pprint m x = gPrettyPrint' "TimeStamp Request" m x 

instance PrettyPrint TimeStampReply where
    pprint m x = gPrettyPrint' "TimeStamp Reply" m x 

instance PrettyPrint RouterAdvertisement where
    pprint m x = gPrettyPrint' "Router Advertisement" m x

instance PrettyPrint RouterSolicitation where
    pprint m x = gPrettyPrint' "Router Solicitation" m x

instance PrettyPrint TraceRoute  where
    pprint m x = gPrettyPrint' "Trace Route" m x

instance PrettyPrint AddressMaskRequest where
    pprint m x = gPrettyPrint' "Address Mask Request" m x

instance PrettyPrint AddressMaskReply where
    pprint m x = gPrettyPrint' "Address Mask Reply" m x

instance PrettyPrint ICMPData where 
    pprint m y =  case y of
                   DU x -> pprint m x 
                   SQ x -> pprint m x 
                   TE x -> pprint m x 
                   RD x -> pprint m x 
                   PP x -> pprint m x 
                   ERQ x -> pprint m x 
                   ERP x -> pprint m x 
                   TSRQ x -> pprint m x 
                   TSRP x -> pprint m x 
                   RA   x -> pprint m x 
                   RS   x -> pprint m x 
                   TR   x -> pprint m x 
                   AMRQ x -> pprint m x 
                   AMRP x -> pprint m x 

instance PrettyPrint ICMPMessage where
    pprint m x =  makeLabelRow "ICMP Message"
               <> pprint m (x ^. icmpHdr)
               <> pprint m (x ^. icmpData)

instance PrettyPrint UDPMessage where
    pprint m x = gPrettyPrint "UDP Message" m x 

instance PrettyPrint TCPOption where
    pprint m x = gPrettyPrint' "TCP Option" m x 

instance PrettyPrint ControlFlags where
    pprint m x = gPrettyPrint' "TCP Option" m x 

instance PrettyPrint TCPSegment  where
    pprint m x = makeLabelRow "TCP Segment"
               <> makeDataRow ["src: "    <> (pprint m $ x ^. tSrc)
                              ,"dst: "    <> (pprint m $ x ^. tDst)
                              ,"seqNum: " <> (pprint m $ x ^. tSeqNum)
                              ,"ackNum: " <> (pprint m $ x ^. tAckNum)
                              ,"offset: " <> (pprint m $ x ^. tOffset)]
               <> pprint m (x ^. tFlags)
               <> makeDataRow ["win: "       <> (pprint m $ x ^. tWin)
                              ,"checksum: "  <> (pprint m $ x ^. tChecksum)
                              ,"urgPtr: "    <> (pprint m $ x ^. tUrgPntr)]
               <> (T.concat $ map (pprint m) (x ^. tOpts))

instance PrettyPrint DNSHeader where
    pprint m x = gPrettyPrint' "DNS Header" m x

instance PrettyPrint DNSQuestion where
    pprint m x = gPrettyPrint' "DNS Question" m x

instance PrettyPrint RR_SOA where
    pprint m x = gPrettyPrint' "DNS RR SOA" m x

instance PrettyPrint RR_MX where
    pprint m x = gPrettyPrint' "DNS RR MX" m x

instance PrettyPrint RR_TXT where
    pprint m x = gPrettyPrint' "DNS RR TXT" m x

instance PrettyPrint RR_NS where
    pprint m x = gPrettyPrint' "DNS RR NS" m x

instance PrettyPrint RR_CNAME where
    pprint m x = gPrettyPrint' "DNS RR CName" m x 

instance PrettyPrint RR_PTR where 
    pprint m x = gPrettyPrint' "DNS RR PTR" m x 

instance PrettyPrint RData where
    pprint m x = case x of
        A y     ->   makeLabelRowDotted "DNS RR A"
                  <> pprint m y 
        NS y    -> pprint m y 
        CName y -> pprint m y 
        SOA y   -> pprint m y
        PTR y   -> pprint m y 
        MX y    -> pprint m y 
        TXT y   -> pprint m y 

instance PrettyPrint DNSRR where
    pprint m x =  gPrettyPrint' "DNS Resource Record" m x 

instance PrettyPrint DNSAnswer where
    pprint m y =  makeLabelRowDotted "DNS Answer"
               <> pprint m y 

instance PrettyPrint DNSAuth where
    pprint m y =  makeLabelRowDotted "DNS Auth"
               <> pprint m y 

instance PrettyPrint DNSAdd where
    pprint m y =  makeLabelRowDotted "DNS Auth"
                   <> pprint m y 

instance PrettyPrint a => PrettyPrint [a] where
    pprint m ys = T.concat $ map (pprint m) ys

-------------FINISH DNS LATER THIS IS TOO TEDIOUS

instance PrettyPrint DNSMessage where
    pprint m x = makeLabelRow "DNS Message"
               <> pprint m (x ^. dnsHdr)
               <> pprint m (x ^. dnsQuestion)
               <> pprint m (x ^. dnsAnswer)
               <> pprint m (x ^. dnsAuth)
               <> pprint m (x ^. dnsAdd)


instance PrettyPrint ProtocolMessage where
    pprint m x = case x of
        ETHm y  -> pprint m y
        ARPm y  -> pprint m y 
        ICMPm y -> pprint m y 
        TCPm y  -> pprint m y 
        UDPm y  -> pprint m y  
        DNSm y  -> pprint m y  
        IP4m y  -> pprint m y
        CONTENTm (MessageContent y) -> makeLabelRow "Message Content"
                                     <> (makeDataRow [pprint m y])
                                     <> dashRow 

