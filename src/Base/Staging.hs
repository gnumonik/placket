{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeApplications          #-}

module Staging  where

import FieldClasses 
import PrimTypes 
import           Classes
import           Control.Lens
import           Data.Proxy
import PrimTypes 
import           Data.Typeable    (Typeable)
import           DNS              ()
import           Ethernet         ()
import           GenericFunctions
import           ICMP4            ()
import           IP4              ()
import           PrimTypes
import           TCP              ()
import           TH2
import TH 
import           THAlias          ()
import           THWrappers 
import           UDP              ()
import           Generics.SOP.TH
import qualified Data.Text as T
import Language.Haskell.TH 
import qualified Data.ByteString as BS
import Control.Monad



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


aliases
defaults
isNetworkProtocol
mkProtocolMessageSum
mkPossiblyInstances 
stringyLens 
optionalFields 

makeClassyPrisms ''ProtocolMessage
deriveGeneric    ''ProtocolMessage



--HAS TO COME AFTER THE 'mkProtocol' SPLICES!!!
--mkNetworkProtocols



