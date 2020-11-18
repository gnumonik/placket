{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Aliases  where

import           Classes
import           Control.Lens
import           Data.Proxy
import           Data.Typeable    (Typeable)
import           DNS              ()
import           Ethernet         ()
import           GenericFunctions
import           ICMP4            ()
import           IP4              ()
import           PrimTypes
import           TCP              ()
import           TH2              (mkProtocol)
import           THAlias          ()
import           UDP              ()


data ETH = ETH deriving (Show, Eq, Typeable)
instance Alias EthernetFrame ETH

data ARP = ARP deriving (Show, Eq, Typeable) 
instance Alias ARPMessage ARP

data IP4 = IP4 deriving (Show, Eq, Typeable)
instance Alias IP4Packet IP4

data ICMP = ICMP deriving (Show, Eq, Typeable)
instance Alias ICMPMessage ICMP

data UDP = UDP deriving (Show, Eq, Typeable)
instance Alias UDPMessage UDP

data TCP = TCP deriving (Show, Eq, Typeable)
instance Alias TCPSegment TCP

data DNS = DNS deriving (Show, Eq, Typeable)
instance Alias DNSMessage DNS

data CONTENT = CONTENT deriving (Show, Eq, Typeable)
instance Alias MessageContent CONTENT 

mkProtocol ''MessageContent 
mkProtocol ''EthernetFrame
mkProtocol ''ARPMessage
mkProtocol ''IP4Packet
mkProtocol ''TCPSegment
mkProtocol ''UDPMessage
mkProtocol ''ICMPMessage
mkProtocol ''DNSMessage

