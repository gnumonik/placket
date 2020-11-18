{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses, OverloadedStrings   #-}

module NextProtocol where

import           Aliases
import           Classes            
import           Control.Lens       (Lens', firstOf, set', (^.), _Just)
import           Control.Lens.TH    (makeFieldsNoPrefix)
import           Control.Monad      (join)
import qualified Data.ByteString    as BS
import qualified Data.Map.Strict           as Map
import qualified Data.Text          as T 
import           Data.Proxy
import           Data.Serialize     (Serialize)
import           Data.Serialize     (get)
import           Data.Serialize.Get 
import           Data.Typeable      (Proxy, Typeable)
import           LibTypes           (ProtocolMessage)
import           PrimTypes
import           THWrappers        
import           Wrappers           ()




nextProtocolCont :: forall a. (NextProtocol a, WrapProtocol a) 
                 => Proxy a 
                 -> (BS.ByteString 
                    -> (ProtocolMessage, Maybe (BS.ByteString -> ProtocolMessage) ))
nextProtocolCont _ = undefined 

data Direction = IN | OUT deriving (Show, Eq)

data NEXT a = NEXT {_gNext :: BS.ByteString -> Either T.Text ProtocolMessage
                   ,_sNext :: a -> a} deriving Typeable

class WrapProtocol a => NextProtocol a where
    nextPID ::  Maybe (NEXTID a)
    nextP   :: (Proxy a) -> Maybe (Map.Map Integer (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString)))

data NEXTID a = NEXTID {_getID :: a -> Integer
                       ,_setID :: Integer -> a -> a} deriving Typeable
makeFieldsNoPrefix ''NEXTID

mEither :: Either a b  -> Maybe b
mEither x = case x of
    Right y -> Just y
    Left _  -> Nothing

getNext :: forall a. NextProtocol a => a -> Maybe (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString))
getNext !msg = case join $ pure ($ msg) <*> f >>= (\x -> Map.lookup x <$> nextMap) of
        Just !g  -> Just g 
        Nothing -> Nothing
   where
        !f  = firstOf (_Just . getID) nextPID
        !nextMap = nextP (Proxy :: Proxy a)

nextID :: Integral b => Lens' s b -> NEXTID s
nextID aLens = NEXTID (\x -> toInteger $  x ^. aLens) (set' aLens . fromIntegral)

isEphemeral :: (Ord a, Num a) => a -> Bool
isEphemeral x =  x >= 32768 && x <= 60999 -- ephemeral ports for linux

isNext :: forall b. (HasAlias (FromAlias b) b, Serialize (FromAlias b), IsNetworkProtocol (FromAlias b), WrapProtocol (FromAlias b)) => b -> (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString))
isNext _  bs = case runGetPartial (get :: Get (FromAlias b)) bs of
    Fail _ _     -> Left "Error: Unable to parse bytestring."
    Partial !_        -> Left "Error: Unable to parse bytestring."
    Done !res !moreBS -> Right $! (wrapP res, moreBS)


instance NextProtocol MessageContent where
    nextPID = Nothing
    nextP _ = Nothing 

instance NextProtocol EthernetFrame where
    nextPID =  Just   $ nextID eEtherType
    nextP _     = Just $ Map.fromList $
        [
             (2048, isNext IP4)
            ,(2054, isNext ARP)
        ]

instance NextProtocol IP4Packet where
    nextPID =  Just $ nextID i4Proto
    nextP  _    = Just $ Map.fromList $
        [
            (1, isNext ICMP)
           ,(6, isNext TCP)
           ,(17, isNext UDP)
        ]

instance NextProtocol TCPSegment where
    -- bleh this depends on what the src/dest are, so does udp. need to add a direction paramer and change the class. will figure out later.
    nextPID =  Just $ nextID tSrc

    nextP  _    = Just $ Map.fromList $
        [
            (53, isNext DNS)
        ]

instance NextProtocol UDPMessage where
    -- This is kind of a hack
    nextPID    = Just $ nextID uSrc
    nextP  _    = Just $ Map.fromList $
        [
            (53, isNext DNS)
        ]

instance NextProtocol ARPMessage where
    nextPID = Nothing
    nextP _  = Nothing

instance NextProtocol ICMPMessage where
    nextPID = Nothing
    nextP _ = Nothing

instance NextProtocol DNSMessage where
    nextPID = Nothing
    nextP _  = Nothing
