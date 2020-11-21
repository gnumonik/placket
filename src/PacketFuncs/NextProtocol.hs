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

import           Staging
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
import           PrimTypes
import THWrappers ( Possibly(fromA))        
import           Staging
import Control.Lens.Getter (view)




data Direction = IN | OUT deriving (Show, Eq)

data NEXT a = NEXT {_gNext :: BS.ByteString -> Either T.Text ProtocolMessage
                   ,_sNext :: a -> a} deriving Typeable

class NextProtocol a where
    nextPID ::  Maybe (NEXTID a)
    nextP   ::  Proxy a -> Maybe (Map.Map Integer (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString)))

data NEXTID a = NEXTID {_getID_IN :: a -> Integer
                       ,_getID_OUT :: a -> Integer
                       ,_setID_IN :: Integer -> a -> a
                       ,_setID_OUT :: Integer -> a -> a} deriving Typeable
makeFieldsNoPrefix ''NEXTID

mEither :: Either a b  -> Maybe b
mEither x = case x of
    Right y -> Just y
    Left _  -> Nothing

getNext :: forall a. NextProtocol a => a -> Maybe (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString))
getNext !msg = case join $ pure ($ msg) <*> f >>= (\x -> Map.lookup x <$> nextMap) of
        Just !g  -> Just g 
        Nothing -> case join $ pure ($ msg) <*> f' >>= (\x -> Map.lookup x <$> nextMap) of
            Just !g' -> Just g'
            Nothing  -> Nothing 
   where
        !f  = firstOf (_Just . getID_IN) nextPID
        !f' = firstOf (_Just . getID_OUT) nextPID
        !nextMap = nextP (Proxy :: Proxy a)

nextID :: Integral b => Lens' s b -> NEXTID s
nextID aLens = NEXTID (\x -> toInteger $  x ^. aLens) (\x -> toInteger $  x ^. aLens) (set' aLens . fromIntegral) (set' aLens . fromIntegral)

isEphemeral :: (Ord a, Num a) => a -> Bool
isEphemeral x =  x >= 32768 && x <= 60999 -- ephemeral ports for linux

isNext :: forall a . (Possibly a ProtocolMessage, Serialize a)  =>  (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString))
isNext  bs = case runGetPartial (get @a) bs of
    Fail _ _     -> Left "Error: Unable to parse bytestring."
    Partial !_        -> Left "Error: Unable to parse bytestring."
    Done !res !moreBS -> Right $! (fromA res, moreBS)


instance NextProtocol MessageContent where
    nextPID = Nothing
    nextP _ = Nothing 

instance NextProtocol EthernetFrame where
    nextPID =  Just   $ nextID eEtherType
    nextP _     = Just $ Map.fromList $
        [
             (2048, isNext @IP4Packet)
            ,(2054, isNext @ARPMessage)
        ]

instance NextProtocol IP4Packet where
    nextPID =  Just $ nextID i4Proto
    nextP  _    = Just $ Map.fromList $
        [
            (1, isNext  @ICMPMessage)
           ,(6, isNext  @TCPSegment)
           ,(17, isNext @UDPMessage)
        ]

instance NextProtocol TCPSegment where
    -- bleh this depends on what the src/dest are, so does udp. need to add a direction paramer and change the class. will figure out later.
    nextPID =  Just $ NEXTID {_getID_IN  = toInteger . view tSrc 
                             ,_getID_OUT = toInteger . view tDst 
                             ,_setID_IN  = set' tSrc . fromIntegral 
                             ,_setID_OUT = set' tDst . fromIntegral}  

    nextP  _    = Just $ Map.fromList $
        [
            (53, isNext @DNSMessage)
        ]

instance NextProtocol UDPMessage where
    -- This is kind of a hack
    nextPID    = Just $ NEXTID {_getID_IN  = toInteger . view uSrc 
                               ,_getID_OUT = toInteger . view uDst 
                               ,_setID_IN  = set' uSrc . fromIntegral 
                               ,_setID_OUT = set' uDst . fromIntegral}  
    nextP  _    = Just $ Map.fromList $
        [
            (53, isNext @DNSMessage)
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
