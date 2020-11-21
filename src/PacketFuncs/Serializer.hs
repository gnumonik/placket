{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
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
{-# LANGUAGE UndecidableSuperClasses, BangPatterns, OverloadedStrings   #-}

--TCP OPTIONS PARSER & PUTTER AREN"T IDENTICAL, FUCKS SHIT UP

module Serializer where

import           Classes                          
import qualified Data.ByteString                  as BS
import           Data.Proxy                       (Proxy (..))
import           Data.Serialize                   

import           Generics.SOP
import           FieldClasses     
import           Staging                     
import           NextProtocol
import qualified Data.Vector as V
import qualified Data.Text as T
import THWrappers ( Possibly(fromA) )                       


genericGetNext :: 
               forall a. (Generic a, AllN SOP NextProtocol (Code a), a ~ ProtocolMessage)
               => a 
               -> Maybe (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString))
genericGetNext pmsg = goSOP (from pmsg)
    where
        goSOP :: forall xss. AllN SOP NextProtocol xss 
              => SOP I xss 
              -> Maybe (BS.ByteString -> Either T.Text (ProtocolMessage,BS.ByteString))
        goSOP (SOP (Z xs)) = case (hcollapse $ gNext xs) of

            [x] -> x

            _   -> error "Impossible ProtocolMessage!"

        goSOP (SOP (S xs)) = goSOP (SOP xs)

        gNext :: forall z. All NextProtocol z 
              => NP I z 
              -> NP (K (Maybe (BS.ByteString 
                    -> Either T.Text (ProtocolMessage,BS.ByteString)))) z
        gNext !xss = hcmap (Proxy :: Proxy NextProtocol) (mapIK getNext) xss 
{-# INLINE genericGetNext #-}

incrementalDeserialize :: 
                forall a. (Possibly a ProtocolMessage, NextProtocol a, Serialize a) 
                       => Proxy a
                       -> BS.ByteString 
                       -> V.Vector ProtocolMessage
                       -> Either T.Text (V.Vector ProtocolMessage)
incrementalDeserialize _ !bstr !acc = case runGetPartial (get @a) bstr of
    Fail !str _                -> Left . T.pack $ str
    Partial _                  -> Left $ "Error: Failed to deserialize."
    Done !result !remainingBS  -> 
        deserializeNext (fromA result) (V.force $! V.cons (fromA result) acc) remainingBS
{-# INLINE incrementalDeserialize #-}

deserializeNext :: ProtocolMessage 
    -> V.Vector ProtocolMessage 
    -> BS.ByteString 
    -> Either T.Text (V.Vector ProtocolMessage)
deserializeNext !oldResult !acc' !inputBS 
    = case genericGetNext oldResult of

        Just f  -> case f inputBS of

            Right (pmsg,moreBS) -> 
                deserializeNext pmsg (V.force $! V.cons pmsg acc') moreBS

            Left someErr -> Left someErr 

        Nothing ->  Right $! V.force $! V.cons (fromA $ MessageContent inputBS) acc'  
{-# INLINE deserializeNext #-}

gSerialize :: forall a. (Generic a, AllN SOP Serialize (Code a), a ~ ProtocolMessage) => a ->  BS.ByteString
gSerialize msg = goSOP (from msg)
   where
       goSOP :: AllN SOP Serialize xss => SOP I xss -> BS.ByteString
       goSOP (SOP (Z xs)) = case hcollapse $! hcmap (Proxy :: Proxy Serialize) (mapIK myPut) xs of
           ![x] ->  x
           _   ->  error "Impossible protocolmessage! (Should never trigger.)"
       goSOP (SOP (S xs)) = goSOP (SOP xs)

       myPut :: Serialize x => x -> BS.ByteString
       myPut x = runPut (put x)
{-# INLINE gSerialize #-}

serializeMessage :: Builder ProtocolMessage -> BS.ByteString
serializeMessage !bld = V.foldr (\x y -> gSerialize x <> y) BS.empty (V.reverse bld )
{-# INLINE serializeMessage #-}

proxy :: forall a. a -> Proxy a
proxy _ = Proxy @a

